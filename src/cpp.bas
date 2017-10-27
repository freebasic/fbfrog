''
'' C pre-processor
''
'' cppMain() goes through the token buffer much like a C preprocessor would do,
'' parsing CPP directives keeping track of #defines and #undefs, doing macro
'' expansion, evaluating #if blocks, and expanding #includes.
''
'' All tokens that shouldn't be preserved for the C parser later are marked via
'' tkSetRemove() (for tkApplyRemoves() later). This affects most directives and
'' all tokens skipped in #if 0 blocks. As a special case, #defines and
'' unexpanded #includes are not deleted, but preserved for the C parser, because
'' we want to parse them there too.
''
'' Since directives are not deleted from the token buffer immediately, we can
'' leave #define bodies in place, and copy the tokens from there whenever they
'' are needed for a macro expansion. Otherwise it would be necessary to load the
'' tokens into some AST.
''
'' In various places (especially during macro expansion), we're temporarily
'' inserting helper tokens such as TK_ARGBEGIN/TK_ARGEND to enclose a range of
'' tokens. Then, instead of having to keep track of token indices which change
'' on every insertion/deletion, we just have to watch out for those specific
'' helper tokens to detect begin/end of the range.
''
'' We insert TK_ENDINCLUDE behind tokens inserted due to #include expansion.
'' This allows detecting #include EOF for the #if/#include stack.
''
'' #includes statements are preserved as-is and the #included content is
'' inserted behind them (if the file could be found). The point is to let the
'' CPP and C parser see as much code as possible, making the translation as
'' accurate as possible, but also to preserve the #include statements. Later it
'' will all be taken apart and distributed into .bi files as specified by the
'' -emit options, and the #include statements will be removed then, if the
'' #include content will be kept instead.
''

#include once "cpp.bi"
#include once "lex.bi"
#include once "fbfrog.bi"
#include once "util-path.bi"

#include once "crt.bi"
#include once "file.bi"

using tktokens

declare function hMaybeExpandMacro(byval x as integer, byval inside_ifexpr as integer, byval expand_recursively as integer) as integer

''
'' Check whether the number literal token (TK_NUMBER) is a valid number literal,
'' and build a CONSTI/CONSTF ASTNODE representing its value (the literal saved
'' as-is in textual form, without type suffixes) and data type.
''
'' As a special case, in CPP expressions, number literals are always treated as
'' 64bit signed or unsigned, no matter what dtype was given as suffix and
'' ignoring the "int" default.
''
'' These are covered:
''    123        (decimal)
''    .123       (decimal float)
''    123.123    (decimal float)
''    0xAABBCCDD (hexadecimal)
''    010        (octal)
''    010.0      (decimal float, not octal float)
''    0b10101    (binary; non-standard but supported by gcc/clang)
''    0          (we treat this as decimal, even though it's octal,
''               for prettier FB code: 0 vs &o0)
'' There also is some simple float exponent and type suffix parsing.
''
'' We have to parse the number literal (but without type suffix) first before we
'' can decide whether it's an integer or float. This decides whether a leading
'' zero indicates octal or not.
''
function hNumberLiteral _
	( _
		byval x as integer, _
		byval is_cpp as integer, _
		byref errmsg as string, _
		byval clong32 as integer _
	) as ASTNODE ptr

	assert(tkGet(x) = TK_NUMBER)
	dim as ubyte ptr p = tkGetText(x)

	var numbase = 10
	var is_float = FALSE
	var have_nonoct_digit = FALSE

	'' Check for non-decimal prefixes:
	'' 0, 0x, 0X, 0b, 0B
	if p[0] = CH_0 then '' 0
		select case p[1]
		case CH_L_B, CH_B  '' 0b, 0B
			p += 2
			numbase = 2
		case CH_L_X, CH_X  '' 0x, 0X
			p += 2
			numbase = 16
		case CH_0 to CH_9
			p += 1
			numbase = 8
		end select
	end if

	'' Body (integer part + fractional part, if any)
	var begin = p

	select case numbase
	case 2
		while (p[0] = CH_0) or (p[0] = CH_1)
			p += 1
		wend

	case 16
		do
			select case as const p[0]
			case CH_0 to CH_9, CH_A to CH_F, CH_L_A to CH_L_F
			case else
				exit do
			end select
			p += 1
		loop

	case else
		do
			select case as const p[0]
			case CH_0 to CH_7
				'' These digits are allowed in both dec/oct literals
			case CH_8, CH_9
				'' These digits are only allowed in dec literals, not
				'' oct, but we don't know which it is yet.
				have_nonoct_digit = TRUE
			case CH_DOT
				'' Only one dot allowed
				if is_float then exit do
				is_float = TRUE
			case else
				exit do
			end select
			p += 1
		loop
	end select

	'' Exponent? (can be used even without fractional part, e.g. '1e1')
	select case p[0]
	case CH_E, CH_L_E   '' 'E', 'e'
		is_float = TRUE
		p += 1

		'' ['+' | '-']
		select case p[0]
		case CH_PLUS, CH_MINUS
			p += 1
		end select

		'' ['0'-'9']*
		while (p[0] >= CH_0) and (p[0] <= CH_9)
			p += 1
		wend
	end select

	'' Floats with leading zeroes are decimal, not octal.
	if is_float and (numbase = 8) then
		numbase = 10
	end if

	if have_nonoct_digit and (numbase = 8) then
		errmsg = "invalid digit in octal number literal"
		exit function
	end if

	'' Save the number literal body (we don't want to include type suffixes here)
	var old = p[0]
	p[0] = 0  '' temporary null terminator
	var n = astNew(ASTCLASS_CONSTI, cptr(zstring ptr, begin))
	p[0] = old

	'' Float type suffixes
	select case p[0]
	case CH_F, CH_L_F    '' 'F' | 'f'
		p += 1
		is_float = TRUE
		n->dtype = TYPE_SINGLE
	case CH_D, CH_L_D    '' 'D' | 'd'
		p += 1
		is_float = TRUE
		n->dtype = TYPE_DOUBLE
	end select

	if is_float then
		n->class = ASTCLASS_CONSTF
		'' No float type suffix? Then default to double.
		if n->dtype = TYPE_NONE then
			n->dtype = TYPE_DOUBLE
		end if
	else
		'' Integer type suffixes:
		''  l, ll, ul, ull, lu, llu
		'' MSVC-specific ones:
		''  [u]i{8|16|32|64}
		'' All letters can also be upper-case.
		var have_u = FALSE
		var have_l = FALSE
		var have_ll = FALSE

		select case p[0]
		case CH_U, CH_L_U       '' u
			p += 1
			have_u = TRUE
		end select

		select case p[0]
		case CH_L, CH_L_L       '' l, [u]l
			p += 1
			select case p[0]
			case CH_L, CH_L_L       '' l, [u]ll
				p += 1
				have_ll = TRUE
			case else
				have_l = TRUE
			end select

			if have_u = FALSE then
				select case p[0]
				case CH_U, CH_L_U       '' u, llu
					p += 1
					have_u = TRUE
				end select
			end if

		case CH_I, CH_L_I       '' i, [u]i
			select case p[1]
			case CH_8 : p += 2 '' i8
			case CH_1 : if p[2] = CH_6 then : p += 3 : end if  '' i16
			case CH_3 : if p[2] = CH_2 then : p += 3 : end if  '' i32
			case CH_6 : if p[2] = CH_4 then : p += 3 : have_ll = TRUE : end if  '' i64
			end select
		end select

		'' In CPP mode, all integer literals are 64bit, the 'l' suffix is ignored
		if is_cpp then
			n->dtype = iif(have_u, TYPE_ULONGINT, TYPE_LONGINT)
		'' In C mode, integer literals default to 'int', and suffixes are respected
		elseif have_ll then
			n->dtype = iif(have_u, TYPE_ULONGINT, TYPE_LONGINT)
		elseif have_l then
			n->dtype = typeGetCLong(have_u, clong32)
		else
			n->dtype = iif(have_u, TYPE_ULONG, TYPE_LONG)
		end if

		select case numbase
		case 16 : n->attrib or= ASTATTRIB_HEX
		case  8 : n->attrib or= ASTATTRIB_OCT
		case  2 : n->attrib or= ASTATTRIB_BIN
		end select
	end if

	'' Show error if we didn't reach the end of the number literal
	if p[0] <> 0 then
		errmsg = "invalid suffix on number literal: '" + *cptr(zstring ptr, p) + "'"
		astDelete(n)
		exit function
	end if

	function = n
end function

private function hReadEscapeSequence(byref p as ubyte ptr, byref errmsg as string) as ulongint
	select case p[0]
	case CH_DQUOTE    : p += 1 : function = CH_DQUOTE     '' \"
	case CH_QUOTE     : p += 1 : function = CH_QUOTE      '' \'
	case CH_QUEST     : p += 1 : function = CH_QUEST      '' \?
	case CH_BACKSLASH : p += 1 : function = CH_BACKSLASH  '' \\
	case CH_L_A       : p += 1 : function = CH_BELL       '' \a
	case CH_L_B       : p += 1 : function = CH_BACKSPACE  '' \b
	case CH_L_F       : p += 1 : function = CH_FORMFEED   '' \f
	case CH_L_N       : p += 1 : function = CH_LF         '' \n
	case CH_L_R       : p += 1 : function = CH_CR         '' \r
	case CH_L_T       : p += 1 : function = CH_TAB        '' \t
	case CH_L_V       : p += 1 : function = CH_VTAB       '' \v

	'' \NNN (octal, max 3 digits)
	case CH_0 to CH_7
		dim as uinteger value
		var length = 0
		do
			value = (value shl 3) + (p[0] - asc("0"))
			length += 1
			p += 1
		loop while (p[0] >= CH_0) and (p[0] <= CH_7) and (length < 3)

		function = value

	'' \xNNNN... (hexadecimal, as many digits as possible)
	case CH_L_X
		dim as ulongint value
		var length = 0
		p += 1
		do
			dim as uinteger digit = p[0]

			select case digit
			case CH_0 to CH_9
				digit -= CH_0
			case CH_A to CH_F
				digit -= (CH_A - 10)
			case CH_L_A to CH_L_F
				digit -= (CH_L_A - 10)
			case else
				exit do
			end select

			value = (value shl 4) + digit

			length += 1
			p += 1
		loop

		function = value

	case else
		errmsg = "unknown escape sequence"
	end select
end function

'' C string literal parsing
''   "..."
''   L"..."
''   '.'
''   L'.'
'' String literals can contain escape sequences
function hStringLiteral _
	( _
		byval x as integer, _
		byval eval_escapes as integer, _
		byref errmsg as string _
	) as ASTNODE ptr

	assert((tkGet(x) = TK_STRING) or (tkGet(x) = TK_WSTRING) or _
	       (tkGet(x) = TK_CHAR  ) or (tkGet(x) = TK_WCHAR  ))
	dim as ubyte ptr p = tkGetText(x)

	var dtype = TYPE_ZSTRING
	if p[0] = CH_L then
		dtype = TYPE_WSTRING
		p += 1
	end if

	var astclass = ASTCLASS_STRING
	var quotechar = p[0]
	if quotechar = CH_QUOTE then
		astclass = ASTCLASS_CHAR
		dtype = iif(dtype = TYPE_ZSTRING, TYPE_BYTE, TYPE_WCHAR_T)
	end if
	p += 1

	'' Evaluate escape sequences and store the string literal's text into a buffer
	const MAXTEXTLEN = 1 shl 12
	'' +2 extra room to allow for some "overflowing" to reduce the amount
	'' of checking needed, +1 for null terminator.
	static text as zstring * MAXTEXTLEN+2+1 = any
	var j = 0  '' current write position in text buffer

	do
		select case p[0]
		case quotechar
			exit do

		case 0
			assert(FALSE)

		case CH_BACKSLASH '' \
			if eval_escapes then
				p += 1

				var value = hReadEscapeSequence(p, errmsg)
				if len(errmsg) > 0 then
					exit function
				end if

				select case value
				case is > &hFFu
					errmsg = "escape sequence value bigger than " & &hFFu & " (&hFF): " & value & " (&h" & hex(value) & ")"
					exit function

				'' Encode embedded nulls as "\0", and then also backslashes
				'' as "\\" to prevent ambiguity with the backslash in "\0".
				'' This allows the string literal content to still be
				'' represented as null-terminated string.
				case 0
					text[j] = CH_BACKSLASH : j += 1
					text[j] = CH_0         : j += 1
				case CH_BACKSLASH
					text[j] = CH_BACKSLASH : j += 1
					text[j] = CH_BACKSLASH : j += 1

				case else
					text[j] = value : j += 1
				end select
			else
				'' Store backslash as-is, not resolving escape sequences
				text[j] = CH_BACKSLASH : j += 1
				p += 1

				select case p[0]
				case CH_BACKSLASH '' \\
					text[j] = CH_BACKSLASH : j += 1
					p += 1
				case quotechar    '' \" or \'
					text[j] = quotechar : j += 1
					p += 1
				end select
			end if

		case else
			text[j] = p[0] : j += 1
			p += 1
		end select

		if j > MAXTEXTLEN then
			errmsg = "string literal too long, MAXTEXTLEN=" & MAXTEXTLEN
			exit function
		end if
	loop

	'' closing quote
	p += 1

	'' null-terminator
	text[j] = 0

	var n = astNew(astclass, text)
	n->dtype = dtype
	function = n
end function

'' MacroParameter =
''      Identifier         (named parameter)
''    | Identifier '...'   (named + variadic)
''    | '...'              (variadic, using __VA_ARGS__)
'' ('...' can only appear on the last parameter)
private sub hMacroParam(byref x as integer, byval macro as ASTNODE ptr)
	'' Identifier?
	dim id as zstring ptr
	if tkGet(x) >= TK_ID then
		id = tkSpellId(x)
		x += 1
	end if

	'' Shouldn't have seen a '...' yet
	assert((macro->attrib and ASTATTRIB_VARIADIC) = 0)
	var maybevariadic = 0

	'' '...'?
	if tkGet(x) = TK_ELLIPSIS then
		x += 1
		maybevariadic = ASTATTRIB_VARIADIC
		if id = NULL then
			''    #define m(a, ...)
			'' must become
			''    #define m(a, __VA_ARGS__...)
			'' in FB, because in FB, the '...' parameter of variadic macros must always have a name,
			'' and using __VA_ARGS__ for that is the easiest solution, because then we don't need to
			'' go replacing that in the macro body.
			id = @"__VA_ARGS__"
		end if
	elseif id = NULL then
		tkOopsExpected(x, "macro parameter (identifier or '...')")
	end if

	var param = astNew(ASTCLASS_MACROPARAM, id)
	param->attrib or= maybevariadic
	astAppend(macro, param)
	macro->attrib or= maybevariadic
	macro->paramcount += 1
end sub

'' <no whitespace> '(' MacroParameters ')'
private sub hMacroParamList(byref x as integer, byval macro as ASTNODE ptr)
	assert(macro->paramcount = -1)

	'' '(' directly behind #define identifier, no spaces in between?
	if (tkGet(x) = TK_LPAREN) and ((tkGetFlags(x) and TKFLAG_BEHINDSPACE) = 0) then
		x += 1

		macro->paramcount = 0

		'' Not just '()'?
		if tkGet(x) <> TK_RPAREN then
			'' MacroParam (',' MacroParam)*
			do
				hMacroParam(x, macro)

				'' ','?
				if tkGet(x) <> TK_COMMA then
					exit do
				end if
				x += 1
			loop
		end if

		'' ')'?
		tkExpect(x, TK_RPAREN, "to close the parameter list in this macro declaration")
		x += 1
	end if
end sub

function hDefineHead(byref x as integer) as ASTNODE ptr
	'' Identifier? (keywords should be allowed to, so anything >= TK_ID)
	select case tkGet(x)
	case is < TK_ID
		tkExpect(x, TK_ID, "behind #define")
	case KW_DEFINED
		tkOops(x, "'defined' cannot be used as macro name")
	end select
	var macro = astNewPPDEFINE(tkSpellId(x))
	x += 1

	hMacroParamList(x, macro)

	function = macro
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

type DEFINEINFO
	xbody	as integer  '' 1st token of the #define body
	xeol	as integer  '' eol/eof token behind the #define body

	'' PPDEFINE node with information about the macro's parameters etc.
	macro	as ASTNODE ptr
end type

private function definfoNew() as DEFINEINFO ptr
	function = callocate(sizeof(DEFINEINFO))
end function

private sub definfoDelete(byval definfo as DEFINEINFO ptr)
	if definfo then
		astDelete(definfo->macro)
		deallocate(definfo)
	end if
end sub

private function definfoClone(byval a as DEFINEINFO ptr) as DEFINEINFO ptr
	var b = definfoNew()
	b->xbody   = a->xbody
	b->xeol    = a->xeol
	b->macro   = astClone(a->macro)
	function = b
end function

const DEFINEBODY_FLAGMASK = not (TKFLAG_REMOVE or TKFLAG_DIRECTIVE)

'' Copy a #define body into some other place
private sub definfoCopyBody(byval definfo as DEFINEINFO ptr, byval x as integer)
	assert(x > definfo->xeol)
	tkCopy(x, definfo->xbody, definfo->xeol - 1, DEFINEBODY_FLAGMASK)
end sub

'' Compare two #defines and determine whether they are equal
private function definfoDefinesAreEqual(byval a as DEFINEINFO ptr, byval b as DEFINEINFO ptr) as integer
	'' Check name and parameters
	if astIsEqual(a->macro, b->macro) = FALSE then
		exit function
	end if

	'' Check body
	if tkSpell(a->xbody, a->xeol) <> tkSpell(b->xbody, b->xeol) then
		exit function
	end if

	function = TRUE
end function

type SAVEDMACRO
	id		as zstring ptr		'' Macro's name

	'' A DEFINEINFO object if the macro was defined when being saved,
	'' or NULL if it was undefined.
	definfo		as DEFINEINFO ptr
end type

private sub savedmacroDtor(byval macro as SAVEDMACRO ptr)
	deallocate(macro->id)
	definfoDelete(macro->definfo)
end sub

enum
	'' If stack states:
	STATE_FILE = 0  '' file context (fresh toplevel/#include file, no #if yet)
	STATE_IF        '' #if context, fresh
	STATE_TRUE      '' #if context, saw #if/#elseif TRUE (and thus, further #elseif TRUE's must be skipped)
	STATE_ELSE      '' #if context, saw #else (and no further #elseif/#else can be allowed)
end enum

const MAXSTACK = 128

enum
	'' unknown = 0
	GUARDSTATE_CHECKING = 1
	GUARDSTATE_KNOWN
end enum

namespace cpp
	dim shared api as ApiInfo ptr
	dim shared as integer x  '' Current token index

	type STACKNODE
		state		as integer  '' STATE_*
		knownfile	as integer  '' Index into cpp.files array, if it's an #include context, or -1

		'' file contexts:
		'' A node from the incdirs list, representing the incdir where
		'' this file was found (for #include_next support)
		incdir		as ASTNODE ptr
	end type

	'' #if/file context stack
	'' * starts out with only the toplevel file context
	'' * #if blocks and #include contexts are put on this stack
	'' * an #endif found in an #include won't be able to close an #if from
	''   the parent file, since the #include stack node is in the way, and
	''   must be popped first.
	dim shared stack(0 to MAXSTACK-1) as STACKNODE
	dim shared as integer level  '' Current top of stack

	'' Stack level which caused #if 0 skipping
	'' * Allows us to continue parsing #ifs/#endifs even while skipping an
	''   #if 0 block, and this way, to determine which #endif ends skipping
	dim shared as integer skiplevel

	dim shared as integer filelevel

	'' Lookup table of macros known to be either defined or undefined.
	''   defined <=> data = a DEFINEINFO object
	'' undefined <=> data = NULL
	'' Even though unregistered symbols are implicitly undefined,
	'' registering them is useful to show the "assuming undefined" warning
	'' (and to only show it once).
	dim shared macros		as THash ptr

	'' Array of macros saved by #pragma push_macro, for use by #pragma
	'' pop_macro later.
	''  * push_macro saves the macro's current state - whether it's defined
	''    or not, and if it's defined, it's body/value.
	''  * push_macro even saves duplicate copies of the macro state: two
	''    equal push_macro's directly following each other create two stack
	''    entries, not one.
	''  * pop_macro restores the macro to the last saved state, overwriting
	''    the current value, #defining the macro again if it was #undef'ed
	''    in the meantime, or even #undef'ing it.
	''  * pop_macro does nothing if the stack for that macro is empty.
	'' Macros are appended to the array in the order they are saved. Popping
	'' a macro requires searching the array backwards for an entry for the
	'' given macro name. A hash table could be used instead, but that does
	'' not seem to be worth it.
	dim shared savedmacros		as SAVEDMACRO ptr
	dim shared savedmacrocount	as integer

	dim shared incdirs		as ASTNODE ptr

	type KNOWNFILE
		incfile as zstring ptr  '' Normalized file name
		guardstate	as integer
		guard		as zstring ptr  '' #include guard symbol, if any
		pragmaonce as integer  '' Whether #pragma once was found in this file
	end type

	'' Information about known files
	dim shared files as KNOWNFILE ptr
	dim shared filecount as integer
	dim shared filetb as THash ptr  '' data = index into files array
end namespace

sub cppInit(byref api as ApiInfo)
	cpp.api = @api
	cpp.x = 0

	'' Toplevel file context
	with cpp.stack(0)
		.state = STATE_FILE
		.knownfile = -1
		.incdir = NULL
	end with
	cpp.level = 0
	cpp.skiplevel = MAXSTACK  '' No skipping yet
	cpp.filelevel = 0

	cpp.macros = new THash(4, TRUE)
	cpp.savedmacros = NULL
	cpp.savedmacrocount = 0
	cpp.incdirs = astNewGROUP()

	cpp.files = NULL
	cpp.filecount = 0
	cpp.filetb = new THash(4, FALSE)
end sub

sub cppEnd()
	'' If anything is left on the stack at EOF, it can only be #ifs
	'' (#includes should be popped due to TK_ENDINCLUDE's already)
	if cpp.level > 0 then
		assert(cpp.stack(cpp.level).state >= STATE_IF)
		tkOops(cpp.x, "missing #endif")
	end if

	scope
		for i as integer = 0 to cpp.macros->room - 1
			definfoDelete(cpp.macros->items[i].data)
		next
		delete cpp.macros
	end scope
	scope
		for i as integer = 0 to cpp.savedmacrocount - 1
			savedmacroDtor(@cpp.savedmacros[i])
		next
		deallocate(cpp.savedmacros)
	end scope
	astDelete(cpp.incdirs)

	delete cpp.filetb
	scope
		for i as integer = 0 to cpp.filecount - 1
			with cpp.files[i]
				deallocate(.incfile)
				deallocate(.guard)
			end with
		next
		deallocate(cpp.files)
	end scope
end sub

#define cppSkipping() (cpp.skiplevel <> MAXSTACK)

sub cppAddPredefine(byval id as zstring ptr, byval body as zstring ptr)
	var s = "#define " + *id
	if body then
		s += " " + *body
	end if
	s += !"\n"
	var x = tkGetCount()
	lexLoadC(x, s, sourceinfoForZstring("pre-#define"))
	tkSetRemove(x, tkGetCount() - 1)
end sub

sub cppAddTargetPredefines(byval target as TargetInfo)
	cppAddPredefine(osinfo(target.os).fbdefine, "1")
	if archinfo(target.arch).is_64bit then cppAddPredefine("__FB_64BIT__", "1")
	if archinfo(target.arch).is_arm   then cppAddPredefine("__FB_ARM__"  , "1")
end sub

sub cppAddIncDir(byval incdir as zstring ptr)
	astAppend(cpp.incdirs, astNewTEXT(incdir))
end sub

sub cppAppendIncludeDirective(byval filename as zstring ptr, byval tkflags as integer)
	var code = "#include """ + *filename + """" + !"\n"
	var x = tkGetCount()
	lexLoadC(x, code, sourceinfoForZstring(code))
	tkAddFlags(x, tkGetCount() - 1, TKFLAG_REMOVE or tkflags)
end sub

private function cppLookupMacro(byval id as zstring ptr) as DEFINEINFO ptr
	function = cpp.macros->lookupDataOrNull(id)
end function

private function cppIsKnownSymbol(byval id as zstring ptr) as integer
	function = (cpp.macros->lookup(id, hashHash(id))->s <> NULL)
end function

private function cppIsMacroCurrentlyDefined(byval id as zstring ptr) as integer
	function = (cppLookupMacro(id) <> NULL)
end function

'' Add/overwrite a known macro definition (or register it as known undefined)
private sub cppAddMacro(byval id as zstring ptr, byval definfo as DEFINEINFO ptr)
	var hash = hashHash(id)
	var item = cpp.macros->lookup(id, hash)
	if item->s then
		definfoDelete(item->data)
		item->data = definfo
	else
		cpp.macros->add(item, hash, id, definfo)
	end if
end sub

private sub cppAddKnownUndefined(byval id as zstring ptr)
	cppAddMacro(id, NULL)
end sub

'' Append a new entry to the array of saved macros
private sub cppAppendSavedMacro(byval id as zstring ptr, byval definfo as DEFINEINFO ptr)
	cpp.savedmacrocount += 1
	cpp.savedmacros = reallocate(cpp.savedmacros, _
			cpp.savedmacrocount * sizeof(SAVEDMACRO))
	with cpp.savedmacros[cpp.savedmacrocount-1]
		.id = strDuplicate(id)
		.definfo = definfo
	end with
end sub

private sub cppRemoveSavedMacro(byval i as integer)
	assert((i >= 0) and (i < cpp.savedmacrocount))

	var p = cpp.savedmacros + i
	savedmacroDtor(p)
	cpp.savedmacrocount -= 1

	'' Remove array element from the middle of the array: move all elements
	'' behind it to the front, by 1 slot, to close the gap.
	var tail = cpp.savedmacrocount - i
	if tail > 0 then
		memmove(p, p + 1, tail * sizeof(SAVEDMACRO))
	end if
end sub

private function cppLookupSavedMacro(byval id as zstring ptr) as integer
	for i as integer = cpp.savedmacrocount - 1 to 0 step -1
		if *cpp.savedmacros[i].id = *id then
			return i
		end if
	next
	function = -1
end function

private sub cppSaveMacro(byval id as zstring ptr)
	'' Check the macro's current state.
	'' If it's defined, we need to duplicate the DEFINEINFO object;
	'' otherwise, if it's undefined, we use NULL.
	var definfo = cppLookupMacro(id)
	if definfo then
		definfo = definfoClone(definfo)
	end if

	cppAppendSavedMacro(id, definfo)
end sub

private sub cppRestoreMacro(byval id as zstring ptr)
	'' Search for the last saved macro for this id
	var i = cppLookupSavedMacro(id)
	if i < 0 then
		exit sub
	end if

	'' Restore the macro state
	var savedmacro = @cpp.savedmacros[i]
	if savedmacro->definfo then
		'' It was defined when saved, (re)-#define the macro
		cppAddMacro(id, savedmacro->definfo)
		savedmacro->definfo = NULL
	else
		'' It was undefined when saved, #undef the macro
		cppAddKnownUndefined(id)
	end if

	'' Remove the entry from the saved macros stack
	cppRemoveSavedMacro(i)
end sub

private function cppLookupOrAppendKnownFile _
	( _
		byval incfile as zstring ptr, _
		byref prettyfile as string _
	) as integer

	var hash = hashHash(incfile)
	var item = cpp.filetb->lookup(incfile, hash)
	if item->s then
		return cint(item->data)
	end if

	incfile = strDuplicate(incfile)

	var i = cpp.filecount
	cpp.filecount += 1
	cpp.files = reallocate(cpp.files, cpp.filecount * sizeof(*cpp.files))

	clear(cpp.files[i], 0, sizeof(*cpp.files))
	with cpp.files[i]
		.incfile = incfile
	end with

	cpp.filetb->add(item, hash, incfile, cptr(any ptr, i))
	function = i
end function

private sub cppEol()
	if tkGet(cpp.x) <> TK_EOL then
		tkOopsExpected(cpp.x, "end-of-line behind CPP directive")
	end if
	cpp.x += 1
end sub

private function cppStringLiteral(byval eval_escapes as integer) as string
	dim errmsg as string
	var s = hStringLiteral(cpp.x, eval_escapes, errmsg)
	if s = NULL then
		tkOops(cpp.x, errmsg)
	end if
	function = *s->text
	astDelete(s)
	cpp.x += 1
end function

private sub hCheckForUnknownSymbol(byval id as zstring ptr)
	if cppIsKnownSymbol(id) = FALSE then
		'' Unknown symbol; we're going to assume that it's undefined

		'' Show a warning if it seems to be useful; i.e. if it's not a reserved symbol,
		'' but one intended to be defined by the user.
		if frog.verbose then
			if strIsReservedIdInC(id) = FALSE then
				print "treating as undefined: " + *id
			end if
		end if

		'' Register as known undefined
		'' This also prevents the above warning from being shown
		'' multiple times for a single symbol.
		'' TODO: but only with-in a single CPP run, not globally; this should be fixed
		cppAddKnownUndefined(id)
	end if
end sub

'' 1st field: C operator precedence, starting at 1, higher value = higher precedence
'' 2nd field: is it right associative? (assignments/iif)
dim shared copinfo(ASTCLASS_CLOGOR to ASTCLASS_IIF) as COperatorInfo = _
{ _
	( 4, FALSE), _ '' ASTCLASS_CLOGOR
	( 5, FALSE), _ '' ASTCLASS_CLOGAND
	( 0, FALSE), _ '' ASTCLASS_LOGOR (unused)
	( 0, FALSE), _ '' ASTCLASS_LOGAND (unused)
	( 6, FALSE), _ '' ASTCLASS_OR
	( 7, FALSE), _ '' ASTCLASS_XOR
	( 8, FALSE), _ '' ASTCLASS_AND
	( 1, FALSE), _ '' ASTCLASS_CCOMMA
	( 2, TRUE ), _ '' ASTCLASS_CASSIGN
	( 2, TRUE ), _ '' ASTCLASS_CSELFOR
	( 2, TRUE ), _ '' ASTCLASS_CSELFXOR
	( 2, TRUE ), _ '' ASTCLASS_CSELFAND
	( 2, TRUE ), _ '' ASTCLASS_CSELFSHL
	( 2, TRUE ), _ '' ASTCLASS_CSELFSHR
	( 2, TRUE ), _ '' ASTCLASS_CSELFADD
	( 2, TRUE ), _ '' ASTCLASS_CSELFSUB
	( 2, TRUE ), _ '' ASTCLASS_CSELFMUL
	( 2, TRUE ), _ '' ASTCLASS_CSELFDIV
	( 2, TRUE ), _ '' ASTCLASS_CSELFMOD
	( 9, FALSE), _ '' ASTCLASS_CEQ
	( 9, FALSE), _ '' ASTCLASS_CNE
	(10, FALSE), _ '' ASTCLASS_CLT
	(10, FALSE), _ '' ASTCLASS_CLE
	(10, FALSE), _ '' ASTCLASS_CGT
	(10, FALSE), _ '' ASTCLASS_CGE
	( 0, FALSE), _ '' ASTCLASS_EQ (unused)
	( 0, FALSE), _ '' ASTCLASS_NE (unused)
	( 0, FALSE), _ '' ASTCLASS_LT (unused)
	( 0, FALSE), _ '' ASTCLASS_LE (unused)
	( 0, FALSE), _ '' ASTCLASS_GT (unused)
	( 0, FALSE), _ '' ASTCLASS_GE (unused)
	(11, FALSE), _ '' ASTCLASS_SHL
	(11, FALSE), _ '' ASTCLASS_SHR
	(12, FALSE), _ '' ASTCLASS_ADD
	(12, FALSE), _ '' ASTCLASS_SUB
	(13, FALSE), _ '' ASTCLASS_MUL
	(13, FALSE), _ '' ASTCLASS_DIV
	(13, FALSE), _ '' ASTCLASS_MOD
	(15, FALSE), _ '' ASTCLASS_INDEX
	(15, FALSE), _ '' ASTCLASS_MEMBER
	(15, FALSE), _ '' ASTCLASS_MEMBERDEREF
	( 0, FALSE), _ '' ASTCLASS_STRCAT (unused)
	(14, FALSE), _ '' ASTCLASS_CLOGNOT
	(14, FALSE), _ '' ASTCLASS_NOT
	(14, FALSE), _ '' ASTCLASS_NEGATE
	(14, FALSE), _ '' ASTCLASS_UNARYPLUS
	( 0, FALSE), _ '' ASTCLASS_CDEFINED (unused)
	( 0, FALSE), _ '' ASTCLASS_DEFINED (unused)
	(14, FALSE), _ '' ASTCLASS_ADDROF
	(14, FALSE), _ '' ASTCLASS_DEREF
	( 0, FALSE), _ '' ASTCLASS_STRINGIFY (unused)
	(14, FALSE), _ '' ASTCLASS_SIZEOF
	(14, FALSE), _ '' ASTCLASS_CAST
	( 3, TRUE )  _ '' ASTCLASS_IIF
}

type CPPVALUE
	vali as longint
	dtype as integer
end type

''
'' CPP expression parser and evaluator
''
'' - The expression parsing is based on a "precedence climbing" algorithm
''
'' - Operations are evaluated as intmax_t or uintmax_t, i.e. 64bit signed or
''   unsigned, as in gcc/clang
''
'' - For most unary/binary operations we need to check the operand in order to
''   determine the result dtype (signed -> unsigned promotion rules can affect
''   the result). Others (e.g. the logical and relational ones) always return
''   a signed int regardless of the operands' dtypes.
''
'' - &&, || and ?: operands must only be evaluated when actually reached, such
''   that we can ignore division by zero if it occurs on an irrelevant code
''   path. (same goes for our "assuming undefined" warnings though - they should
''   only be shown if it affects the outcome of the expression)
''
'' - The ?: ternary conditional operator is a special case: one of its operands
''   mustn't be evaluated, but we still need to determine its dtype to determine
''   the result dtype of the ?: operation. Because of this we have to
''   differentiate between "evaluation" and "dtype determination" modes.
''
'' - Taking care to produce C's 1|0 boolean values, instead of FB's -1|0
''
'' a =  -a
'' a =  a + b
'' a =  a ? b : c
''
'' a = operand for UOPs, lhs operand for BOPs, result value to return
'' b = rhs operand for BOPs
'' c = 3rd operand for ?: conditional
''
private sub cppExpression _
	( _
		byref a as CPPVALUE, _
		byval dtype_only as integer, _
		byval level as integer = 0 _
	)

	'' Unary prefix operators
	select case tkGet(cpp.x)
	case TK_EXCL  '' !
		cpp.x += 1

		'' operand
		cppExpression(a, dtype_only, cprecedence(ASTCLASS_CLOGNOT))

		a.vali = -(a.vali = 0)
		a.dtype = TYPE_LONGINT  '' ! operator always produces a signed int

	case TK_TILDE  '' ~
		cpp.x += 1

		'' operand
		cppExpression(a, dtype_only, cprecedence(ASTCLASS_NOT))

		a.vali = not a.vali

	case TK_MINUS  '' -
		cpp.x += 1

		'' operand
		cppExpression(a, dtype_only, cprecedence(ASTCLASS_NEGATE))

		a.vali = -a.vali

	case TK_PLUS  '' +
		cpp.x += 1

		'' operand
		cppExpression(a, dtype_only, cprecedence(ASTCLASS_UNARYPLUS))

	'' Atoms
	case TK_LPAREN  '' '(' Expression ')'
		'' '('
		cpp.x += 1

		'' Expression
		cppExpression(a, dtype_only)

		'' ')'
		tkExpect(cpp.x, TK_RPAREN, "for '(...)' parenthesized expression")
		cpp.x += 1

	case TK_NUMBER  '' Number literal
		dim errmsg as string
		var n = hNumberLiteral(cpp.x, TRUE, errmsg, cpp.api->clong32)
		if n = NULL then
			tkOops(cpp.x, errmsg)
		end if
		if n->class = ASTCLASS_CONSTF then
			tkOops(cpp.x, "float literal in CPP expression")
		end if

		assert((n->dtype = TYPE_LONGINT) or (n->dtype = TYPE_ULONGINT))
		a.vali = astEvalConstiAsInt64(n)
		a.dtype = n->dtype

		astDelete(n)

		cpp.x += 1

	'' Unexpanded identifier: treated as a literal 0
	case TK_ID
		if dtype_only = FALSE then
			hCheckForUnknownSymbol(tkSpellId(cpp.x))
		end if
		a.vali = 0
		a.dtype = TYPE_LONGINT

		cpp.x += 1

	'' DEFINED ['('] Identifier [')']
	case KW_DEFINED
		cpp.x += 1

		'' '('
		var have_parens = FALSE
		if tkGet(cpp.x) = TK_LPAREN then
			have_parens = TRUE
			cpp.x += 1
		end if

		'' Identifier
		if tkGet(cpp.x) < TK_ID then
			tkExpect(cpp.x, TK_ID, "as operand of DEFINED")
		end if
		if dtype_only = FALSE then
			var id = tkSpellId(cpp.x)
			hCheckForUnknownSymbol(id)
			'' defined()  ->  1|0
			a.vali = -cppIsMacroCurrentlyDefined(id)
		end if
		a.dtype = TYPE_LONGINT
		cpp.x += 1

		if have_parens then
			'' ')'
			tkExpect(cpp.x, TK_RPAREN, "for DEFINED(...)")
			cpp.x += 1
		end if

	case else
		tkOopsExpected(cpp.x, "expression")
	end select

	'' Infix operators
	do
		dim op as integer
		select case as const tkGet(cpp.x)
		case TK_QUEST    : op = ASTCLASS_IIF     '' ? (a ? b : c)
		case TK_PIPEPIPE : op = ASTCLASS_CLOGOR  '' ||
		case TK_AMPAMP   : op = ASTCLASS_CLOGAND '' &&
		case TK_PIPE     : op = ASTCLASS_OR      '' |
		case TK_CIRC     : op = ASTCLASS_XOR     '' ^
		case TK_AMP      : op = ASTCLASS_AND     '' &
		case TK_EQEQ     : op = ASTCLASS_CEQ     '' ==
		case TK_EXCLEQ   : op = ASTCLASS_CNE     '' !=
		case TK_LT       : op = ASTCLASS_CLT     '' <
		case TK_LTEQ     : op = ASTCLASS_CLE     '' <=
		case TK_GT       : op = ASTCLASS_CGT     '' >
		case TK_GTEQ     : op = ASTCLASS_CGE     '' >=
		case TK_LTLT     : op = ASTCLASS_SHL     '' <<
		case TK_GTGT     : op = ASTCLASS_SHR     '' >>
		case TK_PLUS     : op = ASTCLASS_ADD     '' +
		case TK_MINUS    : op = ASTCLASS_SUB     '' -
		case TK_STAR     : op = ASTCLASS_MUL     '' *
		case TK_SLASH    : op = ASTCLASS_DIV     '' /
		case TK_PERCENT  : op = ASTCLASS_MOD     '' %
		case else        : exit do
		end select

		'' Higher/same level means process now (takes precedence),
		'' lower level means we're done and the parent call will
		'' continue. The first call will start with level 0.
		var oplevel = cprecedence(op)
		if oplevel < level then
			exit do
		end if
		'' Left associative?
		if op <> ASTCLASS_IIF then
			oplevel += 1
		end if

		'' operator
		cpp.x += 1

		dim b as CPPVALUE

		select case op
		case ASTCLASS_CLOGOR  '' ||
			'' Parse rhs (don't evaluate if lhs was true)
			cppExpression(b, dtype_only or (a.vali <> 0), oplevel)
			a.vali = iif(a.vali, 1, iif(b.vali, 1, 0))
			a.dtype = TYPE_LONGINT  '' || always produces a signed int

		case ASTCLASS_CLOGAND  '' &&
			'' Parse rhs (don't evaluate if lhs was false)
			cppExpression(b, dtype_only or (a.vali = 0), oplevel)
			a.vali = iif(a.vali, iif(b.vali, 1, 0), 0)
			a.dtype = TYPE_LONGINT  '' && always produces a signed int

		case ASTCLASS_IIF
			'' Parse 2nd operand (don't evaluate if condition = false)
			cppExpression(b, dtype_only or (a.vali = 0), oplevel)

			'' ':'?
			tkExpect(cpp.x, TK_COLON, "for a?b:c iif operator")
			cpp.x += 1

			'' Parse 3rd operand (don't evaluate if condition = true)
			dim c as CPPVALUE
			cppExpression(c, dtype_only or (a.vali <> 0), oplevel)

			a.vali = iif(a.vali, b.vali, c.vali)
			a.dtype = max(b.dtype, c.dtype)

		case else
			'' Parse rhs
			cppExpression(b, dtype_only, oplevel)

			'' If one operand is unsigned, promote both operands to unsigned.
			'' This also takes care of the result type, except for relational BOPs,
			'' which are handled below.
			a.dtype = max(a.dtype, b.dtype)

			if dtype_only = FALSE then
				select case op
				case ASTCLASS_DIV, ASTCLASS_MOD
					if b.vali = 0 then
						tkOops(cpp.x, "division by zero")
					end if
				end select

				if a.dtype = TYPE_ULONGINT then
					select case as const op
					case ASTCLASS_OR  : a.vali =   cunsg(a.vali) or  cunsg(b.vali)
					case ASTCLASS_XOR : a.vali =   cunsg(a.vali) xor cunsg(b.vali)
					case ASTCLASS_AND : a.vali =   cunsg(a.vali) and cunsg(b.vali)
					case ASTCLASS_CEQ : a.vali = -(cunsg(a.vali) =   cunsg(b.vali))
					case ASTCLASS_CNE : a.vali = -(cunsg(a.vali) <>  cunsg(b.vali))
					case ASTCLASS_CLT : a.vali = -(cunsg(a.vali) <   cunsg(b.vali))
					case ASTCLASS_CLE : a.vali = -(cunsg(a.vali) <=  cunsg(b.vali))
					case ASTCLASS_CGT : a.vali = -(cunsg(a.vali) >   cunsg(b.vali))
					case ASTCLASS_CGE : a.vali = -(cunsg(a.vali) >=  cunsg(b.vali))
					case ASTCLASS_SHL : a.vali =   cunsg(a.vali) shl cunsg(b.vali)
					case ASTCLASS_SHR : a.vali =   cunsg(a.vali) shr cunsg(b.vali)
					case ASTCLASS_ADD : a.vali =   cunsg(a.vali) +   cunsg(b.vali)
					case ASTCLASS_SUB : a.vali =   cunsg(a.vali) -   cunsg(b.vali)
					case ASTCLASS_MUL : a.vali =   cunsg(a.vali) *   cunsg(b.vali)
					case ASTCLASS_DIV : a.vali =   cunsg(a.vali) \   cunsg(b.vali)
					case ASTCLASS_MOD : a.vali =   cunsg(a.vali) mod cunsg(b.vali)
					case else         : assert(FALSE)
					end select
				else
					select case as const op
					case ASTCLASS_OR  : a.vali =   a.vali or  b.vali
					case ASTCLASS_XOR : a.vali =   a.vali xor b.vali
					case ASTCLASS_AND : a.vali =   a.vali and b.vali
					case ASTCLASS_CEQ : a.vali = -(a.vali =   b.vali)
					case ASTCLASS_CNE : a.vali = -(a.vali <>  b.vali)
					case ASTCLASS_CLT : a.vali = -(a.vali <   b.vali)
					case ASTCLASS_CLE : a.vali = -(a.vali <=  b.vali)
					case ASTCLASS_CGT : a.vali = -(a.vali >   b.vali)
					case ASTCLASS_CGE : a.vali = -(a.vali >=  b.vali)
					case ASTCLASS_SHL : a.vali =   a.vali shl b.vali
					case ASTCLASS_SHR : a.vali =   a.vali shr b.vali
					case ASTCLASS_ADD : a.vali =   a.vali +   b.vali
					case ASTCLASS_SUB : a.vali =   a.vali -   b.vali
					case ASTCLASS_MUL : a.vali =   a.vali *   b.vali
					case ASTCLASS_DIV : a.vali =   a.vali \   b.vali
					case ASTCLASS_MOD : a.vali =   a.vali mod b.vali
					case else         : assert(FALSE)
					end select
				end if
			end if

			'' Relational BOPs always produce a signed int
			select case op
			case ASTCLASS_CEQ, ASTCLASS_CNE, _
			     ASTCLASS_CLT, ASTCLASS_CLE, _
			     ASTCLASS_CGT, ASTCLASS_CGE
				a.dtype = TYPE_LONGINT
			end select
		end select
	loop
end sub

private function hCheckForMacroCall(byval x as integer) as DEFINEINFO ptr
	assert(tkGet(x) >= TK_ID)
	var id = tkSpellId(x)

	'' Is this id a macro?
	var definfo = cppLookupMacro(id)
	if definfo = NULL then
		return NULL
	end if

	'' Only expand if not marked otherwise
	if cpp.api->idopt(OPT_NOEXPAND).matches(id) or _
	    (tkGetFlags(x) and TKFLAG_NOEXPAND) or _
	    (definfo->macro->attrib and ASTATTRIB_POISONED) then
		return NULL
	end if

	function = definfo
end function

const MAXARGS = 128

private sub hParseMacroCallArgs _
	( _
		byref x as integer, _
		byval macro as ASTNODE ptr, _
		byval argbegin as integer ptr, _
		byval argend as integer ptr, _
		byref argcount as integer _
	)

	'' Note: The macro call argument list must be parsed without doing
	'' macro expansion. Each argument individually must be expanded later,
	'' but not before the list has been parsed & split up into individual
	'' arguments. I.e. the commas or closing ')' cannot come from macro
	'' expansions.

	var is_variadic = ((macro->attrib and ASTATTRIB_VARIADIC) <> 0)

	'' For each arg in the input...
	var reached_lastarg = FALSE
	do
		if argcount >= MAXARGS then
			tkOops(x, "macro call arg buffer too small, MAXARGS=" & MAXARGS)
		end if

		argbegin[argcount] = x

		'' Is this the argument for the last parameter of a variadic macro?
		'' We're going to read all the remaining tokens into this last argument,
		'' even commas, thus there won't be any other arguments following after this one.
		assert((not is_variadic) or (not reached_lastarg))
		reached_lastarg = (argcount = (macro->paramcount - 1))

		'' For each token that's part of this arg...
		var level = 0
		do
			select case tkGet(x)
			case TK_LPAREN
				level += 1

			case TK_RPAREN
				if level <= 0 then
					exit do
				end if
				level -= 1

			case TK_COMMA
				'' A toplevel comma ends the current arg, unless it's a "..." vararg,
				'' which just "absorbs" everything until the closing ')'.
				if level <= 0 then
					if (not is_variadic) or (not reached_lastarg) then
						exit do
					end if
				end if

			case TK_EOF
				tkOopsExpected(x, "')' to close macro call argument list")
			end select

			x += 1
		loop

		argend[argcount] = x - 1
		argcount += 1

		'' ','?
		if tkGet(x) <> TK_COMMA then
			exit do
		end if
		x += 1
	loop

	'' It's ok to omit the arg(s) for the variadic parameter of a variadic macro.
	if is_variadic and (not reached_lastarg) then
		if argcount >= MAXARGS then
			tkOops(x, "macro call arg buffer too small, MAXARGS=" & MAXARGS)
		end if
		argbegin[argcount] = x
		argend[argcount] = x - 1
		argcount += 1
	end if

	'' Not the expected amount of args?
	if argcount <> macro->paramcount then
		dim s as string
		if argcount > macro->paramcount then
			s = "too many"
		else
			s = "not enough"
		end if
		s += " arguments for '" + *macro->text + "' macro call: "
		s &= argcount & " given, " & macro->paramcount & " needed"
		tkOops(x, s)
	end if
end sub

private function hParseMacroCall _
	( _
		byval x as integer, _
		byval macro as ASTNODE ptr, _
		byval argbegin as integer ptr, _
		byval argend as integer ptr, _
		byref argcount as integer _
	) as integer

	var begin = x

	'' ID
	assert(tkGet(x) >= TK_ID)
	x += 1

	argcount = -1

	'' Not just "#define m"?
	if macro->paramcount >= 0 then
		'' '('?
		if tkGet(x) <> TK_LPAREN then
			return -1
		end if
		x += 1

		argcount = 0

		'' Not just "#define m()"?
		if macro->paramcount > 0 then
			'' Parse the argument list and fill the argbegin() and
			'' argend() arrays accordingly
			hParseMacroCallArgs(x, macro, argbegin, argend, argcount)
		end if

		'' ')'?
		tkExpect(x, TK_RPAREN, "to close macro call argument list")
		x += 1
	end if

	function = x - 1
end function

'' DEFINED ['('] Identifier [')']
private sub hSkipDefinedUop(byref x as integer)
	assert(tkGet(x) = KW_DEFINED)
	x += 1

	'' '('?
	var have_lparen = FALSE
	if tkGet(x) = TK_LPAREN then
		have_lparen = TRUE
		x += 1
	end if

	'' Identifier? (not doing any expansion here)
	if tkGet(x) >= TK_ID then
		x += 1
	end if

	'' ')'?
	if have_lparen then
		if tkGet(x) = TK_RPAREN then
			x += 1
		end if
	end if
end sub

private sub hWrapInTkBeginEnd(byval first as integer, byval last as integer)
	assert(first <= last)
	tkInsert(first, TK_BEGIN)
	last += 1
	tkInsert(last + 1, TK_END)
end sub

private sub hUnwrapTkBeginEnd(byval first as integer, byval last as integer)
	assert(tkGet(first) = TK_BEGIN)
	assert(tkGet(last) = TK_END)
	tkRemove(first, first)
	last -= 1
	tkRemove(last, last)
end sub

private function hExpandInTkBeginEnd _
	( _
		byval x as integer, _
		byval inside_ifexpr as integer _
	) as integer

	assert(tkGet(x) = TK_BEGIN)

	do
		select case tkGet(x)
		case TK_END
			exit do

		case KW_DEFINED
			'' If inside an #if condition expression, don't expand symbols behind the defined operator.
			'' According to the C standard, the handling of defined's that result from macro expansion
			'' is undefined, but gcc handles them as normal defined's, so we do too.
			if inside_ifexpr then
				hSkipDefinedUop(x)
				x -= 1
			end if

		case is >= TK_ID
			if hMaybeExpandMacro(x, inside_ifexpr, TRUE) then
				'' TK_ID replaced by macro body - reparse
				x -= 1
			end if
		end select

		x += 1
	loop

	function = x
end function

private function hExpandInRange _
	( _
		byval first as integer, _
		byval last as integer, _
		byval inside_ifexpr as integer _
	) as integer

	'' Do nothing if range is empty - happens when expanding in a macro
	'' expansion but the expansion is empty, or when expanding in an #if
	'' condition but it's missing.
	if first > last then
		return last
	end if

	'' Insert TK_BEGIN/TK_END around the argument's tokens, to prevent the
	'' macro call parsing functions from reading out-of-bounds.
	hWrapInTkBeginEnd(first, last)
	last += 2
	assert(tkGet(last) = TK_END)

	'' Expand anything in the range
	last = hExpandInTkBeginEnd(first, inside_ifexpr)

	'' Remove TK_BEGIN/TK_END again
	hUnwrapTkBeginEnd(first, last)
	last -= 2

	function = last
end function

'' Set or unset the BEHINDSPACE flag of a token
private sub hOverrideBehindspace(byval x as integer, byval flag as integer)
	tkSetFlags(x, (tkGetFlags(x) and (not TKFLAG_BEHINDSPACE)) or flag)
end sub

''
'' - Macro arguments must be inserted in place of macro parameters, and fully
''   macro-expanded, but only self-contained without help from tokens outside
''   the argument.
''
'' - Arguments used with # mustn't be macro-expanded, and for arguments used
''   with ##, the last/first token musn't be macro-expanded depending on whether
''   the parameter was on the lhs/rhs of the ## (but the rest of the argument's
''   tokens that aren't used by the ##, if any, must be macro-expanded).
''   I.e. macro expansion mustn't be done when parsing the arguments, but later
''   when inserting them in place of parameters, with the given restrictions.
''
'' - # or ## tokens coming from arguments must not be treated as stringify/merge
''   operators. This must be done only for # or ## in the macro body.
''
'' - #stringify operations must be solved before ## merging (e.g. <L ## #param>
''   becomes <L"argtext">)
''
'' - ## operands may be empty: if an argument is used with ##, but the argument
''   is empty, then the ## doesn't merge anything. ## with 2 empty operands
''   is removed completely. Macro body token(s) preceding/following the ##
''   operand are not taken into account for the merge. Empty ## operand doesn't
''   cause preceding/following tokens to be used instead.
''
'' - If a macro parameter expands to multiple tokens, ## affects the last/first
''   token from the lhs/rhs operands respectively, but not all the tokens
''   inserted in place of the parameter(s).
''
private function hInsertMacroExpansion _
	( _
		byval callbehindspace as integer, _
		byval expansionbegin as integer, _
		byval definfo as DEFINEINFO ptr, _
		byval argbegin as integer ptr, _
		byval argend as integer ptr, _
		byval argcount as integer, _
		byval inside_ifexpr as integer _
	) as integer

	'' Insert the macro body tokens from AST into the tk buffer, surrounded
	'' with TK_BEGIN/TK_END, to allow the code below to read "out-of-bounds"
	'' by -1 or +1, which simplifies handling of # and ## operators.
	''
	'' Having the TK_END also removes the need to keep track of the end of
	'' the expansion through all the insertions/deletions done here.
	'' Instead, if we need to know the end of the expansion, we can just
	'' look for the TK_END.
	tkInsert(expansionbegin, TK_END)
	definfoCopyBody(definfo, expansionbegin)
	tkInsert(expansionbegin, TK_BEGIN)

	'' Update the BEHINDSPACE status of the first token in the expansion to
	'' be the same as that of the macro name which we're expanding
	hOverrideBehindspace(expansionbegin + 1, callbehindspace)

	'' Solve #stringify operators (higher priority than ##, and no macro
	'' expansion done for the arg)
	var x = expansionbegin + 1
	while tkGet(x) <> TK_END

		'' '#param'?
		if tkGet(x) = TK_HASH then
			'' Followed by identifier?
			if tkGet(x + 1) >= TK_ID then
				'' Is it a macro parameter?
				var arg = astLookupMacroParam(definfo->macro, tkSpellId(x + 1))
				if arg >= 0 then
					'' Remove #param, and insert stringify result instead
					'' but preserve BEHINDSPACE status.
					assert((arg >= 0) and (arg < argcount))
					var behindspace = tkGetFlags(x) and TKFLAG_BEHINDSPACE
					tkRemove(x, x + 1)

					'' " must be replaced by \"
					'' then we can wrap the stringified text in "..." to produce the TK_STRING
					var s = tkSpell(argbegin[arg], argend[arg])
					s = strReplace(s, """", $"\""")
					s = """" + s + """"

					tkInsert(x, TK_STRING, s)
					hOverrideBehindspace(x, behindspace)
				end if
			end if
		end if

		x += 1
	wend

	'' Replace ## tokens by special internal merge operator tokens, so that
	'' ## tokens from macro arguments aren't mistaken for merge operators.
	x = expansionbegin + 1
	while tkGet(x) <> TK_END

		'' '##'?
		if tkGet(x) = TK_HASHHASH then
			tkInsert(x, TK_PPMERGE)
			var y = x + 1
			tkSetLocation(x, tkGetLocation(y))
			tkRemove(y, y)
		end if

		x += 1
	wend

	'' Insert args into params, surrounded with TK_ARGBEGIN/END, so that
	'' - we know when an arg was empty when doing ## merging (to avoid
	''   merging with other tokens outside the arg),
	'' - we know the arg's boundaries for macro-expanding it later. (must be
	''   done after merging, because only the unmerged tokens of an arg
	''   shall be macro-expanded, and not the ones involved in merging)
	x = expansionbegin + 1
	while tkGet(x) <> TK_END

		'' Macro parameter?
		if tkGet(x) >= TK_ID then
			var arg = astLookupMacroParam(definfo->macro, tkSpellId(x))
			if arg >= 0 then
				'' >= TK_ID
				var behindspace = tkGetFlags(x) and TKFLAG_BEHINDSPACE
				tkRemove(x, x)

				'' TK_ARGBEGIN
				tkInsert(x, TK_ARGBEGIN)
				x += 1

				'' arg's tokens
				tkCopy(x, argbegin[arg], argend[arg], DEFINEBODY_FLAGMASK)
				hOverrideBehindspace(x, behindspace)
				x += argend[arg] - argbegin[arg] + 1

				'' TK_ARGEND
				tkInsert(x, TK_ARGEND)
			end if
		end if

		x += 1
	wend

	''
	'' Do '##' merging
	''
	'' It's not clear how <a ## ## b> or <a ## b ## c> should be processed
	'' (undefined behaviour), so fbfrog shows an error about the first
	'' (cannot merge a and ##) and processes the 2nd as (a##b)##c, i.e.
	'' left-associative.
	''
	x = expansionbegin + 1
	while tkGet(x) <> TK_END

		'' '##' from original macro body (and not '##' from a macro argument)?
		if tkGet(x) = TK_PPMERGE then

			'' 1. If lhs/rhs of '##' were params, then now there will be TK_ARGBEGIN,...,TK_ARGEND sequences.
			'' Move last/first token out of the arg boundaries, so that they end up right next to the '##'.
			'' (can just move the TK_ARGEND/TK_ARGBEGIN respectively, that's easier & faster)
			''
			'' Example with arg on both sides:
			'' from:
			''    [argbegin] a b [argend] ## [argbegin] c d [argend]
			'' to:
			''    [argbegin] a [argend] b ## c [argbegin] d [argend]
			''
			'' If this causes an TK_ARGBEGIN/END to become empty, it must be removed,
			'' so that it won't be misinterpreted as empty arg operand for a following ## operator:
			'' from:
			''    [argbegin] a [argend] ## [argbegin] b [argend] ## [argbegin] c [argend]
			'' to:
			''    a##b ## [argbegin] c [argend]
			'' in order to avoid the situation where the 2nd ##'s lhs seems to be an empty arg:
			''    [argbegin] [argend] a ## b [argbegin] [argend] ## [argbegin] c [argend]
			'' because actually the merged "ab" token is supposed to be 2nd ##'s lhs.

			'' lhs was a non-empty arg?
			if (tkGet(x - 1) = TK_ARGEND) and (tkGet(x - 2) <> TK_ARGBEGIN)  then
				tkRemove(x - 1, x - 1)
				tkInsert(x - 2, TK_ARGEND)
				assert(tkGet(x) = TK_PPMERGE)
				assert(tkGet(x - 1) <> TK_ARGEND)
				assert(tkGet(x - 2) = TK_ARGEND)

				'' Empty now? Then remove the TK_ARGBEGIN/END
				if tkGet(x - 3) = TK_ARGBEGIN then
					tkRemove(x - 3, x - 2)
					x -= 2
				end if
			end if

			'' rhs was a non-empty arg?
			if (tkGet(x + 1) = TK_ARGBEGIN) and (tkGet(x + 2) <> TK_ARGEND) then
				tkRemove(x + 1, x + 1)
				tkInsert(x + 2, TK_ARGBEGIN)
				assert(tkGet(x) = TK_PPMERGE)
				assert(tkGet(x + 1) <> TK_ARGBEGIN)
				assert(tkGet(x + 2) = TK_ARGBEGIN)

				'' Empty now? Then remove the TK_ARGBEGIN/END
				if tkGet(x + 3) = TK_ARGEND then
					tkRemove(x + 2, x + 3)
				end if
			end if

			assert(tkGet(x) = TK_PPMERGE)
			var l = x - 1
			var r = x + 1

			'' If one operand was an empty arg, then no merging needs to be done,
			'' the other operand can just be preserved as-is; or in case both were
			'' empty, the ## just disappears.

			'' Non-empty on both sides?
			if (tkGet(l) <> TK_ARGEND) and (tkGet(r) <> TK_ARGBEGIN) then
				if tkGet(l) = TK_BEGIN then
					tkOops(x, "## merge operator at beginning of macro body, missing operand to merge with")
				end if
				if tkGet(r) = TK_END then
					tkOops(x, "## merge operator at end of macro body, missing operand to merge with")
				end if

				'' Combine the original text representation of both tokens,
				'' and prepend a space if the lhs was BEHINDSPACE, such that
				'' the merged token will also be BEHINDSPACE.
				dim mergetext as string
				if tkGetFlags(l) and TKFLAG_BEHINDSPACE then
					mergetext += " "
				end if
				mergetext += tkSpell(l) + tkSpell(r)

				'' and try to lex them
				var y = tkGetCount()
				lexLoadC(y, mergetext, sourceinfoForZstring("## merge operation"))

				'' That should have produced only 1 token. If it produced more, then the merge failed.
				assert(tkGetCount() >= (y + 1))
				if tkGetCount() > (y + 1) then
					tkRemove(y, tkGetCount() - 1)
					tkOops(x, "## merge operator cannot merge '" + tkSpell(x - 1) + "' and '" + tkSpell(x + 1) + "'")
				end if

				'' Remove the 3 (l ## r) tokens and insert the merged token in place of l
				tkRemove(l, r)
				y -= 3
				x = l

				tkCopy(x, y, y, DEFINEBODY_FLAGMASK)
				y += 1

				tkRemove(y, y)
			else
				'' Just remove the '##'
				tkRemove(x, x)
				x -= 1
			end if
		end if

		x += 1
	wend

	'' Recursively macro-expand the tokens in each TK_ARGBEGIN/END sequence,
	'' and then remove TK_ARGBEGIN/END.
	x = expansionbegin + 1
	while tkGet(x) <> TK_END

		'' Macro parameter?
		if tkGet(x) = TK_ARGBEGIN then
			var y = x
			do
				y += 1
			loop while tkGet(y) <> TK_ARGEND

			'' Macro-expand the arg's tokens
			y = hExpandInRange(x, y, inside_ifexpr)

			'' Remove TK_ARGBEGIN/END wrapping
			assert(tkGet(x) = TK_ARGBEGIN)
			tkRemove(x, x)
			x -= 1
			y -= 1
			assert(tkGet(y) = TK_ARGEND)
			tkRemove(y, y)
			y -= 1

			x = y
		end if

		x += 1
	wend

	'' Remove the TK_BEGIN/END wrapping around the expansion
	assert(tkGet(expansionbegin) = TK_BEGIN)
	tkRemove(expansionbegin, expansionbegin)
	x -= 1
	assert(tkGet(x) = TK_END)
	tkRemove(x, x)
	x -= 1

	function = x
end function

private sub hExpandMacro _
	( _
		byval definfo as DEFINEINFO ptr, _
		byval callbegin as integer, _
		byval callend as integer, _
		byval argbegin as integer ptr, _
		byval argend as integer ptr, _
		byval argcount as integer, _
		byval inside_ifexpr as integer, _
		byval expand_recursively as integer _
	)

	'' Insert the macro body behind the call (this way the positions
	'' stored in argbegin()/argend() stay valid)
	var expansionbegin = callend + 1
	var expansionend = hInsertMacroExpansion( _
			tkGetFlags(callbegin) and TKFLAG_BEHINDSPACE, _
			expansionbegin, definfo, argbegin, argend, argcount, inside_ifexpr)

	'' Mark expansion tokens
	tkAddFlags(expansionbegin, expansionend, TKFLAG_EXPANSION)

	if expand_recursively then
		'' Recursively do macro expansion in the expansion
		'' - Marking the current macro as poisoned, so it won't be expanded
		''   again within the expansion, preventing expansion of complete
		''   recursive calls.
		'' - Incomplete recursive calls need to be marked with NOEXPAND so they
		''   won't be expanded later when they become complete by taking into
		''   account tokens following behind the expansion.
		definfo->macro->attrib or= ASTATTRIB_POISONED
		expansionend = hExpandInRange(expansionbegin, expansionend, inside_ifexpr)
		definfo->macro->attrib and= not ASTATTRIB_POISONED
	end if

	'' Disable future expansion of recursive macro calls to this macro
	'' (those that weren't expanded due to the "poisoning")
	scope
		var x = expansionbegin
		while x <= expansionend

			if tkGet(x) >= TK_ID then
				'' Known macro, and it's the same as this one?
				var calldefinfo = hCheckForMacroCall(x)
				if calldefinfo = definfo then
					'' Can the macro call be parsed successfully,
					'' and is it fully within the expansion?
					dim as integer argbegin(0 to MAXARGS-1)
					dim as integer argend(0 to MAXARGS-1)
					dim as integer argcount
					var callend = hParseMacroCall(x, definfo->macro, @argbegin(0), @argend(0), argcount)
					if (callend >= 0) and (callend <= expansionend) then
						tkAddFlags(x, x, TKFLAG_NOEXPAND)
					end if
				end if
			end if

			x += 1
		wend
	end scope

	'' Update locations on the expansion tokens to point to the macro call,
	'' instead of the #define body etc.
	scope
		var x = expansionbegin
		while x <= expansionend
			tkSetLocation(x, tkGetLocation(callbegin))
			x += 1
		wend
	end scope

	'' Then remove the call tokens
	tkRemove(callbegin, callend)
end sub

private function hMaybeExpandMacro(byval x as integer, byval inside_ifexpr as integer, byval expand_recursively as integer) as integer
	var definfo = hCheckForMacroCall(x)
	if definfo = NULL then
		exit function
	end if

	dim as integer argbegin(0 to MAXARGS-1)
	dim as integer argend(0 to MAXARGS-1)
	dim as integer argcount

	'' Try to parse the macro call (can fail in case of function-like macro
	'' without argument list)
	var callbegin = x
	var callend = hParseMacroCall(callbegin, definfo->macro, @argbegin(0), @argend(0), argcount)
	if callend < 0 then
		exit function
	end if

	hExpandMacro(definfo, callbegin, callend, @argbegin(0), @argend(0), argcount, inside_ifexpr, expand_recursively)
	function = TRUE
end function

private function cppGetFileContext() as cpp.STACKNODE ptr
	for i as integer = cpp.level to 0 step -1
		var ctx = @cpp.stack(i)
		if ctx->state = STATE_FILE then
			return ctx
		end if
	next
	assert(FALSE)
end function

private sub cppPush(byval state as integer, byval knownfile as integer = -1)
	assert(iif(knownfile >= 0, state = STATE_FILE, TRUE))

	cpp.level += 1
	if cpp.level >= MAXSTACK then
		tkOops(cpp.x, "#if/#include stack too small, MAXSTACK=" & MAXSTACK)
	end if

	with cpp.stack(cpp.level)
		.state = state
		.knownfile = knownfile
		.incdir = NULL
	end with

	if state = STATE_FILE then
		cpp.filelevel += 1
	end if
end sub

private sub cppPop()
	'' Finished parsing a file?
	with cpp.stack(cpp.level)
		if .state = STATE_FILE then
			cpp.filelevel -= 1
			if .knownfile >= 0 then
				cpp.files[.knownfile].guardstate = GUARDSTATE_KNOWN
			end if
		end if
	end with
	cpp.level -= 1
end sub

private sub cppApplyIf(byval condition as integer)
	if condition then
		'' #if TRUE, don't skip
		cpp.stack(cpp.level).state = STATE_TRUE
		cpp.skiplevel = MAXSTACK  '' needed for #elif, in case we were skipping previously
	else
		'' #if FALSE, start skipping (or in case of #elif, possibly continue)
		cpp.skiplevel = cpp.level
	end if
end sub

private function hSkipEols(byval x as integer) as integer
	while tkGet(x) = TK_EOL
		x += 1
	wend
	function = x
end function

private function cppIfExpr() as integer
	'' Expand macros in the #if condition before parsing it
	'' * but don't expand operands of the "defined" operator
	'' * we allow "defined" operators to be produced by
	''   macro expansion, like gcc
	hExpandInRange(cpp.x, hSkipToEol(cpp.x) - 1, TRUE)

	'' Try to parse and evaluate an expression
	dim value as CPPVALUE
	cppExpression(value, FALSE)
	function = (value.vali <> 0)
end function

private sub cppIf()
	cppPush(STATE_IF)
	cpp.x += 1

	if cppSkipping() then
		exit sub
	end if

	'' Condition expression
	cppApplyIf(cppIfExpr())

	cppEol()
end sub

private sub cppIfdef(byval directivekw as integer)
	cppPush(STATE_IF)
	cpp.x += 1

	if cppSkipping() then
		exit sub
	end if

	'' Identifier
	if tkGet(cpp.x) < TK_ID then
		tkExpect(cpp.x, TK_ID, "behind " + tkInfoPretty(directivekw))
	end if
	var id = tkSpellId(cpp.x)
	hCheckForUnknownSymbol(id)
	cpp.x += 1

	var condition = cppIsMacroCurrentlyDefined(id)
	if directivekw = KW_IFNDEF then
		condition = not condition
	end if
	cppApplyIf(condition)

	cppEol()
end sub

'' Forget the guard (if any) for the current file context
'' It's possible that we're in a recursive #include, but it doesn't matter,
'' we'll just try to disable it's #include guard optimization multiple times.
private sub cppDisableIncludeGuardOptimization()
	assert(cpp.level >= 1)
	assert(cpp.stack(cpp.level-1).state = STATE_FILE)
	var knownfile = cpp.stack(cpp.level-1).knownfile
	if knownfile >= 0 then
		with cpp.files[knownfile]
			if .guard then
				assert(.guardstate = GUARDSTATE_CHECKING)
				deallocate(.guard)
				.guard = NULL
			end if
		end with
	end if
end sub

'' Check whether we're inside the first nesting level inside a file
'' (for example, an #include guard)
private function cppInsideFileLevelBlock() as integer
	assert(cpp.stack(cpp.level).state <> STATE_FILE)
	if cpp.level >= 1 then
		function = (cpp.stack(cpp.level-1).state = STATE_FILE)
	end if
end function

private sub cppElseIf()
	'' Verify #elif usage even if skipping
	select case cpp.stack(cpp.level).state
	case is < STATE_IF
		tkOops(cpp.x, "#elif without #if")
	case STATE_ELSE
		tkOops(cpp.x, "#elif after #else")
	end select
	cpp.x += 1

	if cppInsideFileLevelBlock() then
		cppDisableIncludeGuardOptimization()
	end if

	'' Evaluate condition in case it matters:
	''    a) not yet skipping,
	''    b) skipping due to a previous #if/#elif FALSE
	if (cpp.skiplevel = MAXSTACK) or (cpp.skiplevel = cpp.level) then
		'' But not if there already was an #if/#elif TRUE on this level
		'' (then this #elif isn't reached)
		if cpp.stack(cpp.level).state = STATE_TRUE then
			'' Start/continue skipping
			cpp.skiplevel = cpp.level
		else
			'' Condition expression
			cppApplyIf(cppIfExpr())
			cppEol()
		end if
	end if
end sub

private sub cppElse()
	'' Verify #else usage even if skipping
	select case cpp.stack(cpp.level).state
	case is < STATE_IF
		tkOops(cpp.x, "#else without #if")
	case STATE_ELSE
		tkOops(cpp.x, "#else after #else")
	end select
	cpp.x += 1

	if cppInsideFileLevelBlock() then
		cppDisableIncludeGuardOptimization()
	end if

	cppEol()

	'' Check whether to skip this #else or not, if
	''    a) not yet skipping,
	''    b) skipping due to a previous #if/#elif FALSE
	if (cpp.skiplevel = MAXSTACK) or (cpp.skiplevel = cpp.level) then
		if cpp.stack(cpp.level).state = STATE_TRUE then
			'' Previous #if/#elseif TRUE, skip #else
			cpp.skiplevel = cpp.level
		else
			'' Previous #if/#elseif FALSE, don't skip #else
			cpp.skiplevel = MAXSTACK
		end if
	end if

	cpp.stack(cpp.level).state = STATE_ELSE
end sub

private sub cppEndIf()
	if cpp.stack(cpp.level).state < STATE_IF then
		tkOops(cpp.x, "#endif without #if")
	end if
	cpp.x += 1

	cppEol()

	if cppInsideFileLevelBlock() then
		'' If we don't reach the #include EOF directly after the #endif,
		'' then this can't be an #include guard
		if tkGet(hSkipEols(cpp.x)) <> TK_ENDINCLUDE then
			assert(tkGet(hSkipEols(cpp.x)) <> TK_EOF)
			cppDisableIncludeGuardOptimization()
		end if
	end if

	'' If skipping due to current level, then stop skipping.
	if cpp.skiplevel = cpp.level then
		cpp.skiplevel = MAXSTACK
	end if

	cppPop()
end sub

private sub maybePrintIncludeTree(byref inctext as string, byref prettyfile as string, byval include_skipped as integer)
	if frog.verbose then
		var s = string(cpp.filelevel, ".") + " "
		if include_skipped then s += "("
		s += inctext
		if prettyfile <> inctext then
			s += " => " + prettyfile
		end if
		if include_skipped then s += ")"
		print s
	end if
end sub

'' Search for #included files in one of the parent directories of the context
'' file. Usually the #include will refer to a file in the same directory or in
'' a sub-directory at the same level or some levels up.
private function hSearchHeaderFile _
	( _
		byref contextfile as string, _
		byval contextincdir as ASTNODE ptr, _
		byref inctext as string, _
		byval is_system_include as integer, _
		byref incdir as ASTNODE ptr _
	) as string

	'' If #including by absolute path, use it as-is
	if pathIsAbsolute(inctext) then
		return inctext
	end if

	'' Relative to context file, unless it was #include <...> or #include_next
	if (contextincdir = NULL) and (not is_system_include) then
		var incfile = pathAddDiv(pathOnly(contextfile)) + inctext
		if fileexists(incfile) then
			return incfile
		end if
		maybePrintIncludeTree(inctext, "not found at " + incfile, FALSE)
	end if

	'' In any of the include search directories; #include_next starts with
	'' the incdir following the one where the parent file was found
	var i = iif(contextincdir, contextincdir->next, cpp.incdirs->head)
	while i

		var incfile = pathAddDiv(*i->text) + inctext
		if fileexists(incfile) then
			incdir = i
			return incfile
		end if
		maybePrintIncludeTree(inctext, "not found at " + incfile, FALSE)

		i = i->next
	wend

	function = ""
end function

'' Check for the typical #include guard header:
''    #ifndef ID <EOL> #define ID ...
private function hDetectIncludeGuardBegin(byval first as integer) as zstring ptr
	assert(tkGet(first - 1) = TK_EOL)

	var x = hSkipEols(first)

	if tkGet(x) <> TK_HASH then exit function
	x += 1
	if tkGet(x) <> KW_IFNDEF then exit function
	x += 1
	if tkGet(x) <> TK_ID then exit function
	var id1 = tkGetText(x)
	x += 1
	if tkGet(x) <> TK_EOL then exit function
	x += 1
	if tkGet(x) <> TK_HASH then exit function
	x += 1
	if tkGet(x) <> KW_DEFINE then exit function
	x += 1
	if tkGet(x) <> TK_ID then exit function
	var id2 = tkGetText(x)
	if *id1 <> *id2 then exit function

	function = id1
end function

'' "filename" | <filename>
'' Escape sequences in "filename" are not evaluated.
'' TODO: Don't evaluate escape sequences/comments in <filename>
private function cppIncludeFilename(byref is_system_include as integer) as string
	select case tkGet(cpp.x)
	case TK_LT
		'' <filename>
		is_system_include = TRUE

		'' Skip tokens until the '>'
		var begin = cpp.x
		do
			cpp.x += 1
			select case tkGet(cpp.x)
			case TK_GT
				exit do
			case TK_EOL, TK_EOF
				tkOops(cpp.x, "missing '>' to finish #include <...")
			end select
		loop

		'' Then spell them to get the filename
		function = tkSpell(begin + 1, cpp.x - 1)
		cpp.x += 1

	case TK_STRING
		'' "filename"
		function = cppStringLiteral(FALSE)

	case else
		tkOopsExpected(cpp.x, """filename"" or <filename> behind #include")
	end select
end function

private sub cppInclude(byval begin as integer, byref flags as integer, byval is_include_next as integer)
	cpp.x += 1

	assert(cppSkipping() = FALSE)

	'' Expand macros behind the #include (but still in the same line)
	'' but only if there is not already a " or < (like gcc).
	'' This way we avoid expanding macros inside <...> which can't be made
	'' a single token like "..." because it depends on context. It should
	'' only be a single token if used for an #include, but if it's written
	'' in a #define body then we don't know what the context will be.
	select case tkGet(cpp.x)
	case TK_LT, TK_STRING
	case else
		hExpandInRange(cpp.x, hSkipToEol(cpp.x) - 1, FALSE)
	end select

	'' "filename" | <filename>
	var location = tkGetLocation(cpp.x)
	var includetkflags = tkGetFlags(cpp.x)
	var is_system_include = FALSE
	var inctext = cppIncludeFilename(is_system_include)

	cppEol()

	dim incfile as string
	dim incdir as ASTNODE ptr
	if includetkflags and TKFLAG_ROOTFILE then
		'' No #include file search for internal #includes
		incfile = inctext
	else
		'' #include file search
		dim contextfile as string
		if location.source then
			contextfile = *location.source->name
		end if

		dim contextincdir as ASTNODE ptr
		if is_include_next then
			contextincdir = cppGetFileContext()->incdir
		end if

		incfile = hSearchHeaderFile(contextfile, contextincdir, inctext, is_system_include, incdir)
		if len(incfile) = 0 then
			'' #include not found
			cpp.api->print(inctext + " (not found)")

			'' Preserve non-internal #includes that weren't found
			if (includetkflags and (TKFLAG_PREINCLUDE or TKFLAG_ROOTFILE)) = 0 then
				flags and= not TKFLAG_REMOVE
			end if

			exit sub
		end if
	end if

	'' Get the normalized representation of the path, for use in hash tables
	'' etc. Otherwise foo.h from the root dir and ../foo.h from a subdir
	'' would be seen as different files.
	incfile = pathNormalize(pathMakeAbsolute(incfile))

	'' * Don't preserve internal #includes,
	'' * don't preserve #includes if we will emit the #included content
	''   into the same .bi file as the #include directive itself.
	''
	'' We do this check here instead of later when distributing declarations
	'' into .bi files, because #include tokens/ASTNODEs don't carry enough
	'' information about the #included file. Knowing the #include "filename"
	'' isn't enough, because it may be a relative path such as "../foo.h".
	''
	'' Not internal?
	if (includetkflags and (TKFLAG_PREINCLUDE or TKFLAG_ROOTFILE)) = 0 then
		assert(location.source->is_file)
		var directivebi = frogLookupBiFromH(location.source->name)
		var contentbi = frogLookupBiFromH(incfile)
		'' Not emitted into same .bi as #included content?
		if directivebi <> contentbi then
			'' Then preserve it
			flags and= not TKFLAG_REMOVE
		end if
	end if

	'' For display we make the filename relative to curdir()
	var prettyfile = pathStripCurdir(incfile)

	var knownfile = cppLookupOrAppendKnownFile(incfile, prettyfile)
	with cpp.files[knownfile]
		'' Did we find a #pragma once in this file previously?
		if .pragmaonce then
			'' Don't #include it again ever
			maybePrintIncludeTree(inctext, prettyfile, TRUE)
			exit sub
		end if

		'' Did we find an #include guard in this file previously?
		if (.guardstate = GUARDSTATE_KNOWN) and (.guard <> NULL) then
			'' Only load the file if the guard symbol isn't defined (anymore) now.
			if cppIsMacroCurrentlyDefined(.guard) then
				'' Skipping header due to include guard
				maybePrintIncludeTree(inctext, prettyfile, TRUE)
				exit sub
			end if
		end if
	end with

	maybePrintIncludeTree(inctext, prettyfile, FALSE)
	cpp.api->print(prettyfile)

	'' Push the #include file context
	cppPush(STATE_FILE, knownfile)
	cpp.stack(cpp.level).incdir = incdir

	'' Read the include file and insert its tokens
	var file = filebuffersAdd(incfile, location)
	var y = lexLoadC(cpp.x, file->buffer, file->source)

	'' If tokens were inserted, ensure there is an EOL at the end
	if cpp.x < y then
		if tkGet(y - 1) <> TK_EOL then
			tkInsert(y, TK_EOL)
			y += 1
		end if
	end if

	'' Put TK_ENDINCLUDE behind the #include file content, so we can detect
	'' the included EOF and pop the #include context from the cpp.stack.
	tkInsert(y, TK_ENDINCLUDE)
	y += 1

	'' Insert EOL behind the TK_ENDINCLUDE so we can detect BOL there
	tkInsert(y, TK_EOL)
	y += 1

	'' Start parsing the #included content
	assert(tkGet(cpp.x - 1) = TK_EOL)
	assert(y <= tkGetCount())
	assert(tkGet(y - 2) = TK_ENDINCLUDE)

	''
	'' Prepare for the include guard optimization
	''
	'' If we didn't check this file for an #include guard yet, and we're
	'' not currently checking already (recursive #includes), then we can
	'' check during this #include context.
	''
	'' Does the #include begin with the typical #include guard header?
	''     #ifndef FOO
	''     #define FOO
	''
	'' We'll store the guard id (if any) for now. If we later find that
	'' there is an #elif/#else, or that we don't reach #include EOF after
	'' the "guard" #endif, then we can mark the include guard optimization
	'' as impossible by setting the guard to NULL.
	'' (see cppDisableIncludeGuardOptimization())
	''
	with cpp.files[knownfile]
		if .guardstate = 0 then
			.guardstate = GUARDSTATE_CHECKING
			assert(.guard = NULL)
			.guard = strDuplicate(hDetectIncludeGuardBegin(cpp.x))
		end if
	end with
end sub

private sub cppEndInclude()
	assert(cpp.skiplevel = MAXSTACK)
	assert(cpp.level > 0)
	if cpp.stack(cpp.level).state >= STATE_IF then
		tkOops(cpp.x - 1, "missing #endif")
	end if
	cppPop()

	'' Mark the TK_ENDINCLUDE for removal, so they won't get in the way of
	'' C parsing (in case declarations cross #include/file boundaries).
	tkSetRemove(cpp.x, cpp.x)
	cpp.x += 1
end sub

private sub hMaybeExpandMacroInDefineBody(byval parentdefine as ASTNODE ptr)
	var id = tkSpellId(cpp.x)

	'' Only expand if the called macro was given with -expandindefine
	if cpp.api->idopt(OPT_EXPANDINDEFINE).matches(id) = FALSE then
		exit sub
	end if

	'' Similar to hMaybeExpandMacro():
	var definfo = hCheckForMacroCall(cpp.x)
	if definfo = NULL then
		exit sub
	end if

	dim as integer argbegin(0 to MAXARGS-1)
	dim as integer argend(0 to MAXARGS-1)
	dim as integer argcount
	var callbegin = cpp.x
	var callend = hParseMacroCall(callbegin, definfo->macro, @argbegin(0), @argend(0), argcount)
	if callend < 0 then
		exit sub
	end if

	'' Don't expand if the macrocall involves parameters of the parentdefine
	for i as integer = callbegin to callend
		if tkGet(i) >= TK_ID then
			if astLookupMacroParam(parentdefine, tkSpellId(i)) >= 0 then
				exit sub
			end if
		end if
	next

	hExpandMacro(definfo, callbegin, callend, @argbegin(0), @argend(0), argcount, FALSE, FALSE)

	'' TK_ID expanded; reparse it
	cpp.x -= 1
end sub

private function hShouldRemoveDefine(byval id as zstring ptr) as integer
	function = cpp.api->idopt(OPT_REMOVEDEFINE).matches(id)
end function

'' DEFINE Identifier ['(' ParameterList ')'] Body Eol
private sub cppDefine(byref flags as integer)
	cpp.x += 1

	assert(cppSkipping() = FALSE)

	'' Identifier ['(' ParameterList ')']
	var macro = hDefineHead(cpp.x)

	'' Body
	var xbody = cpp.x

	''
	'' If there are any -expandindefine options, look for corresponding
	'' macros in the #define body, and expand them.
	''
	'' But don't expand macro calls that involve parameters of the #define,
	'' because then we risk wrong expansion:
	''    -expandindefine a
	''    #define a(x) x##1
	''    #define b(x) a(x)
	''    #define c(x) x + a(1)
	'' =>
	''    #define a(x) x##1
	''    #define b(x) x1      // wrong, b() is broken now
	''    #define c(x) x + 11  // ok: invocation of a() doesn't involve x
	''
	if cpp.api->idopt(OPT_EXPANDINDEFINE).nonEmpty then
		do
			select case tkGet(cpp.x)
			case TK_EOL
				exit do

			case is >= TK_ID
				hMaybeExpandMacroInDefineBody(macro)

			end select

			cpp.x += 1
		loop
	end if

	'' Eol
	var xeol = hSkipToEol(xbody)
	assert(tkGet(xeol) = TK_EOL)
	cpp.x = xeol + 1

	var definfo = definfoNew()
	definfo->xbody = xbody
	definfo->xeol = xeol
	definfo->macro = macro

	if frog.verbose >= 2 then
		print "#define " + *macro->text + " " + tkSpell(xbody, xeol)
	end if

	'' Report conflicting #defines
	var prevdef = cppLookupMacro(macro->text)
	if prevdef then
		if definfoDefinesAreEqual(prevdef, definfo) = FALSE then
			'' TODO: should only report once per symbol (per fbfrog run, not cpp run)
			print "conflicting #define " + *macro->text
		end if
	end if

	cppAddMacro(macro->text, definfo)

	'' Normally, we preserve #define directives (unlike the other CPP directives),
	'' thus no generic tkSetRemove() here. Unless the symbol was registed for removal.
	if hShouldRemoveDefine(macro->text) = FALSE then
		flags and= not TKFLAG_REMOVE
	end if
end sub

private sub cppUndef(byref flags as integer)
	cpp.x += 1

	assert(cppSkipping() = FALSE)

	'' Identifier
	if tkGet(cpp.x) < TK_ID then
		tkExpect(cpp.x, TK_ID, "behind #undef")
	end if
	var id = tkSpellId(cpp.x)
	cpp.x += 1

	if frog.verbose >= 2 then
		print "#undef " + *id
	end if

	cppAddKnownUndefined(id)

	cppEol()

	'' Ditto
	if hShouldRemoveDefine(id) = FALSE then
		flags and= not TKFLAG_REMOVE
	end if
end sub

private sub cppPragmaPushPopMacro(byval is_push as integer)
	cpp.x += 1

	var whatfor = iif(is_push, _
		@"for #pragma push_macro(""..."")", _
		@"for #pragma pop_macro(""..."")")

	'' '('
	tkExpect(cpp.x, TK_LPAREN, whatfor)
	cpp.x += 1

	'' "..."
	tkExpect(cpp.x, TK_STRING, whatfor)
	var id = cppStringLiteral(TRUE)

	'' ')'
	tkExpect(cpp.x, TK_RPAREN, whatfor)
	cpp.x += 1

	if is_push then
		cppSaveMacro(id)
	else
		cppRestoreMacro(id)
	end if
end sub

private function cppPragma(byref flags as integer) as integer
	select case tkSpell(cpp.x)
	'' #pragma once
	case "once"
		cpp.x += 1

		var knownfile = cppGetFileContext()->knownfile
		if knownfile >= 0 then
			cpp.files[knownfile].pragmaonce = TRUE
		end if

	'' #pragma message("...")
	case "message"
		'' Ignore
		cpp.x = hSkipToEol(cpp.x)

	'' MSVC:
	'' #pragma comment(lib, "<library file name>")
	case "comment"
		cpp.x += 1

		'' '('
		tkExpect(cpp.x, TK_LPAREN, "for #pragma comment(...)")
		cpp.x += 1

		select case tkSpell(cpp.x)
		case "lib"
			cpp.x += 1

			'' ','
			tkExpect(cpp.x, TK_COMMA, "for #pragma comment(lib, ""..."")")
			cpp.x += 1

			'' "..."
			tkExpect(cpp.x, TK_STRING, "for #pragma comment(lib, ""..."")")
			cpp.x += 1

			'' Preserve the #pragma comment(lib, "...") for the C parser
			flags and= not TKFLAG_REMOVE

		case else
			exit function
		end select

		'' ')'
		tkExpect(cpp.x, TK_RPAREN, "for #pragma comment(...)")
		cpp.x += 1

	case "GCC"
		cpp.x += 1

		select case tkSpell(cpp.x)
		case "system_header", "push_options", "pop_options", "reset_options", _
		     "optimize", "target", "visibility", "diagnostic"
			'' Ignore
			cpp.x = hSkipToEol(cpp.x)

		case else
			exit function
		end select

	case "clang"
		cpp.x += 1

		select case tkSpell(cpp.x)
		case "diagnostic"
			cpp.x = hSkipToEol(cpp.x)

		case else
			exit function
		end select

	case "warning"
		'' Ignore
		cpp.x = hSkipToEol(cpp.x)

	'' #pragma pack(N)
	'' #pragma pack()
	'' #pragma pack(push, N)
	'' #pragma pack(pop)
	case "pack"
		cpp.x += 1

		'' Just skip to EOL and let the C parser worry about checking
		'' the syntax
		cpp.x = hSkipToEol(cpp.x)

		'' Preserve the #pragma pack for the C parser
		flags and= not TKFLAG_REMOVE

	case "push_macro"
		cppPragmaPushPopMacro(TRUE)

	case "pop_macro"
		cppPragmaPushPopMacro(FALSE)

	case else
		exit function
	end select

	cppEol()
	function = TRUE
end function

private sub cppDirective()
	'' '#'
	var begin = cpp.x
	assert(tkGet(cpp.x) = TK_HASH)
	cpp.x += 1

	var directivekw = tkGet(cpp.x)

	'' When skipping, only #if/#elif/#else/#endif directives are handled,
	'' anything else (even invalid directives) must be ignored.
	if cppSkipping() then
		select case directivekw
		case KW_IF, KW_IFDEF, KW_IFNDEF, KW_ELIF, KW_ELSE, KW_ENDIF

		case else
			tkSetRemove(begin, cpp.x)
			cpp.x += 1
			exit sub
		end select
	end if

	'' Marking the '#' here already to get better error messages
	tkAddFlags(begin, begin, TKFLAG_STARTOFDIRECTIVE)

	var flags = TKFLAG_REMOVE or TKFLAG_DIRECTIVE

	select case directivekw
	case KW_IF
		cppIf()

	case KW_IFDEF, KW_IFNDEF
		cppIfdef(directivekw)

	case KW_ELIF
		cppElseIf()

	case KW_ELSE
		cppElse()

	case KW_ENDIF
		cppEndIf()

	case KW_INCLUDE
		cppInclude(begin, flags, FALSE)

	case KW_INCLUDE_NEXT
		cppInclude(begin, flags, TRUE)

	case KW_DEFINE
		cppDefine(flags)

	case KW_UNDEF
		cppUndef(flags)

	case KW_PRAGMA
		cpp.x += 1
		if cppPragma(flags) = FALSE then
			tkOops(cpp.x, "unknown #pragma")
		end if

	case KW_ERROR
		'' Not using the #error's text as error message,
		'' otherwise it would be mistaken for being generated by fbfrog.
		tkOops(cpp.x, "#error")

	case KW_WARNING
		cpp.x += 1
		'' ditto
		print tkReport(cpp.x, "#warning")
		cpp.x = hSkipToEol(cpp.x) + 1

	case TK_EOL
		'' '#' followed by EOL (accepted by gcc/clang too)
		cpp.x += 1

	case else
		tkOops(cpp.x, "unknown PP directive")
	end select

	if flags then
		tkAddFlags(begin, cpp.x - 1, flags)
	end if
end sub

private sub cppNext()
	select case tkGet(cpp.x)
	case TK_ENDINCLUDE
		cppEndInclude()
		exit sub

	'' '#'
	case TK_HASH
		'' Parse directive if at BOL and the '#' token isn't the result of a macro expansion
		'' We do this for every "toplevel" '#', before ever doing macro expansion behind it,
		'' so it should be safe to assume that if the '#' isn't coming from a macro expansion,
		'' the rest isn't either.
		if tkIsEolOrEof(cpp.x - 1) and tkIsOriginal(cpp.x) then
			cppDirective()
			exit sub
		end if

	'' _Pragma("...")
	case KW__PRAGMA
		if cppSkipping() = FALSE then
			var begin = cpp.x
			cpp.x += 1

			'' '('
			tkExpect(cpp.x, TK_LPAREN, "behind _Pragma")
			cpp.x += 1

			'' StringLiteral
			tkExpect(cpp.x, TK_STRING, "inside _Pragma()")
			var text = cppStringLiteral(TRUE)

			'' ')'
			tkExpect(cpp.x, TK_RPAREN, "to close _Pragma")
			cpp.x += 1

			'' Insert #pragma corresponding to the _Pragma(),
			'' while ensuring to have EOL in front of and behind it,
			'' mark the _Pragma() for removal, then we can parse the
			'' #pragma as usual.
			tkSetRemove(begin, cpp.x - 1)
			var pragma = !"\n#pragma " + text
			if tkGet(cpp.x) <> TK_EOL then
				pragma += !"\n"
			end if
			lexLoadC(cpp.x, pragma, sourceinfoForZstring("_Pragma(" + text + ")"))
			exit sub
		end if

	'' Identifier/keyword? Check whether it needs to be macro-expanded
	case is >= TK_ID
		if cppSkipping() = FALSE then
			if hMaybeExpandMacro(cpp.x, FALSE, TRUE) = FALSE then
				'' TK_ID not expanded - skip it (otherwise, we have to reparse it)
				cpp.x += 1
			end if
			exit sub
		end if

	'' Remove standalone EOLs, so the C parser doesn't have to handle them
	case TK_EOL
		tkSetRemove(cpp.x)
		cpp.x += 1
		exit sub
	end select

	'' Some token that doesn't matter to the CPP
	if cppSkipping() then
		tkSetRemove(cpp.x)
	end if
	cpp.x += 1
end sub

sub cppMain()
	while tkGet(cpp.x) <> TK_EOF
		cppNext()
	wend
end sub

'' Move CPP directives (the ones preserved for C parsing - #defines and
'' #includes) out of C declarations, so the C parser can treat them as toplevel
'' declarations/statements too.
sub hMoveDirectivesOutOfConstructs()
	var x = 0
	do
		'' Skip any directives at begin of construct
		while tkIsDirective(x)
			x += 1
		wend

		if tkGet(x) = TK_EOF then
			exit do
		end if

		var nxt = hSkipConstruct(x, TRUE)

		'' Exclude directives at end of construct from the construct
		while tkIsDirective(nxt - 1)
			nxt -= 1
		wend
		assert(x < nxt)

		'' Handle directives inside this construct: Move them to the end
		'' and exclude them from the construct.
		var writepos = nxt
		while x < nxt
			if tkIsDirective(x) then
				'' Collect all directives in a row
				var y = x
				while tkIsDirective(y + 1)
					y += 1
				wend
				assert(tkGet(y) = TK_EOL)
				assert(y < nxt)

				'' Move from middle to the end (but behind previously moved
				'' directives, to preserve their order)
				tkCopy(writepos, x, y, -1)
				tkRemove(x, y)

				'' Update end-of-construct position as we're moving
				'' directives out of the current construct
				nxt -= y - x + 1
			else
				x += 1
			end if
		wend
	loop
end sub

private function removeEols(byval first as integer, byval last as integer) as integer
	var x = first
	while x <= last
		if tkGet(x) = TK_EOL then
			tkRemove(x, x)
			x -= 1
			last -= 1
		end if
		x += 1
	wend
	function = last
end function

sub hApplyReplacements()
	'' Lex all the C token "patterns", so we can use tkCTokenRangesAreEqual()
	'' Insert them at the front of the tk buffer, because
	''  * they have to go *somewhere*
	''  * then we can easily skip them when searching through the main tokens,
	''    without having to worry about confusing them with real constructs...
	''  * then inserting/removing tokens from the main part won't affect our
	''    offsets into the pattern part
	var x = 0
	for i as integer = 0 to cpp.api->replacementcount - 1
		var begin = x
		x = lexLoadC(x, cpp.api->replacements[i].fromcode, sourceinfoForZstring("C code pattern from replacements file"))

		'' But remove EOLs from the patterns, because we're going to match against tk buffer content
		'' after the CPP phase, i.e. which had its EOLs removed aswell (except for #directives)
		x = removeEols(begin, x)

		cpp.api->replacements[i].patternlen = x - begin
	next
	var xmainbegin = x

	'' Search & replace
	x = xmainbegin
	while tkGet(x) <> TK_EOF
		var nxt = hSkipConstruct(x, FALSE)

		'' Compare the construct's tokens against tokens of the C code
		'' pattern given in the replacements file.
		''  * comparing based on tokens, so whitespace doesn't matter

		'' For CPP directives, exclude the EOL from the comparison,
		'' because the C code patterns don't include the \n either.
		var last = nxt - 1
		assert(x <= last)
		if tkIsDirective(x) and (tkGet(last) = TK_EOL) then
			last -= 1
		end if

		var constructlen = last - x + 1
		var patternbegin = 0

		for i as integer = 0 to cpp.api->replacementcount - 1
			var replacement = cpp.api->replacements + i

			'' Does the construct match this replacement pattern?
			if constructlen = replacement->patternlen then
				if tkCTokenRangesAreEqual(x, patternbegin, constructlen) then
					'' Remove the construct
					var location = tkGetLocation(x)
					tkRemove(x, nxt - 1)

					'' The token(s) we insert must have a source location so we can
					'' check which .h file it belongs to later: giving it
					'' the location of the construct's first token.

					if replacement->tofb then
						'' Insert TK_FBCODE instead
						tkInsert(x, TK_FBCODE, replacement->tocode)
						tkSetLocation(x, location)
						nxt = x + 1
					else
						'' Insert C tokens instead
						nxt = lexLoadC(x, replacement->tocode, sourceinfoForZstring("C code from replacements file"))

						'' Remove EOLs, as done by the CPP
						scope
							var i = x
							while i < nxt

								if tkGet(i) = TK_EOL then
									tkRemove(i, i)
									i -= 1
									nxt -= 1
								end if

								i += 1
							wend
						end scope

						'' If it looks like we inserted a #directive, add an EOL at the end,
						'' and add the proper tk flags
						if tkGet(x) = TK_HASH then
							tkAddFlags(x, x, TKFLAG_STARTOFDIRECTIVE)
							tkInsert(nxt, TK_EOL)
							tkAddFlags(x, nxt, TKFLAG_DIRECTIVE)
							nxt += 1
						end if

						for i as integer = x to nxt - 1
							tkSetLocation(i, location)
						next
					end if

					exit for
				end if
			end if

			patternbegin += replacement->patternlen
		next

		x = nxt
	wend

	'' Remove patterns from the end of the tk buffer again
	tkRemove(0, xmainbegin - 1)
end sub
