''
'' Partial C pre-processor for selective macro expansion inside #define bodies
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

#include once "c-pp.bi"
#include once "c-common.bi"
#include once "c-lex.bi"
#include once "fbfrog.bi"
#include once "util-path.bi"

#include once "crt.bi"
#include once "file.bi"

using tktokens

destructor DefineInfo()
	astDelete(macro)
end destructor

function DefineInfo.clone() as DefineInfo ptr
	var b = new DefineInfo
	b->xbody   = xbody
	b->xeol    = xeol
	b->macro   = astClone(macro)
	function = b
end function

const DEFINEBODY_FLAGMASK = not (TKFLAG_REMOVE or TKFLAG_DIRECTIVE)

'' Copy a #define body into some other place
sub DefineInfo.copyBody(byref tk as TokenBuffer, byval x as integer)
	assert(x > xeol)
	tk.copy(x, xbody, xeol - 1, DEFINEBODY_FLAGMASK)
end sub

'' Compare two #defines and determine whether they are equal
function DefineInfo.equals(byref tk as TokenBuffer, byval b as DefineInfo ptr) as integer
	'' Check name, parameters and body
	return astIsEqual(macro, b->macro) andalso tk.spell(xbody, xeol) = tk.spell(b->xbody, b->xeol)
end function

constructor CppContext(byref sourcectx as SourceContext, byref tk as TokenBuffer, byref options as BindingOptions)
	this.sourcectx = @sourcectx
	this.tk = @tk
	this.options = @options
	x = 0
end constructor

destructor CppContext()
	for i as integer = 0 to macros.room - 1
		delete cptr(DefineInfo ptr, macros.items[i].data)
	next
end destructor

function CppContext.lookupMacro(byval id as zstring ptr) as DefineInfo ptr
	function = macros.lookupDataOrNull(id)
end function

'' Add/overwrite a known macro definition (or register it as known undefined)
sub CppContext.addMacro(byval id as zstring ptr, byval definfo as DefineInfo ptr)
	var hash = hashHash(id)
	var item = macros.lookup(id, hash)
	if item->s then
		delete cptr(DefineInfo ptr, item->data)
		item->data = definfo
	else
		macros.add(item, hash, id, definfo)
	end if
end sub

sub CppContext.addKnownUndefined(byval id as zstring ptr)
	addMacro(id, NULL)
end sub

function CppContext.checkForMacroCall(byval y as integer) as DefineInfo ptr
	assert(tk->get(y) >= TK_ID)
	var id = tk->spellId(y)

	'' Is this id a macro?
	var definfo = lookupMacro(id)
	if definfo = NULL then
		return NULL
	end if

	'' Only expand if not marked otherwise
	if options->idopt(OPT_NOEXPAND).matches(id) or _
	   (tk->getFlags(y) and TKFLAG_NOEXPAND) or _
	   (definfo->macro->attrib and ASTATTRIB_POISONED) then
		return NULL
	end if

	function = definfo
end function

private function hSkipEols(byref tk as TokenBuffer, byval x as integer, byval delta as integer) as integer
	while tk.get(x) = TK_EOL
		x += delta
	wend
	function = x
end function

'' Set or unset the BEHINDSPACE flag of a token
private sub hOverrideBehindspace(byref tk as TokenBuffer, byval x as integer, byval flag as integer)
	tk.setFlags(x, (tk.getFlags(x) and (not TKFLAG_BEHINDSPACE)) or flag)
end sub

const MAXARGS = 128

private sub hParseMacroCallArgs _
	( _
		byref tk as TokenBuffer, _
		byref x as integer, _
		byval macro as AstNode ptr, _
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
			tk.showErrorAndAbort(x, "macro call arg buffer too small, MAXARGS=" & MAXARGS)
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
			select case tk.get(x)
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
				tk.oopsExpected(x, "')' to close macro call argument list")
			end select

			x += 1
		loop

		argend[argcount] = x - 1
		argcount += 1

		'' ','?
		if tk.get(x) <> TK_COMMA then
			exit do
		end if
		x += 1
	loop

	'' It's ok to omit the arg(s) for the variadic parameter of a variadic macro.
	if is_variadic and (not reached_lastarg) then
		if argcount >= MAXARGS then
			tk.showErrorAndAbort(x, "macro call arg buffer too small, MAXARGS=" & MAXARGS)
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
		tk.showErrorAndAbort(x, s)
	end if

	'' Cut EOL/space in front of and behind macro call args
	for i as integer = 0 to argcount - 1
		var begin_had_eol = (argbegin[i] = TK_EOL)
		argbegin[i] = hSkipEols(tk, argbegin[i], 1)
		argend[i] = hSkipEols(tk, argend[i], -1)
		if argbegin[i] < argend[i] then
			hOverrideBehindspace(tk, argbegin[i], 0)
		end if
	next

	'' If arg contains EOL, mark following token as behindspace
	'' (the whole macro arg is considered to be a single line, but we can't delete tokens from the
	'' buffer here)
	for i as integer = 0 to argcount - 1
		for j as integer = argbegin[i] to argend[i]
			if tk.get(j) = TK_EOL then
				var k = j + 1
				if k <= argend[i] then
					tk.setFlags(k, tk.getFlags(k) or TKFLAG_BEHINDSPACE)
				end if
			end if
		next
	next
end sub

private function hParseMacroCall _
	( _
		byref tk as TokenBuffer, _
		byval x as integer, _
		byval macro as AstNode ptr, _
		byval argbegin as integer ptr, _
		byval argend as integer ptr, _
		byref argcount as integer _
	) as integer

	var begin = x

	'' ID
	assert(tk.get(x) >= TK_ID)
	x += 1

	argcount = -1

	'' Not just "#define m"?
	if macro->paramcount >= 0 then
		'' '('?
		if tk.get(x) <> TK_LPAREN then
			return -1
		end if
		x += 1

		argcount = 0

		'' Not just "#define m()"?
		if macro->paramcount > 0 then
			'' Parse the argument list and fill the argbegin() and
			'' argend() arrays accordingly
			hParseMacroCallArgs(tk, x, macro, argbegin, argend, argcount)
		end if

		'' ')'?
		tk.expect(x, TK_RPAREN, "to close macro call argument list")
		x += 1
	end if

	function = x - 1
end function

'' DEFINED ['('] Identifier [')']
private sub hSkipDefinedUop(byref tk as TokenBuffer, byref x as integer)
	assert(tk.get(x) = KW_DEFINED)
	x += 1

	'' '('?
	var have_lparen = FALSE
	if tk.get(x) = TK_LPAREN then
		have_lparen = TRUE
		x += 1
	end if

	'' Identifier? (not doing any expansion here)
	if tk.get(x) >= TK_ID then
		x += 1
	end if

	'' ')'?
	if have_lparen then
		if tk.get(x) = TK_RPAREN then
			x += 1
		end if
	end if
end sub

private sub hWrapInTkBeginEnd(byref tk as TokenBuffer, byval first as integer, byval last as integer)
	assert(first <= last)
	tk.insert(first, TK_BEGIN)
	last += 1
	tk.insert(last + 1, TK_END)
end sub

private sub hUnwrapTkBeginEnd(byref tk as TokenBuffer, byval first as integer, byval last as integer)
	assert(tk.get(first) = TK_BEGIN)
	assert(tk.get(last) = TK_END)
	tk.remove(first, first)
	last -= 1
	tk.remove(last, last)
end sub

function CppContext.expandInTkBeginEnd(byval y as integer, byval inside_ifexpr as integer) as integer
	assert(tk->get(y) = TK_BEGIN)

	do
		select case tk->get(y)
		case TK_END
			exit do

		case KW_DEFINED
			'' If inside an #if condition expression, don't expand symbols behind the defined operator.
			'' According to the C standard, the handling of defined's that result from macro expansion
			'' is undefined, but gcc handles them as normal defined's, so we do too.
			if inside_ifexpr then
				hSkipDefinedUop(*tk, y)
				y -= 1
			end if

		case is >= TK_ID
			if maybeExpandMacro(y, inside_ifexpr, TRUE) then
				'' TK_ID replaced by macro body - reparse
				y -= 1
			end if
		end select

		y += 1
	loop

	function = y
end function

function CppContext.expandInRange _
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
	hWrapInTkBeginEnd(*tk, first, last)
	last += 2
	assert(tk->get(last) = TK_END)

	'' Expand anything in the range
	last = expandInTkBeginEnd(first, inside_ifexpr)

	'' Remove TK_BEGIN/TK_END again
	hUnwrapTkBeginEnd(*tk, first, last)
	last -= 2

	function = last
end function

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
function CppContext.insertMacroExpansion _
	( _
		byval callbehindspace as integer, _
		byval expansionbegin as integer, _
		byref definfo as DefineInfo, _
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
	tk->insert(expansionbegin, TK_END)
	definfo.copyBody(*tk, expansionbegin)
	tk->insert(expansionbegin, TK_BEGIN)

	'' Update the BEHINDSPACE status of the first token in the expansion to
	'' be the same as that of the macro name which we're expanding
	hOverrideBehindspace(*tk, expansionbegin + 1, callbehindspace)

	'' Solve #stringify operators (higher priority than ##, and no macro
	'' expansion done for the arg)
	var y = expansionbegin + 1
	while tk->get(y) <> TK_END

		'' '#param'?
		if tk->get(y) = TK_HASH then
			'' Followed by identifier?
			if tk->get(y + 1) >= TK_ID then
				'' Is it a macro parameter?
				var arg = astLookupMacroParam(definfo.macro, tk->spellId(y + 1))
				if arg >= 0 then
					'' Remove #param, and insert stringify result instead
					'' but preserve BEHINDSPACE status.
					assert((arg >= 0) and (arg < argcount))
					var behindspace = tk->getFlags(y) and TKFLAG_BEHINDSPACE
					tk->remove(y, y + 1)

					'' " must be replaced by \"
					'' then we can wrap the stringified text in "..." to produce the TK_STRING
					var s = tk->spell(argbegin[arg], argend[arg])
					s = strReplace(s, """", $"\""")
					s = """" + s + """"

					tk->insert(y, TK_STRING, s)
					hOverrideBehindspace(*tk, y, behindspace)
				end if
			end if
		end if

		y += 1
	wend

	'' Replace ## tokens by special internal merge operator tokens, so that
	'' ## tokens from macro arguments aren't mistaken for merge operators.
	y = expansionbegin + 1
	while tk->get(y) <> TK_END

		'' '##'?
		if tk->get(y) = TK_HASHHASH then
			tk->insert(y, TK_PPMERGE)
			var z = y + 1
			tk->setLocation(y, tk->getLocation(z))
			tk->remove(z, z)
		end if

		y += 1
	wend

	'' Insert args into params, surrounded with TK_ARGBEGIN/END, so that
	'' - we know when an arg was empty when doing ## merging (to avoid
	''   merging with other tokens outside the arg),
	'' - we know the arg's boundaries for macro-expanding it later. (must be
	''   done after merging, because only the unmerged tokens of an arg
	''   shall be macro-expanded, and not the ones involved in merging)
	y = expansionbegin + 1
	while tk->get(y) <> TK_END

		'' Macro parameter?
		if tk->get(y) >= TK_ID then
			var arg = astLookupMacroParam(definfo.macro, tk->spellId(y))
			if arg >= 0 then
				'' >= TK_ID
				var behindspace = tk->getFlags(y) and TKFLAG_BEHINDSPACE
				tk->remove(y, y)

				'' TK_ARGBEGIN
				tk->insert(y, TK_ARGBEGIN)
				y += 1

				'' arg's tokens
				tk->copy(y, argbegin[arg], argend[arg], DEFINEBODY_FLAGMASK)
				hOverrideBehindspace(*tk, y, behindspace)
				y += argend[arg] - argbegin[arg] + 1

				'' TK_ARGEND
				tk->insert(y, TK_ARGEND)
			end if
		end if

		y += 1
	wend

	''
	'' Do '##' merging
	''
	'' It's not clear how <a ## ## b> or <a ## b ## c> should be processed
	'' (undefined behaviour), so fbfrog shows an error about the first
	'' (cannot merge a and ##) and processes the 2nd as (a##b)##c, i.e.
	'' left-associative.
	''
	y = expansionbegin + 1
	while tk->get(y) <> TK_END

		'' '##' from original macro body (and not '##' from a macro argument)?
		if tk->get(y) = TK_PPMERGE then

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
			if (tk->get(y - 1) = TK_ARGEND) and (tk->get(y - 2) <> TK_ARGBEGIN)  then
				tk->remove(y - 1, y - 1)
				tk->insert(y - 2, TK_ARGEND)
				assert(tk->get(y) = TK_PPMERGE)
				assert(tk->get(y - 1) <> TK_ARGEND)
				assert(tk->get(y - 2) = TK_ARGEND)

				'' Empty now? Then remove the TK_ARGBEGIN/END
				if tk->get(y - 3) = TK_ARGBEGIN then
					tk->remove(y - 3, y - 2)
					y -= 2
				end if
			end if

			'' rhs was a non-empty arg?
			if (tk->get(y + 1) = TK_ARGBEGIN) and (tk->get(y + 2) <> TK_ARGEND) then
				tk->remove(y + 1, y + 1)
				tk->insert(y + 2, TK_ARGBEGIN)
				assert(tk->get(y) = TK_PPMERGE)
				assert(tk->get(y + 1) <> TK_ARGBEGIN)
				assert(tk->get(y + 2) = TK_ARGBEGIN)

				'' Empty now? Then remove the TK_ARGBEGIN/END
				if tk->get(y + 3) = TK_ARGEND then
					tk->remove(y + 2, y + 3)
				end if
			end if

			assert(tk->get(y) = TK_PPMERGE)
			var l = y - 1
			var r = y + 1

			'' If one operand was an empty arg, then no merging needs to be done,
			'' the other operand can just be preserved as-is; or in case both were
			'' empty, the ## just disappears.

			'' Non-empty on both sides?
			if (tk->get(l) <> TK_ARGEND) and (tk->get(r) <> TK_ARGBEGIN) then
				if tk->get(l) = TK_BEGIN then
					tk->showErrorAndAbort(y, "## merge operator at beginning of macro body, missing operand to merge with")
				end if
				if tk->get(r) = TK_END then
					tk->showErrorAndAbort(y, "## merge operator at end of macro body, missing operand to merge with")
				end if

				'' Combine the original text representation of both tokens,
				'' and prepend a space if the lhs was BEHINDSPACE, such that
				'' the merged token will also be BEHINDSPACE.
				dim mergetext as string
				if tk->getFlags(l) and TKFLAG_BEHINDSPACE then
					mergetext += " "
				end if
				mergetext += tk->spell(l) + tk->spell(r)

				'' and try to lex them
				var z = tk->count()
				lexLoadC(*tk, z, mergetext, sourcectx->lookupOrMakeSourceInfo("## merge operation", FALSE))

				'' That should have produced only 1 token. If it produced more, then the merge failed.
				assert(tk->count() >= (z + 1))
				if tk->count() > (z + 1) then
					tk->remove(z, tk->count() - 1)
					tk->showErrorAndAbort(y, "## merge operator cannot merge '" + tk->spell(y - 1) + "' and '" + tk->spell(y + 1) + "'")
				end if

				'' Remove the 3 (l ## r) tokens and insert the merged token in place of l
				tk->remove(l, r)
				z -= 3
				y = l

				tk->copy(y, z, z, DEFINEBODY_FLAGMASK)
				z += 1

				tk->remove(z, z)
			else
				'' Just remove the '##'
				tk->remove(y, y)
				y -= 1
			end if
		end if

		y += 1
	wend

	'' Recursively macro-expand the tokens in each TK_ARGBEGIN/END sequence,
	'' and then remove TK_ARGBEGIN/END.
	y = expansionbegin + 1
	while tk->get(y) <> TK_END

		'' Macro parameter?
		if tk->get(y) = TK_ARGBEGIN then
			var z = y
			do
				z += 1
			loop while tk->get(z) <> TK_ARGEND

			'' Macro-expand the arg's tokens
			z = expandInRange(y, z, inside_ifexpr)

			'' Remove TK_ARGBEGIN/END wrapping
			assert(tk->get(y) = TK_ARGBEGIN)
			tk->remove(y, y)
			y -= 1
			z -= 1
			assert(tk->get(z) = TK_ARGEND)
			tk->remove(z, z)
			z -= 1

			y = z
		end if

		y += 1
	wend

	'' Remove the TK_BEGIN/END wrapping around the expansion
	assert(tk->get(expansionbegin) = TK_BEGIN)
	tk->remove(expansionbegin, expansionbegin)
	y -= 1
	assert(tk->get(y) = TK_END)
	tk->remove(y, y)
	y -= 1

	function = y
end function

sub CppContext.expandMacro _
	( _
		byref definfo as DefineInfo, _
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
	var expansionend = insertMacroExpansion( _
			tk->getFlags(callbegin) and TKFLAG_BEHINDSPACE, _
			expansionbegin, definfo, argbegin, argend, argcount, inside_ifexpr)

	'' Mark expansion tokens
	tk->addFlags(expansionbegin, expansionend, TKFLAG_EXPANSION)

	if expand_recursively then
		'' Recursively do macro expansion in the expansion
		'' - Marking the current macro as poisoned, so it won't be expanded
		''   again within the expansion, preventing expansion of complete
		''   recursive calls.
		'' - Incomplete recursive calls need to be marked with NOEXPAND so they
		''   won't be expanded later when they become complete by taking into
		''   account tokens following behind the expansion.
		definfo.macro->attrib or= ASTATTRIB_POISONED
		expansionend = expandInRange(expansionbegin, expansionend, inside_ifexpr)
		definfo.macro->attrib and= not ASTATTRIB_POISONED
	end if

	'' Disable future expansion of recursive macro calls to this macro
	'' (those that weren't expanded due to the "poisoning")
	scope
		var y = expansionbegin
		while y <= expansionend

			if tk->get(y) >= TK_ID then
				'' Known macro, and it's the same as this one?
				var calldefinfo = checkForMacroCall(y)
				if calldefinfo = @definfo then
					'' Can the macro call be parsed successfully,
					'' and is it fully within the expansion?
					dim as integer argbegin(0 to MAXARGS-1)
					dim as integer argend(0 to MAXARGS-1)
					dim as integer argcount
					var callend = hParseMacroCall(*tk, y, definfo.macro, @argbegin(0), @argend(0), argcount)
					if (callend >= 0) and (callend <= expansionend) then
						tk->addFlags(y, y, TKFLAG_NOEXPAND)
					end if
				end if
			end if

			y += 1
		wend
	end scope

	'' Update locations on the expansion tokens to point to the macro call,
	'' instead of the #define body etc.
	scope
		var y = expansionbegin
		while y <= expansionend
			tk->setLocation(y, tk->getLocation(callbegin))
			y += 1
		wend
	end scope

	'' Then remove the call tokens
	tk->remove(callbegin, callend)
end sub

function CppContext.maybeExpandMacro(byval y as integer, byval inside_ifexpr as integer, byval expand_recursively as integer) as integer
	var definfo = checkForMacroCall(y)
	if definfo = NULL then
		exit function
	end if

	dim as integer argbegin(0 to MAXARGS-1)
	dim as integer argend(0 to MAXARGS-1)
	dim as integer argcount

	'' Try to parse the macro call (can fail in case of function-like macro
	'' without argument list)
	var callbegin = y
	var callend = hParseMacroCall(*tk, callbegin, definfo->macro, @argbegin(0), @argend(0), argcount)
	if callend < 0 then
		exit function
	end if

	expandMacro(*definfo, callbegin, callend, @argbegin(0), @argend(0), argcount, inside_ifexpr, expand_recursively)
	function = TRUE
end function

sub CppContext.maybeExpandMacroInDefineBody(byval parentdefine as AstNode ptr)
	var id = tk->spellId(x)

	'' Only expand if the called macro was given with -expandindefine
	if options->idopt(OPT_EXPANDINDEFINE).matches(id) = FALSE then
		exit sub
	end if

	'' Similar to maybeExpandMacro():
	var definfo = checkForMacroCall(x)
	if definfo = NULL then
		exit sub
	end if

	dim as integer argbegin(0 to MAXARGS-1)
	dim as integer argend(0 to MAXARGS-1)
	dim as integer argcount
	var callbegin = x
	var callend = hParseMacroCall(*tk, callbegin, definfo->macro, @argbegin(0), @argend(0), argcount)
	if callend < 0 then
		exit sub
	end if

	'' Don't expand if the macrocall involves parameters of the parentdefine
	for i as integer = callbegin to callend
		if tk->get(i) >= TK_ID then
			if astLookupMacroParam(parentdefine, tk->spellId(i)) >= 0 then
				exit sub
			end if
		end if
	next

	expandMacro(*definfo, callbegin, callend, @argbegin(0), @argend(0), argcount, FALSE, FALSE)

	'' TK_ID expanded; reparse it
	x -= 1
end sub

function CppContext.shouldRemoveDefine(byval id as zstring ptr) as integer
	function = options->idopt(OPT_REMOVEDEFINE).matches(id)
end function

'' DEFINE Identifier ['(' ParameterList ')'] Body Eol
sub CppContext.parseDefine(byref flags as integer)
	x += 1

	'' Identifier ['(' ParameterList ')']
	var macro = hDefineHead(*tk, x)

	'' Body
	var xbody = x

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
	if options->idopt(OPT_EXPANDINDEFINE).nonEmpty then
		do
			select case tk->get(x)
			case TK_EOL
				exit do

			case is >= TK_ID
				maybeExpandMacroInDefineBody(macro)

			end select

			x += 1
		loop
	end if

	'' Eol
	var xeol = tk->skipToEol(xbody)
	assert(tk->get(xeol) = TK_EOL)
	x = xeol + 1

	var definfo = new DefineInfo
	definfo->xbody = xbody
	definfo->xeol = xeol
	definfo->macro = macro

	if frog.verbose >= 2 then
		print "#define " + *macro->text + " " + tk->spell(xbody, xeol)
	end if

	'' Report conflicting #defines
	var prevdef = lookupMacro(macro->text)
	if prevdef then
		if prevdef->equals(*tk, definfo) = FALSE then
			'' TODO: should only report once per symbol (per fbfrog run, not cpp run)
			print "conflicting #define " + *macro->text
		end if
	end if

	addMacro(macro->text, definfo)

	'' Normally, we preserve #define directives (unlike the other CPP directives),
	'' thus no generic tk->setRemove() here. Unless the symbol was registed for removal.
	if shouldRemoveDefine(macro->text) = FALSE then
		flags and= not TKFLAG_REMOVE
	end if
end sub

'' Move CPP directives (the ones preserved for C parsing - #defines and
'' #includes) out of C declarations, so the C parser can treat them as toplevel
'' declarations/statements too.
sub hMoveDirectivesOutOfConstructs(byref tk as TokenBuffer)
	var x = 0
	do
		'' Skip any directives at begin of construct
		while tk.isDirective(x)
			x += 1
		wend

		if tk.get(x) = TK_EOF then
			exit do
		end if

		var nxt = tk.skipConstruct(x, TRUE)

		'' Exclude directives at end of construct from the construct
		while tk.isDirective(nxt - 1)
			nxt -= 1
		wend
		assert(x < nxt)

		'' Handle directives inside this construct: Move them to the end
		'' and exclude them from the construct.
		var writepos = nxt
		while x < nxt
			if tk.isDirective(x) then
				'' Collect all directives in a row
				var y = x
				while tk.isDirective(y + 1)
					y += 1
				wend
				assert(tk.get(y) = TK_EOL)
				assert(y < nxt)

				'' Move from middle to the end (but behind previously moved
				'' directives, to preserve their order)
				tk.copy(writepos, x, y, -1)
				tk.remove(x, y)

				'' Update end-of-construct position as we're moving
				'' directives out of the current construct
				nxt -= y - x + 1
			else
				x += 1
			end if
		wend
	loop
end sub

private function removeEols(byref tk as TokenBuffer, byval first as integer, byval last as integer) as integer
	var x = first
	while x <= last
		if tk.get(x) = TK_EOL then
			tk.remove(x, x)
			x -= 1
			last -= 1
		end if
		x += 1
	wend
	function = last
end function

sub hApplyReplacements(byref sourcectx as SourceContext, byref tk as TokenBuffer, byref options as BindingOptions)
	'' Lex all the C token "patterns", so we can use tk.areCTokenRangesEqual()
	'' Insert them at the front of the tk buffer, because
	''  * they have to go *somewhere*
	''  * then we can easily skip them when searching through the main tokens,
	''    without having to worry about confusing them with real constructs...
	''  * then inserting/removing tokens from the main part won't affect our
	''    offsets into the pattern part
	var x = 0
	for i as integer = 0 to options.replacementcount - 1
		var begin = x
		x = lexLoadC(tk, x, options.replacements[i].fromcode, sourcectx.lookupOrMakeSourceInfo("C code pattern from replacements file", FALSE))

		'' But remove EOLs from the patterns, because we're going to match against tk buffer content
		'' after the CPP phase, i.e. which had its EOLs removed aswell (except for #directives)
		x = removeEols(tk, begin, x)

		options.replacements[i].patternlen = x - begin
	next
	var xmainbegin = x

	'' Search & replace
	x = xmainbegin
	while tk.get(x) <> TK_EOF
		var nxt = tk.skipConstruct(x, FALSE)

		'' Compare the construct's tokens against tokens of the C code
		'' pattern given in the replacements file.
		''  * comparing based on tokens, so whitespace doesn't matter

		'' For CPP directives, exclude the EOL from the comparison,
		'' because the C code patterns don't include the \n either.
		var last = nxt - 1
		assert(x <= last)
		if tk.isDirective(x) and (tk.get(last) = TK_EOL) then
			last -= 1
		end if

		var constructlen = last - x + 1
		var patternbegin = 0

		for i as integer = 0 to options.replacementcount - 1
			var replacement = options.replacements + i

			'' Does the construct match this replacement pattern?
			if constructlen = replacement->patternlen then
				if tk.areCTokenRangesEqual(x, patternbegin, constructlen) then
					'' Remove the construct
					var location = tk.getLocation(x)
					tk.remove(x, nxt - 1)

					'' The token(s) we insert must have a source location so we can
					'' check which .h file it belongs to later: giving it
					'' the location of the construct's first token.

					if replacement->tofb then
						'' Insert TK_FBCODE instead
						tk.insert(x, TK_FBCODE, replacement->tocode)
						tk.setLocation(x, location)
						nxt = x + 1
					else
						'' Insert C tokens instead
						nxt = lexLoadC(tk, x, replacement->tocode, sourcectx.lookupOrMakeSourceInfo("C code from replacements file", FALSE))

						'' Remove EOLs, as done by the CPP
						scope
							var i = x
							while i < nxt

								if tk.get(i) = TK_EOL then
									tk.remove(i, i)
									i -= 1
									nxt -= 1
								end if

								i += 1
							wend
						end scope

						'' If it looks like we inserted a #directive, add an EOL at the end,
						'' and add the proper tk flags
						if tk.get(x) = TK_HASH then
							tk.addFlags(x, x, TKFLAG_STARTOFDIRECTIVE)
							tk.insert(nxt, TK_EOL)
							tk.addFlags(x, nxt, TKFLAG_DIRECTIVE)
							nxt += 1
						end if

						for i as integer = x to nxt - 1
							tk.setLocation(i, location)
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
	tk.remove(0, xmainbegin - 1)
end sub
