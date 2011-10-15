#define NULL 0
#define FALSE 0
#define TRUE (-1)

'' assert() that doesn't require -g
#macro ASSUMING(test) _
	if ((test) = FALSE) then : _
		bugoops(#test, __FUNCTION__, __LINE__) : _
	end if
#endmacro

declare sub bugoops _
	( _
		byval test as zstring ptr, _
		byval funcname as zstring ptr, _
		byval linenum as integer _
	)
declare sub oops(byref message as string)
declare function xallocate(byval as ulong) as any ptr
declare function xcallocate(byval as ulong) as any ptr
declare function xreallocate(byval as any ptr, byval as ulong) as any ptr

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

declare sub frog_work()
declare sub frog_add_file(byref h as string, byref bi as string)
declare sub frog_init()
declare sub frog_end()

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

type ListNode
	as ListNode ptr next
	as ListNode ptr prev
end type

type LinkedList
	as ListNode ptr head
	as ListNode ptr tail
	as integer nodesize
end type

declare function list_head(byval l as LinkedList ptr) as any ptr
declare function list_next(byval p as any ptr) as any ptr
declare function list_append(byval l as LinkedList ptr) as any ptr
declare sub list_init(byval l as LinkedList ptr, byval unit as integer)
declare sub list_end(byval l as LinkedList ptr)

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

'' The hash table is an array of these hash items, which associate a string to
'' some user data (always an array index in our case).
type HashItem
	as ubyte ptr s
	as integer length
	as uinteger hash        '' Hash value for quick comparison
	as integer data         '' User data
end type

type HashTable
	as HashItem ptr items  '' The table
	as integer count        '' Used
	as integer room         '' Allocated
end type

declare function hash_hash(byval s as ubyte ptr, byval length as integer) as uinteger
declare function hash_lookup _
	( _
		byval h as HashTable ptr, _
		byval s as ubyte ptr, _
		byval length as integer, _
		byval hash as uinteger _
	) as HashItem ptr
declare sub hash_add _
	( _
		byval h as HashTable ptr, _
		byval s as zstring ptr, _
		byval dat as integer _
	)
declare sub hash_init(byval h as HashTable ptr, byval exponent as integer)
declare sub hash_end(byval h as HashTable ptr)

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

'' When changing the enum all token tables/lists in tk*.bas must be updated too
enum
	TK_EOF = 0
	TK_TODO         '' TODOs added as fix-me-markers
	TK_BYTE         '' For stray bytes that don't fit in elsewhere
	TK_EOL
	TK_SPACE        '' Concatenated spaces/tabs
	TK_COMMENT      '' /* ... */
	TK_LINECOMMENT  '' // ...

	'' Number literals
	TK_DECNUM
	TK_HEXNUM
	TK_OCTNUM

	'' String literals, sorted to match STRFLAG_*:
	TK_STRING       '' 000    "foo"
	TK_CHAR         '' 001    'c'
	TK_WSTRING      '' 010    L"foo"
	TK_WCHAR        '' 011    L'c'
	TK_ESTRING      '' 100    "\n"
	TK_ECHAR        '' 101    '\n'
	TK_EWSTRING     '' 110    L"\n"
	TK_EWCHAR       '' 111    L'\n'

	'' C tokens
	TK_LOGNOT       '' !
	TK_NE           '' !=
	TK_HASH         '' #
	TK_MERGE        '' ##
	TK_MOD          '' %
	TK_SELFMOD      '' %=
	TK_BITAND       '' &
	TK_SELFBITAND   '' &=
	TK_LOGAND       '' &&
	TK_LPAREN       '' (
	TK_RPAREN       '' )
	TK_MUL          '' *
	TK_SELFMUL      '' *=
	TK_ADD          '' +
	TK_SELFADD      '' +=
	TK_INCREMENT    '' ++
	TK_COMMA        '' ,
	TK_SUB          '' -
	TK_SELFSUB      '' -=
	TK_DECREMENT    '' --
	TK_FIELDDEREF   '' ->
	TK_DOT          '' .
	TK_ELLIPSIS     '' ...
	TK_DIV          '' /
	TK_SELFDIV      '' /=
	TK_COLON        '' :
	TK_SEMI         '' ;
	TK_LT           '' <
	TK_SHL          '' <<
	TK_SELFSHL      '' <<=
	TK_LE           '' <=
	TK_ASSIGN       '' =
	TK_EQ           '' ==
	TK_GT           '' >
	TK_SHR          '' >>
	TK_SELFSHR      '' >>=
	TK_GE           '' >=
	TK_QUESTION     '' ?
	TK_LBRACKET     '' [
	TK_BACKSLASH    '' \
	TK_RBRACKET     '' ]
	TK_BITXOR       '' ^
	TK_SELFBITXOR   '' ^=
	TK_UNDERSCORE   '' _ (only for FB code, not used by the C lexer)
	TK_LBRACE       '' {
	TK_BITOR        '' |
	TK_SELFBITOR    '' |=
	TK_LOGOR        '' ||
	TK_RBRACE       '' }
	TK_BITNOT       '' ~

	'' >= TK_ID: keywords/identifiers
	TK_ID           '' Identifiers (a-z, A-Z, 0-9, _, $)

	'' C Keywords (some also used by FB)
	KW__C_FIRST
	KW_AUTO = KW__C_FIRST
	KW_BREAK
	KW_CASE
	KW_CHAR
	KW_CONST
	KW_CONTINUE
	KW_DEFAULT
	KW_DEFINE
	KW_DEFINED
	KW_DO
	KW_DOUBLE
	KW_ELIF
	KW_ELSE
	KW_ENDIF
	KW_ENUM
	KW_EXTERN
	KW_FLOAT
	KW_FOR
	KW_GOTO
	KW_IF
	KW_IFDEF
	KW_IFNDEF
	KW_INCLUDE
	KW_INLINE
	KW_INT
	KW_LONG
	KW_PRAGMA
	KW_REGISTER
	KW_RESTRICT
	KW_RETURN
	KW_SHORT
	KW_SIGNED
	KW_SIZEOF
	KW_STATIC
	KW_STRUCT
	KW_SWITCH
	KW_TYPEDEF
	KW_UNDEF
	KW_UNION
	KW_UNSIGNED
	KW_VOID
	KW_VOLATILE
	KW_WHILE

	'' FB only keywords
	'' Here we only need to have FB keywords that we might produce during
	'' the C-to-FB translation.
	'' Note: The lexer doesn't recognize/use these, they're only added
	'' during the translation process.
	KW__FB_FIRST
	KW_ALIAS = KW__FB_FIRST
	KW_ANY
	KW_AS
	KW_BYTE
	KW_BYVAL
	KW_CAST
	KW_CDECL
	KW_CPTR
	KW_DECLARE
	KW_DIM
	KW_ELSEIF
	KW_END
	KW_EXIT
	KW_EXPORT
	KW_FIELD
	KW_FUNCTION
	KW_IIF
	KW_INTEGER
	KW_LONGINT
	KW_LOOP
	KW_NEXT
	KW_PASCAL
	KW_PRIVATE
	KW_PTR
	KW_SCOPE
	KW_SELECT
	KW_SHARED
	KW_SINGLE
	KW_STDCALL
	KW_SUB
	KW_THEN
	KW_TO
	KW_TYPE
	KW_UBYTE
	KW_UINTEGER
	KW_ULONG
	KW_ULONGINT
	KW_USHORT
	KW_WEND
	KW_WSTR
	KW_WSTRING
	KW_ZSTRING

	TK__COUNT
end enum

'' Note: When updating, update the debugging code too!
enum
	MARK_TOPLEVEL = 0
	MARK_PP        '' Some PP directive
	MARK_EXTERN    '' EXTERN "C" block
	MARK_ENDEXTERN '' To identify '}'
	MARK_STRUCT    '' [typedef to] struct/union/enum block
	MARK_ENDENUM   '' To identify '}'
	MARK_ENDSTRUCT '' To identify '}'
	MARK_ENDUNION  '' To identify '}'
	MARK_ENUMCONST '' Enum constants
	MARK_TYPEDEF   '' Typedefs (though not those with struct/etc blocks)
	MARK_TOPDECL   '' Toplevel declarations, after parsing, before split up into vardecls/procdecls
	MARK_PROCDECL  '' procdecls after being split from topdecls
	MARK_VARDECL   '' vardecls after being split from topdecls
	MARK_FIELDDECL '' Fields
	MARK__COUNT
end enum

extern as zstring ptr token_text(0 to (TK__COUNT - 1))
extern as zstring ptr mark_text(0 to (MARK__COUNT - 1))

'' Great debugging helper, for example: TRACE(x), "decl begin"
#define TRACE(x) print lcase(__FUNCTION__) & "(" & __LINE__ & "):" & _
	x & " " & *mark_text(tk_mark(x)) & "[" & *token_text(tk_get(x)) & "]"

declare sub tk_insert _
	( _
		byval x as integer, _
		byval id as integer, _
		byval text as zstring ptr _
	)
declare sub tk_insert_space(byval x as integer)
declare sub tk_copy _
	( _
		byval x as integer, _
		byval first as integer, _
		byval last as integer _
	)
declare sub tk_remove(byval first as integer, byval last as integer)
declare sub tk_set_mark _
	( _
		byval mark as integer, _
		byval first as integer, _
		byval last as integer _
	)
declare function tk_get(byval x as integer) as integer
declare function tk_text(byval x as integer) as zstring ptr
declare function tk_mark(byval x as integer) as integer
declare function tk_debug_text(byval x as integer) as string
declare sub tk_init()
declare sub tk_end()
declare sub tk_insert_file(byval x as integer, byref filename as string)
declare sub tk_emit_file(byref filename as string)
