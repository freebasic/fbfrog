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
	TK_QUEST        '' ?
	TK_LBRACKET     '' [
	TK_BACKSLASH    '' \
	TK_RBRACKET     '' ]
	TK_BITXOR       '' ^
	TK_SELFBITXOR   '' ^=
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
	KW__C_COUNT

	'' FB only keywords
	'' Here we only need to have FB keywords that we might produce during
	'' the C-to-FB translation.
	KW__FB_FIRST = KW__C_COUNT
	KW_ALIAS = KW__FB_FIRST
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
	KW__FB_COUNT

	TK__COUNT = KW__FB_COUNT
end enum

declare sub tk_move(byval delta as integer)
declare sub tk_move_to(byval x as integer)
declare sub tk_in(byval id as integer, byval text as zstring ptr)
declare sub tk_in_raw _
	( _
		byval id as integer, _
		byval text as ubyte ptr, _
		byval length as integer _
	)
declare sub tk_out()
declare sub tk_drop_all()
declare sub tk_init()
declare sub tk_end()
declare function tk_get(byval x as integer) as integer
declare function tk_text(byval x as integer) as zstring ptr
declare function tk_count() as integer
declare sub tk_count_input_size(byval n as integer)
declare sub tk_count_input_token()

declare sub tk_emit_file(byref filename as string)
declare sub tk_in_file(byref filename as string)
