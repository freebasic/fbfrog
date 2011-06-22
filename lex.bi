'' When changing, update the keywords/tokens tables
enum
	TK_EOF      = 0
	TK_EOL

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
	TK_MINUS        '' -
	TK_SELFMINUS    '' -=
	TK_DECREMENT    '' --
	TK_FIELDDEREF   '' ->
	TK_DOT          '' .
	TK_ELLIPSIS     '' ...
	TK_DIV          '' /
	TK_SELFDIV      '' /=
	TK_LINECOMMENT  '' //
	TK_MULTICOMMENT '' /* ... */
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
	TK_IIF          '' ?
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

	TK_NUMLIT       '' Number literals
	TK_STRLIT       '' Strings
	TK_CHRLIT       '' Chars
	TK_ID           '' Identifiers (a-z, A-Z, 0-9, _, $)

	''
	'' Keywords
	'' Careful: keep in sync with the keyword table in lex.bas!
	''
	TK__FIRSTKW

	TK_BOOL = TK__FIRSTKW
	TK_COMPLEX
	TK_IMAGINARY
	TK_AUTO
	TK_BREAK
	TK_CASE
	TK_CHAR
	TK_CONST
	TK_CONTINUE
	TK_DEFAULT
	TK_DEFINE
	TK_DEFINED
	TK_DO
	TK_DOUBLE
	TK_ELSE
	TK_ELIF
	TK_ENDIF
	TK_ENUM
	TK_EXTERN
	TK_FLOAT
	TK_FOR
	TK_GOTO
	TK_IF
	TK_IFDEF
	TK_IFNDEF
	TK_INCLUDE
	TK_INLINE
	TK_INT
	TK_LONG
	TK_PRAGMA
	TK_REGISTER
	TK_RESTRICT
	TK_RETURN
	TK_SHORT
	TK_SIGNED
	TK_SIZEOF
	TK_STATIC
	TK_STRUCT
	TK_SWITCH
	TK_TYPEDEF
	TK_UNDEF
	TK_UNION
	TK_UNSIGNED
	TK_VOID
	TK_VOLATILE
	TK_WHILE

	TK__LASTKW = TK_WHILE

	TK__KWCOUNT = TK__LASTKW - TK__FIRSTKW + 1
end enum

declare sub lex_oops(byref message as string)
declare function lex_at_line_begin() as integer
declare function lex_tk() as integer
declare function lex_text() as ubyte ptr
declare function lex_textlen() as integer
declare function lex_lookahead_tk(byval n as integer) as integer
declare sub lex_skip()
declare sub lex_skip_until(byval tk as integer)
declare sub lex_select_file(byref filename as string)
declare sub lex_global_init()
