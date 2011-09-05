enum
	TK_BOF = 0
	TK_EOF
	TK_EOL

	TK_TODO         '' TODOs added as fix-me-markers
	TK_BYTE         '' For stray bytes that don't fit in elsewhere

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
	TK_LINECOMMENT  '' // ...
	TK_BLOCKCOMMENT '' /* ... */
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
	KW__BOOL = KW__C_FIRST
	KW__COMPLEX
	KW__IMAGINARY
	KW_AUTO
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
	KW__FB_FIRST = KW__C_COUNT
	KW_ABS = KW__FB_FIRST
	KW_ACCESS
	KW_ACOS
	KW_ALIAS
	KW_AND
	KW_ANDALSO
	KW_ANY
	KW_APPEND
	KW_AS
	KW_ASC
	KW_ASIN
	KW_ASM
	KW_ATAN2
	KW_ATN
	KW_BASE
	KW_BINARY
	KW_BYREF
	KW_BYTE
	KW_BYVAL
	KW_CALL
	KW_CAST
	KW_CBYTE
	KW_CDBL
	KW_CDECL
	KW_CHR
	KW_CINT
	KW_CIRCLE
	KW_CLASS
	KW_CLNG
	KW_CLNGINT
	KW_CLOSE
	KW_COLOR
	KW_COMMON
	KW_CONSTRUCTOR
	KW_COS
	KW_CPTR
	KW_CSHORT
	KW_CSIGN
	KW_CSNG
	KW_CUBYTE
	KW_CUINT
	KW_CULNG
	KW_CULNGINT
	KW_CUNSG
	KW_CUSHORT
	KW_CVD
	KW_CVI
	KW_CVL
	KW_CVLONGINT
	KW_CVS
	KW_CVSHORT
	KW_DATA
	KW_DECLARE
	KW_DEFBYTE
	KW_DEFDBL
	KW_DEFINT
	KW_DEFLNG
	KW_DEFLONGINT
	KW_DEFSHORT
	KW_DEFSNG
	KW_DEFSTR
	KW_DEFUBYTE
	KW_DEFUINT
	KW_DEFULNG
	KW_DEFULONGINT
	KW_DEFUSHORT
	KW_DELETE
	KW_DESTRUCTOR
	KW_DIM
	KW_DRAW
	KW_DYNAMIC
	KW_ELSEIF
	KW_ENCODING
	KW_END
	KW_ENDMACRO
	KW_EQV
	KW_ERASE
	KW_ERR
	KW_ERROR
	KW_ESCAPE
	KW_EXIT
	KW_EXP
	KW_EXPLICIT
	KW_EXPORT
	KW_FIELD
	KW_FIX
	KW_FRAC
	KW_FUNCTION
	KW_GET
	KW_GOSUB
	KW_IIF
	KW_IMAGECREATE
	KW_IMP
	KW_IMPORT
	KW_INCLIB
	KW_INPUT
	KW_INSTR
	KW_INSTRREV
	KW_INTEGER
	KW_IS
	KW_LANG
	KW_LBOUND
	KW_LEN
	KW_LET
	KW_LIB
	KW_LIBPATH
	KW_LINE
	KW_LOCAL
	KW_LOCK
	KW_LOG
	KW_LONGINT
	KW_LOOP
	KW_LPRINT
	KW_LSET
	KW_LTRIM
	KW_MACRO
	KW_MID
	KW_MKD
	KW_MKI
	KW_MKL
	KW_MKLONGINT
	KW_MKS
	KW_MKSHORT
	KW_MOD
	KW_MSBITFIELDS
	KW_NAKED
	KW_NAME
	KW_NAMESPACE
	KW_NEW
	KW_NEXT
	KW_NOGOSUB
	KW_NOKEYWORD
	KW_NOT
	KW_ON
	KW_ONCE
	KW_OPEN
	KW_OPERATOR
	KW_OPTION
	KW_OR
	KW_ORELSE
	KW_OUTPUT
	KW_OVERLOAD
	KW_PAINT
	KW_PALETTE
	KW_PASCAL
	KW_PEEK
	KW_POINT
	KW_POINTER
	KW_POKE
	KW_POP
	KW_PUSH
	KW_PRESERVE
	KW_PRESET
	KW_PRINT
	KW_PRIVATE
	KW_PROCPTR
	KW_PROPERTY
	KW_PROTECTED
	KW_PSET
	KW_PTR
	KW_PUBLIC
	KW_PUT
	KW_RANDOM
	KW_READ
	KW_REDIM
	KW_REM
	KW_RESTORE
	KW_RESUME
	KW_RSET
	KW_RTRIM
	KW_SADD
	KW_SCOPE
	KW_SCREEN
	KW_SCREENRES
	KW_SEEK
	KW_SELECT
	KW_SGN
	KW_SHARED
	KW_SHL
	KW_SHR
	KW_SIN
	KW_SINGLE
	KW_SPC
	KW_SQR
	KW_STDCALL
	KW_STEP
	KW_STR
	KW_STRING
	KW_STRPTR
	KW_SUB
	KW_SWAP
	KW_TAB
	KW_TAN
	KW_THEN
	KW_TO
	KW_TRIM
	KW_TYPE
	KW_TYPEOF
	KW_UBOUND
	KW_UBYTE
	KW_UINTEGER
	KW_ULONG
	KW_ULONGINT
	KW_UNLOCK
	KW_UNTIL
	KW_USHORT
	KW_USING
	KW_VAR
	KW_VARPTR
	KW_VA_FIRST
	KW_VIEW
	KW_WCHR
	KW_WEND
	KW_WIDTH
	KW_WINDOW
	KW_WITH
	KW_WRITE
	KW_WSTR
	KW_WSTRING
	KW_XOR
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
declare sub tk_count_input_size(byval n as integer)
declare sub tk_count_input_token()

declare sub tk_emit_file(byref filename as string)
declare sub tk_in_file(byref filename as string)
