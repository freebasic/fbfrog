#include once "fbfrog.bi"

enum
	DECL_PARAM = 0
	DECL_TOP
	DECL_TYPEDEF
	DECL_TYPEDEFSTRUCTBLOCK
	DECL_VAR
	DECL_FIELD
	DECL_PROC
end enum

declare function hSkip( byval x as integer ) as integer
declare function hSkipRev( byval x as integer ) as integer
declare function hSkipUnlessEol( byval x as integer ) as integer
declare function hSkipRevUnlessEol( byval x as integer ) as integer
declare function hIsWhitespaceUntilEol( byval x as integer ) as integer
declare function hSkipOptional _
	( _
		byval x as integer, _
		byval tk as integer _
	) as integer
declare function hInsertStatementSeparator( byval x as integer ) as integer
declare sub hRemoveThisAndSpace( byval x as integer )
declare function hFindToken( byval x as integer, byval tk as integer ) as integer
declare function hFindParentheses _
	( _
		byval x as integer, _
		byval backwards as integer _
	) as integer
declare function parseUnknown( byval x as integer ) as integer
declare function parsePPDirective _
	( _
		byval x as integer, _
		byval is_preparse as integer _
	) as integer

declare function parseEnumconst _
	( _
		byval x as integer, _
		byval is_unknown as integer _
	) as integer
declare function parseMultdecl _
	( _
		byval x as integer, _
		byval begin as integer, _
		byval decl as integer _
	) as integer
declare function parseTopdeclOrTypedef( byval x as integer ) as integer
declare function translateEnumconst( byval x as integer ) as integer
declare sub hFixupMultdecl( byval x as integer, byval begin as integer )
declare function translateDecl _
	( _
		byval x as integer, _
		byval decl as integer _
	) as integer

declare function parseStruct( byval x as integer ) as integer
declare function parseExternBegin( byval x as integer ) as integer
declare function parseExternEnd( byval x as integer ) as integer
declare function translateCompoundEnd _
	( _
		byval x as integer, _
		byval compoundkw as integer _
	) as integer
declare function translateStruct( byval x as integer ) as integer
