#include once "hash.bi"

dim shared as HashTable fbkwhash

dim shared as zstring ptr fbkeywords(0 to ...) = _
{ _
	@"ABS"        , @"ACCESS"     , @"ACOS"       , @"ALIAS"      , _
	@"AND"        , @"ANDALSO"    , @"ANY"        , @"APPEND"     , _
	@"AS"         , @"ASC"        , @"ASIN"       , @"ASM"        , _
	@"ATAN2"      , @"ATN"        , _
	@"BASE"       , @"BINARY"     , @"BYREF"      , @"BYTE"       , _
	@"BYVAL"      , _
	@"CALL"       , @"CASE"       , @"CAST"       , @"CBYTE"      , _
	@"CDBL"       , @"CDECL"      , @"CHR"        , @"CINT"       , _
	@"CIRCLE"     , @"CLASS"      , @"CLNG"       , @"CLNGINT"    , _
	@"CLOSE"      , @"COLOR"      , @"COMMON"     , @"CONST"      , _
	@"CONSTRUCTOR", @"CONTINUE"   , @"COS"        , @"CPTR"       , _
	@"CSHORT"     , @"CSIGN"      , @"CSNG"       , @"CUBYTE"     , _
	@"CUINT"      , @"CULNG"      , @"CULNGINT"   , @"CUNSG"      , _
	@"CUSHORT"    , @"CVD"        , @"CVI"        , @"CVL"        , _
	@"CVLONGINT"  , @"CVS"        , @"CVSHORT"    , _
	@"DATA"       , @"DECLARE"    , @"DEFBYTE"    , @"DEFDBL"     , _
	@"DEFINE"     , @"DEFINED"    , @"DEFINT"     , @"DEFLNG"     , _
	@"DEFLONGINT" , @"DEFSHORT"   , @"DEFSNG"     , @"DEFSTR"     , _
	@"DEFUBYTE"   , @"DEFUINT"    , @"DEFULNG"    , @"DEFULONGINT", _
	@"DEFUSHORT"  , @"DELETE"     , @"DESTRUCTOR" , @"DIM"        , _
	@"DO"         , @"DOUBLE"     , @"DRAW"       , @"DYNAMIC"    , _
	@"ELSE"       , @"ELSEIF"     , @"ENCODING"   , @"END"        , _
	@"ENDIF"      , @"ENDMACRO"   , @"ENUM"       , @"EQV"        , _
	@"ERASE"      , @"ERR"        , @"ERROR"      , @"ESCAPE"     , _
	@"EXIT"       , @"EXP"        , @"EXPLICIT"   , @"EXPORT"     , _
	@"EXTERN"     , _
	@"FIELD"      , @"FIX"        , @"FOR"        , @"FRAC"       , _
	@"FUNCTION"   , _
	@"GET"        , @"GOSUB"      , @"GOTO"       , _
	@"IF"         , @"IFDEF"      , @"IFNDEF"     , @"IIF"        , _
	@"IMAGECREATE", @"IMP"        , @"IMPORT"     , @"INCLIB"     , _
	@"INCLUDE"    , @"INPUT"      , @"INSTR"      , @"INSTRREV"   , _
	@"INT"        , @"INTEGER"    , @"IS"         , _
	@"LANG"       , @"LBOUND"     , @"LEN"        , @"LET"        , _
	@"LIB"        , @"LIBPATH"    , @"LINE"       , @"LOCAL"      , _
	@"LOCK"       , @"LOG"        , @"LONG"       , @"LONGINT"    , _
	@"LOOP"       , @"LPRINT"     , @"LSET"       , @"LTRIM"      , _
	@"MACRO"      , @"MID"        , @"MKD"        , @"MKI"        , _
	@"MKL"        , @"MKLONGINT"  , @"MKS"        , @"MKSHORT"    , _
	@"MOD"        , @"MSBITFIELDS", _
	@"NAKED"      , @"NAME"       , @"NAMESPACE"  , @"NEW"        , _
	@"NEXT"       , @"NOGOSUB"    , @"NOKEYWORD"  , @"NOT"        , _
	@"ON"         , @"ONCE"       , @"OPEN"       , @"OPERATOR"   , _
	@"OPTION"     , @"OR"         , @"ORELSE"     , @"OUTPUT"     , _
	@"OVERLOAD"   , _
	@"PAINT"      , @"PALETTE"    , @"PASCAL"     , @"PEEK"       , _
	@"POINT"      , @"POINTER"    , @"POKE"       , @"POP"        , _
	@"PUSH"       , @"PRAGMA"     , @"PRESERVE"   , @"PRESET"     , _
	@"PRINT"      , @"PRIVATE"    , @"PROCPTR"    , @"PROPERTY"   , _
	@"PROTECTED"  , @"PSET"       , @"PTR"        , @"PUBLIC"     , _
	@"PUT"        , _
	@"RANDOM"     , @"READ"       , @"REDIM"      , @"REM"        , _
	@"RESTORE"    , @"RESUME"     , @"RETURN"     , @"RSET"       , _
	@"RTRIM"      , _
	@"SADD"       , @"SCOPE"      , @"SCREEN"     , @"SCREENRES"  , _
	@"SEEK"       , @"SELECT"     , @"SGN"        , @"SHARED"     , _
	@"SHL"        , @"SHORT"      , @"SHR"        , @"SIN"        , _
	@"SINGLE"     , @"SIZEOF"     , @"SPC"        , @"SQR"        , _
	@"STATIC"     , @"STDCALL"    , @"STEP"       , @"STR"        , _
	@"STRING"     , @"STRPTR"     , @"SUB"        , @"SWAP"       , _
	@"TAB"        , @"TAN"        , @"THEN"       , @"TO"         , _
	@"TRIM"       , @"TYPE"       , @"TYPEOF"     , _
	@"UBOUND"     , @"UBYTE"      , @"UINTEGER"   , @"ULONG"      , _
	@"ULONGINT"   , @"UNDEF"      , @"UNION"      , @"UNLOCK"     , _
	@"UNSIGNED"   , @"UNTIL"      , @"USHORT"     , @"USING"      , _
	@"VAR"        , @"VARPTR"     , @"VA_FIRST"   , @"VIEW"       , _
	@"WCHR"       , @"WEND"       , @"WHILE"      , @"WIDTH"      , _
	@"WINDOW"     , @"WITH"       , @"WRITE"      , @"WSTR"       , _
	@"WSTRING"    , _
	@"XOR"        , _
	@"ZSTRING"      _
}

sub transforms_global_init()
	hash_init(@emit.fbkwhash, 9)
	for i as integer = 0 to ubound(fbkeywords)
		dim as zstring ptr kw = fbkeywords(i)
		dim as integer length = len(*kw)
		dim as uinteger hash = hash_hash(kw, length)
		dim as HashItem ptr item = hash_lookup(@emit.fbkwhash, kw, length, hash)
		assert(item->s = NULL)
		item->s = kw
		item->length = length
		item->hash = hash
		emit.fbkwhash.count += 1
	next
end sub

private function is_fb_keyword(byval id as zstring ptr) as integer
	dim as integer length = len(*id)
	return (hash_lookup(@emit.fbkwhash, id, length, _
	                    hash_hash(id, length))->s <> NULL)
end function
