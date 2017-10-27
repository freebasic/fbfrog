#include once "emit-fbkeywords.bi"

type FbKeywordInfo
	id as zstring ptr
	fbkw as integer
end type

dim shared fbkeywordsinfo(0 to ...) as FbKeywordInfo => { _
	(@"ANDALSO"    , FBKW_OP), _
	(@"AND"        , FBKW_OP), _
	(@"DELETE"     , FBKW_OP), _
	(@"EQV"        , FBKW_OP), _
	(@"IMP"        , FBKW_OP), _
	(@"MOD"        , FBKW_OP), _
	(@"NEW"        , FBKW_OP), _
	(@"NOT"        , FBKW_OP), _
	(@"ORELSE"     , FBKW_OP), _
	(@"OR"         , FBKW_OP), _
	(@"SHL"        , FBKW_OP), _
	(@"SHR"        , FBKW_OP), _
	(@"XOR"        , FBKW_OP), _
	_
	(@"_"          , FBKW_CORE), _
	(@"ABS"        , FBKW_CORE), _
	(@"ABSTRACT"   , FBKW_CORE), _
	(@"ALIAS"      , FBKW_CORE), _
	(@"ANY"        , FBKW_CORE), _
	(@"AS"         , FBKW_CORE), _
	(@"ASM"        , FBKW_CORE), _
	(@"BASE"       , FBKW_CORE), _
	(@"BYREF"      , FBKW_CORE), _
	(@"BYTE"       , FBKW_CORE), _
	(@"BYVAL"      , FBKW_CORE), _
	(@"CALL"       , FBKW_CORE), _
	(@"CASE"       , FBKW_CORE), _
	(@"CAST"       , FBKW_CORE), _
	(@"CBYTE"      , FBKW_CORE), _
	(@"CDBL"       , FBKW_CORE), _
	(@"CDECL"      , FBKW_CORE), _
	(@"CINT"       , FBKW_CORE), _
	(@"CLASS"      , FBKW_CORE), _
	(@"CLNG"       , FBKW_CORE), _
	(@"CLNGINT"    , FBKW_CORE), _
	(@"COMMON"     , FBKW_CORE), _
	(@"CONST"      , FBKW_CORE), _
	(@"CONSTRUCTOR", FBKW_CORE), _
	(@"CONTINUE"   , FBKW_CORE), _
	(@"CPTR"       , FBKW_CORE), _
	(@"CSHORT"     , FBKW_CORE), _
	(@"CSIGN"      , FBKW_CORE), _
	(@"CSNG"       , FBKW_CORE), _
	(@"CUBYTE"     , FBKW_CORE), _
	(@"CUINT"      , FBKW_CORE), _
	(@"CULNG"      , FBKW_CORE), _
	(@"CULNGINT"   , FBKW_CORE), _
	(@"CUNSG"      , FBKW_CORE), _
	(@"CUSHORT"    , FBKW_CORE), _
	(@"DECLARE"    , FBKW_CORE), _
	(@"DESTRUCTOR" , FBKW_CORE), _
	(@"DIM"        , FBKW_CORE), _
	(@"DO"         , FBKW_CORE), _
	(@"DOUBLE"     , FBKW_CORE), _
	(@"ELSE"       , FBKW_CORE), _
	(@"ELSEIF"     , FBKW_CORE), _
	(@"END"        , FBKW_CORE), _
	(@"ENDIF"      , FBKW_CORE), _
	(@"ENUM"       , FBKW_CORE), _
	(@"EXIT"       , FBKW_CORE), _
	(@"EXPORT"     , FBKW_CORE), _
	(@"EXTENDS"    , FBKW_CORE), _
	(@"EXTERN"     , FBKW_CORE), _
	(@"FIX"        , FBKW_CORE), _
	(@"FOR"        , FBKW_CORE), _
	(@"FRAC"       , FBKW_CORE), _
	(@"FUNCTION"   , FBKW_CORE), _
	(@"GOTO"       , FBKW_CORE), _
	(@"IF"         , FBKW_CORE), _
	(@"IIF"        , FBKW_CORE), _
	(@"IMPLEMENTS" , FBKW_CORE), _
	(@"IMPORT"     , FBKW_CORE), _
	(@"INTEGER"    , FBKW_CORE), _
	(@"INT"        , FBKW_CORE), _
	(@"IS"         , FBKW_CORE), _
	(@"LET"        , FBKW_CORE), _
	(@"LIB"        , FBKW_CORE), _
	(@"LONG"       , FBKW_CORE), _
	(@"LONGINT"    , FBKW_CORE), _
	(@"LOOP"       , FBKW_CORE), _
	(@"NAMESPACE"  , FBKW_CORE), _
	(@"NEXT"       , FBKW_CORE), _
	(@"OPERATOR"   , FBKW_CORE), _
	(@"OVERLOAD"   , FBKW_CORE), _
	(@"PASCAL"     , FBKW_CORE), _
	(@"PEEK"       , FBKW_CORE), _
	(@"POINTER"    , FBKW_CORE), _
	(@"POKE"       , FBKW_CORE), _
	(@"PRIVATE"    , FBKW_CORE), _
	(@"PROCPTR"    , FBKW_CORE), _
	(@"PROPERTY"   , FBKW_CORE), _
	(@"PROTECTED"  , FBKW_CORE), _
	(@"PTR"        , FBKW_CORE), _
	(@"PUBLIC"     , FBKW_CORE), _
	(@"REM"        , FBKW_CORE), _
	(@"RETURN"     , FBKW_CORE), _
	(@"SCOPE"      , FBKW_CORE), _
	(@"SELECT"     , FBKW_CORE), _
	(@"SGN"        , FBKW_CORE), _
	(@"SHARED"     , FBKW_CORE), _
	(@"SHORT"      , FBKW_CORE), _
	(@"SINGLE"     , FBKW_CORE), _
	(@"STATIC"     , FBKW_CORE), _
	(@"STDCALL"    , FBKW_CORE), _
	(@"STEP"       , FBKW_CORE), _
	(@"STRING"     , FBKW_CORE), _
	(@"SUB"        , FBKW_CORE), _
	(@"SWAP"       , FBKW_CORE), _
	(@"THEN"       , FBKW_CORE), _
	(@"TO"         , FBKW_CORE), _
	(@"TYPE"       , FBKW_CORE), _
	(@"TYPEOF"     , FBKW_CORE), _
	(@"UBYTE"      , FBKW_CORE), _
	(@"UINTEGER"   , FBKW_CORE), _
	(@"ULONG"      , FBKW_CORE), _
	(@"ULONGINT"   , FBKW_CORE), _
	(@"UNION"      , FBKW_CORE), _
	(@"UNSIGNED"   , FBKW_CORE), _
	(@"UNTIL"      , FBKW_CORE), _
	(@"USHORT"     , FBKW_CORE), _
	(@"USING"      , FBKW_CORE), _
	(@"VA_FIRST"   , FBKW_CORE), _
	(@"VAR"        , FBKW_CORE), _
	(@"VIRTUAL"    , FBKW_CORE), _
	(@"WEND"       , FBKW_CORE), _
	(@"WHILE"      , FBKW_CORE), _
	(@"WITH"       , FBKW_CORE), _
	(@"WSTRING"    , FBKW_CORE), _
	(@"ZSTRING"    , FBKW_CORE), _
	_
	(@"ACCESS"     , FBKW_QUIRK), _
	(@"ACOS"       , FBKW_QUIRK), _
	(@"APPEND"     , FBKW_QUIRK), _
	(@"ASC"        , FBKW_QUIRK), _
	(@"ASIN"       , FBKW_QUIRK), _
	(@"ATAN2"      , FBKW_QUIRK), _
	(@"ATN"        , FBKW_QUIRK), _
	(@"BINARY"     , FBKW_QUIRK), _
	(@"CHR"        , FBKW_QUIRK), _
	(@"CIRCLE"     , FBKW_QUIRK), _
	(@"CLOSE"      , FBKW_QUIRK), _
	(@"COLOR"      , FBKW_QUIRK), _
	(@"COS"        , FBKW_QUIRK), _
	(@"CVD"        , FBKW_QUIRK), _
	(@"CVI"        , FBKW_QUIRK), _
	(@"CVL"        , FBKW_QUIRK), _
	(@"CVLONGINT"  , FBKW_QUIRK), _
	(@"CVS"        , FBKW_QUIRK), _
	(@"CVSHORT"    , FBKW_QUIRK), _
	(@"DATA"       , FBKW_QUIRK), _
	(@"DEFBYTE"    , FBKW_QUIRK), _
	(@"DEFDBL"     , FBKW_QUIRK), _
	(@"DEFINED"    , FBKW_QUIRK), _
	(@"DEFINT"     , FBKW_QUIRK), _
	(@"DEFLNG"     , FBKW_QUIRK), _
	(@"DEFLONGINT" , FBKW_QUIRK), _
	(@"DEFSHORT"   , FBKW_QUIRK), _
	(@"DEFSNG"     , FBKW_QUIRK), _
	(@"DEFSTR"     , FBKW_QUIRK), _
	(@"DEFUBYTE"   , FBKW_QUIRK), _
	(@"DEFUINT"    , FBKW_QUIRK), _
	(@"DEFULNG"    , FBKW_QUIRK), _
	(@"DEFULONGINT", FBKW_QUIRK), _
	(@"DEFUSHORT"  , FBKW_QUIRK), _
	(@"DRAW"       , FBKW_QUIRK), _
	(@"DYNAMIC"    , FBKW_QUIRK), _
	(@"ENCODING"   , FBKW_QUIRK), _
	(@"ERASE"      , FBKW_QUIRK), _
	(@"ERR"        , FBKW_QUIRK), _
	(@"ERROR"      , FBKW_QUIRK), _
	(@"EXP"        , FBKW_QUIRK), _
	(@"EXPLICIT"   , FBKW_QUIRK), _
	(@"FIELD"      , FBKW_QUIRK), _
	(@"GET"        , FBKW_QUIRK), _
	(@"GOSUB"      , FBKW_QUIRK), _
	(@"IMAGECREATE", FBKW_QUIRK), _
	(@"INCLUDE"    , FBKW_QUIRK), _
	(@"INPUT"      , FBKW_QUIRK), _
	(@"INSTR"      , FBKW_QUIRK), _
	(@"INSTRREV"   , FBKW_QUIRK), _
	(@"LBOUND"     , FBKW_QUIRK), _
	(@"LCASE"      , FBKW_QUIRK), _
	(@"LEN"        , FBKW_QUIRK), _
	(@"LINE"       , FBKW_QUIRK), _
	(@"LOCAL"      , FBKW_QUIRK), _
	(@"LOCK"       , FBKW_QUIRK), _
	(@"LOG"        , FBKW_QUIRK), _
	(@"LPRINT"     , FBKW_QUIRK), _
	(@"LSET"       , FBKW_QUIRK), _
	(@"LTRIM"      , FBKW_QUIRK), _
	(@"MID"        , FBKW_QUIRK), _
	(@"MKD"        , FBKW_QUIRK), _
	(@"MKI"        , FBKW_QUIRK), _
	(@"MKL"        , FBKW_QUIRK), _
	(@"MKLONGINT"  , FBKW_QUIRK), _
	(@"MKS"        , FBKW_QUIRK), _
	(@"MKSHORT"    , FBKW_QUIRK), _
	(@"NAME"       , FBKW_QUIRK), _
	(@"ON"         , FBKW_QUIRK), _
	(@"OPEN"       , FBKW_QUIRK), _
	(@"OPTION"     , FBKW_QUIRK), _
	(@"OUTPUT"     , FBKW_QUIRK), _
	(@"PAINT"      , FBKW_QUIRK), _
	(@"PALETTE"    , FBKW_QUIRK), _
	(@"POINT"      , FBKW_QUIRK), _
	(@"PRESERVE"   , FBKW_QUIRK), _
	(@"PRESET"     , FBKW_QUIRK), _
	(@"PRINT"      , FBKW_QUIRK), _
	(@"PSET"       , FBKW_QUIRK), _
	(@"PUT"        , FBKW_QUIRK), _
	(@"RANDOM"     , FBKW_QUIRK), _
	(@"READ"       , FBKW_QUIRK), _
	(@"REDIM"      , FBKW_QUIRK), _
	(@"RESTORE"    , FBKW_QUIRK), _
	(@"RESUME"     , FBKW_QUIRK), _
	(@"RSET"       , FBKW_QUIRK), _
	(@"RTRIM"      , FBKW_QUIRK), _
	(@"SADD"       , FBKW_QUIRK), _
	(@"SCREEN"     , FBKW_QUIRK), _
	(@"SEEK"       , FBKW_QUIRK), _
	(@"SIN"        , FBKW_QUIRK), _
	(@"SIZEOF"     , FBKW_QUIRK), _
	(@"SPC"        , FBKW_QUIRK), _
	(@"SQR"        , FBKW_QUIRK), _
	(@"STR"        , FBKW_QUIRK), _
	(@"STRPTR"     , FBKW_QUIRK), _
	(@"TAB"        , FBKW_QUIRK), _
	(@"TAN"        , FBKW_QUIRK), _
	(@"THREADCALL" , FBKW_QUIRK), _
	(@"TRIM"       , FBKW_QUIRK), _
	(@"UBOUND"     , FBKW_QUIRK), _
	(@"UCASE"      , FBKW_QUIRK), _
	(@"UNLOCK"     , FBKW_QUIRK), _
	(@"VARPTR"     , FBKW_QUIRK), _
	(@"VIEW"       , FBKW_QUIRK), _
	(@"WCHR"       , FBKW_QUIRK), _
	(@"WIDTH"      , FBKW_QUIRK), _
	(@"WINDOW"     , FBKW_QUIRK), _
	(@"WINPUT"     , FBKW_QUIRK), _
	(@"WRITE"      , FBKW_QUIRK), _
	(@"WSTR"       , FBKW_QUIRK), _
	_
	(@"ALLOCATE"    , FBKW_RTL), _
	(@"ASSERT"      , FBKW_RTL), _
	(@"ASSERTWARN"  , FBKW_RTL), _
	(@"BEEP"        , FBKW_RTL), _
	(@"BIN"         , FBKW_RTL), _
	(@"BIT"         , FBKW_RTL), _
	(@"BITRESET"    , FBKW_RTL), _
	(@"BITSET"      , FBKW_RTL), _
	(@"BLOAD"       , FBKW_RTL), _
	(@"BSAVE"       , FBKW_RTL), _
	(@"CALLOCATE"   , FBKW_RTL), _
	(@"CHAIN"       , FBKW_RTL), _
	(@"CHDIR"       , FBKW_RTL), _
	(@"CLEAR"       , FBKW_RTL), _
	(@"CLS"         , FBKW_RTL), _
	(@"COMMAND"     , FBKW_RTL), _
	(@"CONDBROADCAST", FBKW_RTL), _
	(@"CONDCREATE"  , FBKW_RTL), _
	(@"CONDDESTROY" , FBKW_RTL), _
	(@"CONDSIGNAL"  , FBKW_RTL), _
	(@"CONDWAIT"    , FBKW_RTL), _
	(@"CSRLIN"      , FBKW_RTL), _
	(@"CURDIR"      , FBKW_RTL), _
	(@"DATE"        , FBKW_RTL), _
	(@"DEALLOCATE"  , FBKW_RTL), _
	(@"DIR"         , FBKW_RTL), _
	(@"DYLIBFREE"   , FBKW_RTL), _
	(@"DYLIBLOAD"   , FBKW_RTL), _
	(@"DYLIBSYMBOL" , FBKW_RTL), _
	(@"ENVIRON"     , FBKW_RTL), _
	(@"EOF"         , FBKW_RTL), _
	(@"ERFN"        , FBKW_RTL), _
	(@"ERL"         , FBKW_RTL), _
	(@"ERMN"        , FBKW_RTL), _
	(@"EXEC"        , FBKW_RTL), _
	(@"EXEPATH"     , FBKW_RTL), _
	(@"FLIP"        , FBKW_RTL), _
	(@"FRE"         , FBKW_RTL), _
	(@"FREEFILE"    , FBKW_RTL), _
	(@"GETJOYSTICK" , FBKW_RTL), _
	(@"GETKEY"      , FBKW_RTL), _
	(@"GETMOUSE"    , FBKW_RTL), _
	(@"HEX"         , FBKW_RTL), _
	(@"HIBYTE"      , FBKW_RTL), _
	(@"HIWORD"      , FBKW_RTL), _
	(@"IMAGECONVERTROW", FBKW_RTL), _
	(@"IMAGEDESTROY", FBKW_RTL), _
	(@"IMAGEINFO"   , FBKW_RTL), _
	(@"INKEY"       , FBKW_RTL), _
	(@"INP"         , FBKW_RTL), _
	(@"KILL"        , FBKW_RTL), _
	(@"LEFT"        , FBKW_RTL), _
	(@"LOBYTE"      , FBKW_RTL), _
	(@"LOC"         , FBKW_RTL), _
	(@"LOCATE"      , FBKW_RTL), _
	(@"LOF"         , FBKW_RTL), _
	(@"LOWORD"      , FBKW_RTL), _
	(@"LPOS"        , FBKW_RTL), _
	(@"MKDIR"       , FBKW_RTL), _
	(@"MULTIKEY"    , FBKW_RTL), _
	(@"MUTEXCREATE" , FBKW_RTL), _
	(@"MUTEXDESTROY", FBKW_RTL), _
	(@"MUTEXLOCK"   , FBKW_RTL), _
	(@"MUTEXUNLOCK" , FBKW_RTL), _
	(@"OBJECT"      , FBKW_RTL), _
	(@"OCT"         , FBKW_RTL), _
	(@"OFFSETOF"    , FBKW_RTL), _
	(@"OUT"         , FBKW_RTL), _
	(@"PCOPY"       , FBKW_RTL), _
	(@"PMAP"        , FBKW_RTL), _
	(@"POINTCOORD"  , FBKW_RTL), _
	(@"POS"         , FBKW_RTL), _
	(@"RANDOMIZE"   , FBKW_RTL), _
	(@"REALLOCATE"  , FBKW_RTL), _
	(@"RESET"       , FBKW_RTL), _
	(@"RGB"         , FBKW_RTL), _
	(@"RGBA"        , FBKW_RTL), _
	(@"RIGHT"       , FBKW_RTL), _
	(@"RMDIR"       , FBKW_RTL), _
	(@"RND"         , FBKW_RTL), _
	(@"RUN"         , FBKW_RTL), _
	(@"SCREENCONTROL", FBKW_RTL), _
	(@"SCREENCOPY"  , FBKW_RTL), _
	(@"SCREENEVENT" , FBKW_RTL), _
	(@"SCREENGLPROC", FBKW_RTL), _
	(@"SCREENINFO"  , FBKW_RTL), _
	(@"SCREENLIST"  , FBKW_RTL), _
	(@"SCREENLOCK"  , FBKW_RTL), _
	(@"SCREENPTR"   , FBKW_RTL), _
	(@"SCREENRES"   , FBKW_RTL), _
	(@"SCREENSET"   , FBKW_RTL), _
	(@"SCREENSYNC"  , FBKW_RTL), _
	(@"SCREENUNLOCK", FBKW_RTL), _
	(@"SETDATE"     , FBKW_RTL), _
	(@"SETENVIRON"  , FBKW_RTL), _
	(@"SETMOUSE"    , FBKW_RTL), _
	(@"SETTIME"     , FBKW_RTL), _
	(@"SHELL"       , FBKW_RTL), _
	(@"SLEEP"       , FBKW_RTL), _
	(@"SPACE"       , FBKW_RTL), _
	(@"STOP"        , FBKW_RTL), _
	(@"SYSTEM"      , FBKW_RTL), _
	(@"THREADCREATE", FBKW_RTL), _
	(@"THREADWAIT"  , FBKW_RTL), _
	(@"TIME"        , FBKW_RTL), _
	(@"TIMER"       , FBKW_RTL), _
	(@"VA_ARG"      , FBKW_RTL), _
	(@"VAL"         , FBKW_RTL), _
	(@"VALINT"      , FBKW_RTL), _
	(@"VALLNG"      , FBKW_RTL), _
	(@"VALUINT"     , FBKW_RTL), _
	(@"VALULNG"     , FBKW_RTL), _
	(@"VA_NEXT"     , FBKW_RTL), _
	(@"WAIT"        , FBKW_RTL), _
	(@"WBIN"        , FBKW_RTL), _
	(@"WHEX"        , FBKW_RTL), _
	(@"WINDOWTITLE" , FBKW_RTL), _
	(@"WOCT"        , FBKW_RTL), _
	(@"WSPACE"      , FBKW_RTL), _
	_
	(@"DEFINE"  , FBKW_PP), _
	(@"ENDMACRO", FBKW_PP), _
	(@"IFDEF"   , FBKW_PP), _
	(@"IFNDEF"  , FBKW_PP), _
	(@"INCLIB"  , FBKW_PP), _
	(@"LANG"    , FBKW_PP), _
	(@"LIBPATH" , FBKW_PP), _
	(@"MACRO"   , FBKW_PP), _
	(@"PRAGMA"  , FBKW_PP), _
	(@"UNDEF"   , FBKW_PP)  _
}

constructor FBKeywordTable()
	for i as integer = 0 to ubound(fbkeywordsinfo)
		with fbkeywordsinfo(i)
			assert(tb.contains(.id, hashHash(.id)) = FALSE)
			tb.addOverwrite(.id, cast(any ptr, .fbkw))
		end with
	next
end constructor

function FBKeywordTable.lookup(byval id as zstring ptr) as integer
	var ucaseid = ucase(*id, 1)
	var item = tb.lookup(ucaseid, hashHash(ucaseid))
	if item->s then
		function = cint(item->data)
	else
		function = -1
	end if
end function

dim shared fbkeywords as FBKeywordTable
