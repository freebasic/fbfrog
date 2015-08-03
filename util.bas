#include once "fbfrog.bi"
#include once "crt.bi"

function min(byval a as integer, byval b as integer) as integer
	if b < a then a = b
	function = a
end function

function max(byval a as integer, byval b as integer) as integer
	if b > a then a = b
	function = a
end function

function sourceinfoForZstring(byval prettyname as zstring ptr) byref as SourceInfo
	function = *new SourceInfo(strDuplicate(prettyname), FALSE)
end function

sub FileBuffer.load(byval location as TkLocation)
	'' Read in the whole file content
	var f = freefile()
	if open(*source.name, for binary, access read, as #f) then
		oopsLocation(location, "could not open file: '" + *source.name + "'")
	end if

	dim as ulongint filesize = lof(f)
	if filesize > &h40000000 then
		oopsLocation(location, "a header file bigger than 1 GiB? no way...")
	end if

	'' An extra 0 byte at the end of the buffer so we can look ahead
	'' without bound checks, and don't need to give special treatment
	'' to empty files.
	dim as integer sizetoload = filesize
	buffer = callocate(sizetoload + 1)

	if sizetoload > 0 then
		var sizeloaded = 0
		var result = get(#f, , *cptr(ubyte ptr, buffer), sizetoload, sizeloaded)
		if result or (sizeloaded <> sizetoload) then
			oopsLocation(location, "file I/O failed")
		end if
	end if

	close #f

	'' Currently tokens store text as null-terminated strings, so they
	'' can't allow embedded nulls, and null also indicates EOF to the lexer.
	for i as integer = 0 to sizetoload-1
		if buffer[i] = 0 then
			oopsLocation(location, "file '" + *source.name + "' has embedded nulls, please fix that first!")
		end if
	next
end sub

namespace filebuffers
	dim shared hashtb as THASH
end namespace

sub filebuffersInit()
	hashInit(@filebuffers.hashtb, 8, FALSE)
end sub

function filebuffersAdd(byval filename as zstring ptr, byval location as TkLocation) as FileBuffer ptr
	'' Cache file buffers based on the file name
	var hash = hashHash(filename)
	var item = hashLookup(@filebuffers.hashtb, filename, hash)

	'' Not yet loaded?
	if item->s = NULL then
		var file = new FileBuffer
		file->source.name = strDuplicate(filename)
		file->source.is_file = TRUE
		file->load(location)
		hashAdd(@filebuffers.hashtb, item, hash, file->source.name, file)
	end if

	function = item->data
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

sub oops(byval message as zstring ptr)
	print "oops, " + *message
	end 1
end sub

function hDumpLocation(byval location as TkLocation) as string
	if location.source then
		function = *location.source->name + "(" & location.linenum & ")"
	end if
end function

sub hCalcErrorLine _
	( _
		byval column as integer, _
		byval limit as integer, _
		byref s as string, _
		byref offset as integer _
	)

	'' Line too long to fit in console?
	if len(s) > limit then
		var shift = 0

		if column < ((limit * 3) / 4) then
			'' Offset is still well visible, so align to left.
			s = left(s, limit)
		else
			'' Must scroll the line to the left (and the offset too),
			'' to make the location visible.
			'' a) center it, if the string is long enough for this,
			'' b) or align to the right.

			'' Enough chars behind the offset to fill up a half?
			var half = limit / 2
			if (len(s) - column) >= half then
				'' Center: shift left to align offset to visible boundary,
				shift = column - limit
				'' and shift further to reach the middle.
				shift += half
				s = mid(s, shift+1, limit)
			else
				'' Right align:
				shift = len(s) - limit
				s = right(s, limit)
			end if
		end if

		offset = column - shift
	else
		offset = column
	end if

end sub

function hErrorMarker(byval indent as integer, byval length as integer) as string
	function = space(indent) + "^" + string(length - 1, "~")
end function

'' Builds an error message string like this:
''    filename.bas(10): duplicate definition of 'foo'
'' The filename is shown relative to curdir() if possible, that's usually nicer
'' for the user.
function hReport(byval location as TkLocation, byval message as zstring ptr) as string
	if location.source then
		function = pathStripCurdir(*location.source->name) + "(" & location.linenum & "): " + *message
	else
		function = *message
	end if
end function

sub oopsLocation(byval location as TkLocation, byval message as zstring ptr)
	print hReport(location, message)
	end 1
end sub

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

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

dim shared fbkeywords as THASH

sub fbkeywordsInit()
	hashInit(@fbkeywords, 8, FALSE)
	for i as integer = 0 to ubound(fbkeywordsinfo)
		with fbkeywordsinfo(i)
			assert(hashContains(@fbkeywords, .id, hashHash(.id)) = FALSE)
			hashAddOverwrite(@fbkeywords, .id, cast(any ptr, .fbkw))
		end with
	next
end sub

function fbkeywordsLookup(byval id as zstring ptr) as integer
	var ucaseid = ucase(*id, 1)
	var item = hashLookup(@fbkeywords, ucaseid, hashHash(ucaseid))
	if item->s then
		function = cint(item->data)
	else
		function = -1
	end if
end function

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

function DeclPattern.matches _
	( _
		byval parentparent as ASTNODE ptr, _
		byval parent as ASTNODE ptr, _
		byval child as ASTNODE ptr, _
		byval childindex as integer _
	) as integer

	if len(parentid) > 0 then
		if parent = NULL then exit function

		'' If it's an anonymous procptr subtype, check its parent's id instead
		dim parentid as zstring ptr
		if parentparent andalso _
		   (parent->class = ASTCLASS_PROC) andalso _
		   (parentparent->subtype = parent) then
			parentid = parentparent->text
		else
			parentid = parent->text
		end if
		if strMatch(*parentid, this.parentid) = FALSE then exit function
	end if

	if len(childid) > 0 then
		'' Match child by name
		function = strMatch(*child->text, childid)
	else
		'' Match child by index
		function = (this.childindex = childindex)
	end if
end function

sub DeclPatterns.add(byref pattern as DeclPattern)
	var i = count
	count += 1
	patterns = reallocate(patterns, sizeof(*patterns) * count)
	patterns[i].constructor()
	patterns[i] = pattern
end sub

'' Declaration pattern format:
''    [<parent-id-pattern>.]<child-id-pattern>
''    <parent-id-pattern>.<child-index>
'' TODO: match based on astclass to speed things up a bit
''       (if we have a parentpattern, the child can only be a field/param/enumconst)
'' TODO: if pattern is a simple id (no wildcards or parent pattern), add to
''       hashtb instead of adding as DeclPattern?
sub DeclPatterns.parseAndAdd(byref s as string)
	dim pattern as DeclPattern

	strSplit(s, ".", pattern.parentid, pattern.childid)

	if len(pattern.childid) > 0 then
		'' Can be given a child index instead of a child id
		if strIsNumber(pattern.childid) then
			pattern.childindex = valuint(pattern.childid)
			pattern.childid = ""
		end if
	else
		'' Just one pattern; use it as child
		swap pattern.parentid, pattern.childid
	end if

	add(pattern)
end sub

destructor DeclPatterns()
	for i as integer = 0 to count - 1
		patterns[i].destructor()
	next
	deallocate(patterns)
end destructor

function DeclPatterns.matches _
	( _
		byval parentparent as ASTNODE ptr, _
		byval parent as ASTNODE ptr, _
		byval child as ASTNODE ptr, _
		byval childindex as integer _
	) as integer
	for i as integer = 0 to count - 1
		if patterns[i].matches(parentparent, parent, child, childindex) then
			return TRUE
		end if
	next
end function

''
'' StringMatcher: Prefix tree for pattern matching
'' Goal: to make matching faster than going through the patterns and doing strMatch() for each
''
'' foo
'' foobar
'' fuz
'' bar
''
'' str f
''   str oo
''     eol
''     str bar
''       eol
''   str uz
''     eol
'' str bar
''   eol
''
'' ab
'' a*b
''
'' str a
''   str b
''     eol
''   wildcard
''     str b
''       eol
''

constructor StringMatcher()
	root = astNewGROUP()
end constructor

destructor StringMatcher()
	astDelete(root)
end destructor

private function findCommonPrefixLen(byval a as const zstring ptr, byval b as const zstring ptr) as integer
	var length = 0
	while ((*a)[0] = (*b)[0]) and ((*a)[0] <> 0)
		length += 1
		a += 1
		b += 1
	wend
	function = length
end function

private function findWildcardOrEol(byval s as const zstring ptr) as integer
	var i = 0
	while ((*s)[0] <> CH_STAR) and ((*s)[0] <> 0)
		i += 1
		s += 1
	wend
	function = i
end function

static sub StringMatcher.insert(byval n as ASTNODE ptr, byval s as const zstring ptr)
	''
	'' Check current choices at this level
	'' We need to check for common prefix between an existing choice and the
	'' new pattern, and then re-use it and possibly extract it.
	''
	'' insert foo2 into
	''   foo1
	''     a
	''     b
	'' =>
	''   foo
	''     1
	''       a
	''       b
	''     2
	''
	'' insert foo2 into
	''   foo1
	''   bar
	'' =>
	''   foo
	''     1
	''     2
	''   bar
	''
	scope
		var i = n->head
		while i

			select case i->class
			case ASTCLASS_WILDCARD
				if (*s)[0] = CH_STAR then
					'' WILDCARD node is already here, recursively add the rest
					insert(i, @s[1])
					exit sub
				end if

			case ASTCLASS_EOL
				if (*s)[0] = 0 then
					'' Empty pattern, and we already have an EOL node here,
					'' so nothing more needs to be added
					exit sub
				end if

			case ASTCLASS_STRING
				var commonprefixlen = findCommonPrefixLen(i->text, s)
				if commonprefixlen > 0 then
					'' If there is a remainder, the node has to be split up
					if (*i->text)[commonprefixlen] <> 0 then
						'' Move the remainder suffix into a child node
						var remainder = astNew(ASTCLASS_STRING, @i->text[commonprefixlen])
						astTakeChildren(remainder, i)
						astAppend(i, remainder)

						'' Truncate to prefix only
						(*i->text)[commonprefixlen] = 0
					end if

					'' Pattern not completely covered yet? Then recursively add its remainder.
					if (*s)[commonprefixlen] <> 0 then
						insert(i, @s[commonprefixlen])
					end if

					exit sub
				end if
			end select

			i = i->next
		wend
	end scope

	'' Otherwise, add a new choice at this level
	var eol = findWildcardOrEol(s)
	if eol > 0 then
		'' Add node for string prefix, recursively add the rest
		dim as ubyte ptr prefix = allocate(eol + 1)
		memcpy(prefix, s, eol)
		prefix[eol] = 0

		var strnode = astNew(ASTCLASS_STRING)
		strnode->text = prefix
		astAppend(n, strnode)

		insert(n->tail, @s[eol])
	else
		select case (*s)[0]
		case CH_STAR
			'' Add node for wildcard, recursively add the rest
			astAppend(n, astNew(ASTCLASS_WILDCARD))
			insert(n->tail, @s[1])
		case 0
			'' EOL node
			astAppend(n, astNew(ASTCLASS_EOL))
		end select
	end if
end sub

sub StringMatcher.addPattern(byval s as const zstring ptr)
	insert(root, s)
end sub

static function StringMatcher.matches(byval n as ASTNODE ptr, byval s as const zstring ptr) as integer
	'' Check current choices at this level
	var prefix = n->head
	while prefix

		select case prefix->class
		case ASTCLASS_WILDCARD
			'' Also check suffixes, there must be at least an EOL node
			var suffix = prefix->head

			'' Check whether any of the suffixes match
			do
				'' Have to try matches at all possible positions (i.e. expanding the wildcard),
				'' even if it's just a null terminator (so it can match an EOL)
				var i = s
				do
					if matches(suffix, i) then return TRUE
					if (*i)[0] = 0 then exit do
					i += 1
				loop
				suffix = suffix->next
			loop while suffix

			'' No match here, this can happen if there is a suffix behind the wildcard but it
			'' didn't match the given string. Continue to next sibling prefix.

		case ASTCLASS_EOL
			'' The given string must be empty
			if (*s)[0] = 0 then return TRUE

			'' Continue to next sibling prefix

		case ASTCLASS_STRING
			'' The given string must have this exact prefix
			var prefixlen = strlen(prefix->text)
			if strncmp(prefix->text, s, prefixlen) = 0 then
				'' Prefix matched; now check suffixes, there must be at least an EOL node
				var suffix = prefix->head
				do
					if matches(suffix, @s[prefixlen]) then return TRUE
					suffix = suffix->next
				loop while suffix
			end if

			'' Continue to next sibling prefix

		end select

		prefix = prefix->next
	wend

	'' Nothing matched at this level
	function = FALSE
end function

function StringMatcher.matches(byval s as const zstring ptr) as integer
	function = matches(root, s)
end function
