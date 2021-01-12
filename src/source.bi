#include once "util-hash.bi"

''
'' Token locations represent the source file (or other sources besides files)
'' and the line number. That's all we currently use in error messages.
''
'' To save memory, we don't store a SourceInfo ptr + line number in every TkLocation,
'' but instead assign unique numbers.
'' Each source file is given a unique range of values, matching its number of lines of code.
'' This way there is a unique value representing every available line of code.
'' The value 0 is reserved for representing invalid/unset locations.
''
type TkLocation field = 1
	value as ulong '' source's base number + line number - 1, 0 = invalid/unset
end type
#assert sizeof(TkLocation) = 4

type SourceInfo_ as SourceInfo

type DecodedLocation
	source as SourceInfo_ ptr
	linenum as integer '' 1-based line number
end type

type SourceInfo
	'' A unique name identifying this source (full path in case of source files).
	'' Owned string, used as key in SourceContext's hash table,
	'' allowing re-use of already loaded file contents.
	name as zstring ptr

	is_file as integer

	'' Source text (file content in case of files)
	'' Owned null-terminated string (embedded nulls are disallowed in loadFile())
	text as zstring ptr

	'' Number of text lines in this source file/string, or -1 if not parsed yet
	linecount as integer = -1

	'' This source's array index in the SourceContext's locationbases() array,
	'' or -1 if not added yet
	arrayindex as integer = -1

	declare constructor(byval sourcename as const zstring ptr)
	declare destructor()

	declare sub setText(byval text as const zstring ptr)
	declare sub loadFile(byval location as DecodedLocation)
	declare const function countLines() as integer
end type

type SourceContext
private:
	sources as THash = THash(8, FALSE) '' data = owned SourceInfo ptr

	'' List of base values and corresponding sources, for binary search.
	'' (two arrays always kept in sync)
	locationbases(any) as ulong
	locationsources(any) as SourceInfo ptr

	'' Counter for name generation for internal sources
	internalcount as integer

	declare function lookupOrAddSource(byval sourcename as const zstring ptr) as SourceInfo ptr
	declare sub addSourceBase(byval source as SourceInfo ptr)
	declare const function findEntryFor(byval location as TkLocation) as integer

public:
	declare destructor()
	declare operator let(byref rhs as const SourceContext) '' unimplemented

	declare function addInternalSource(byval additionaltitle as const zstring ptr, byval text as const zstring ptr) as SourceInfo ptr
	declare function addFileSource(byval filename as const zstring ptr, byval location as TkLocation) as SourceInfo ptr
	declare const function decode(byval location as TkLocation) as DecodedLocation
	declare const function encode(byval location as DecodedLocation) as TkLocation

	declare function dump(byval location as TkLocation) as string
	declare function dump(byval location as DecodedLocation) as string
	declare const sub dumpBases()
end type

declare sub hCalcErrorLine _
	( _
		byval column as integer, _
		byval limit as integer, _
		byref s as string, _
		byref offset as integer _
	)
declare function hErrorMarker(byval indent as integer, byval length as integer) as string
declare function hReport(byval location as DecodedLocation, byval message as zstring ptr) as string
declare sub oopsLocation(byval location as DecodedLocation, byval message as zstring ptr)
