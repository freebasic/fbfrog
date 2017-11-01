#include "util-hash.bi"

type SourceInfo
	name as const string '' used as key in SourceContext's and FileBuffers' hash table
	is_file as integer '' whether name is a file path
	declare constructor(byval sourcename as const zstring ptr, byval is_file as integer)
end type

type SourceContext
private:
	table as THash = Thash(8, FALSE) '' data = owned SourceInfo ptr
public:
	declare destructor()
	declare operator let(byref rhs as const SourceContext) '' unimplemented
	declare function lookupOrMakeSourceInfo(byval sourcename as const zstring ptr, byval is_file as integer) as const SourceInfo ptr
end type

type TkLocation
	source as const SourceInfo ptr
	linenum as integer
end type

type FileBuffer
	buffer as zstring ptr '' file content, null-terminated (embedded nulls are disallowed)
	source as const SourceInfo ptr
	declare sub load(byval location as TkLocation)
end type

declare function filebuffersAdd(byref sourcectx as SourceContext, byval filename as zstring ptr, byval location as TkLocation) as FileBuffer ptr

declare sub oops(byval message as zstring ptr)
declare function hDumpLocation(byval location as TkLocation) as string
declare sub hCalcErrorLine _
	( _
		byval column as integer, _
		byval limit as integer, _
		byref s as string, _
		byref offset as integer _
	)
declare function hErrorMarker(byval indent as integer, byval length as integer) as string
declare function hReport(byval location as TkLocation, byval message as zstring ptr) as string
declare sub oopsLocation(byval location as TkLocation, byval message as zstring ptr)
