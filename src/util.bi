type SourceInfo
	name as zstring ptr
	is_file as integer '' whether name is a file path
end type

declare function sourceinfoForZstring(byval prettyname as zstring ptr) byref as SourceInfo

type TkLocation
	source as SourceInfo ptr
	linenum as integer
end type

type FileBuffer
	buffer as zstring ptr '' file content, null-terminated (embedded nulls are disallowed)
	source as SourceInfo
	declare sub load(byval location as TkLocation)
end type

declare function filebuffersAdd(byval filename as zstring ptr, byval location as TkLocation) as FileBuffer ptr

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
