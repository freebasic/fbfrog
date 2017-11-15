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

#define DynamicArray(T_id) DynamicArray__##T_id

#macro declareDynamicArray(T, T_id, T_is_pod)
	type DynamicArray(T_id)
		p as T ptr
		count as integer
		declare constructor()
		declare constructor(byref rhs as const DynamicArray(T_id)) '' unimplemented
		declare operator let(byref rhs as const DynamicArray(T_id)) '' unimplemented
		declare destructor()
		#if T_is_pod
			declare sub append(byval item as T)
		#else
			declare sub append(byref item as const T)
		#endif
	end type
#endmacro

#macro implementDynamicArray(T, T_id, T_is_pod, T_is_fbstring, T_needs_deallocate)
	constructor DynamicArray(T_id)()
	end constructor

	destructor DynamicArray(T_id)()
		#if T_is_fbstring
			#assert T_is_pod = false
			#assert T_needs_deallocate = false
			for i as integer = 0 to count - 1
				p[i] = ""
			next
		#elseif T_needs_deallocate
			#assert T_is_pod
			for i as integer = 0 to count - 1
				deallocate(p[i])
			next
		#elseif T_is_pod = false
			for i as integer = 0 to count - 1
				p[i].destructor()
			next
		#endif
		deallocate(p)
	end destructor

	#if T_is_pod
		sub DynamicArray(T_id).append(byval item as T)
			var i = count
			count += 1
			p = reallocate(p, sizeof(T) * count)
			p[i] = item
		end sub
	#else
		sub DynamicArray(T_id).append(byref item as const T)
			var i = count
			count += 1
			p = reallocate(p, sizeof(T) * count)
			clear(p[i], 0, sizeof(T))
			p[i] = item
		end sub
	#endif
#endmacro

declareDynamicArray(const zstring ptr, const_zstring_ptr, true)
declareDynamicArray(string, string, false)
