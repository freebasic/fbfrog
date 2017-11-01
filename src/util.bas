#include once "util.bi"
#include once "util-str.bi"
#include once "util-hash.bi"
#include once "util-path.bi"
#include once "crt.bi"

function min(byval a as integer, byval b as integer) as integer
	if b < a then a = b
	function = a
end function

function max(byval a as integer, byval b as integer) as integer
	if b > a then a = b
	function = a
end function

constructor SourceInfo(byval sourcename as const zstring ptr, byval is_file as integer)
	var mutablename = cptr(string ptr, @this.name)
	*mutablename = *sourcename
	this.is_file = is_file
end constructor

destructor SourceContext()
	for i as integer = 0 to table.room - 1
		var item = table.items + i
		if item->s then
			delete cptr(SourceInfo ptr, item->data)
		end if
	next
end destructor

function SourceContext.lookupOrMakeSourceInfo(byval sourcename as const zstring ptr, byval is_file as integer) as const SourceInfo ptr
	var hash = hashHash(sourcename)
	var item = table.lookup(sourcename, hash)
	if item->s = NULL then
		var sourceinfo = new SourceInfo(sourcename, is_file)
		table.add(item, hash, sourceinfo->name, sourceinfo)
	end if
	return item->data
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
	dim shared hashtb as THash = THash(8, FALSE)
end namespace

function filebuffersAdd(byref sourcectx as SourceContext, byval filename as zstring ptr, byval location as TkLocation) as FileBuffer ptr
	'' Cache file buffers based on the file name
	var hash = hashHash(filename)
	var item = filebuffers.hashtb.lookup(filename, hash)

	'' Not yet loaded?
	if item->s = NULL then
		var file = new FileBuffer
		file->source = sourcectx.lookupOrMakeSourceInfo(filename, TRUE)
		file->load(location)
		filebuffers.hashtb.add(item, hash, strptr(file->source->name), file)
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
		function = location.source->name + "(" & location.linenum & ")"
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
		function = pathStripCurdir(location.source->name) + "(" & location.linenum & "): " + *message
	else
		function = *message
	end if
end function

sub oopsLocation(byval location as TkLocation, byval message as zstring ptr)
	print hReport(location, message)
	end 1
end sub
