#include once "source.bi"
#include once "chars.bi"
#include once "util-path.bi"
#include once "util-str.bi"
#include once "util.bi"

constructor SourceInfo(byval sourcename as const zstring ptr)
	this.name = strDuplicate(sourcename)
end constructor

destructor SourceInfo()
	deallocate(this.name)
	deallocate(this.text)
end destructor

sub SourceInfo.setText(byval text as const zstring ptr)
	var old = this.text
	this.text = strDuplicate(text)
	deallocate(old)
	this.linecount = this.countLines()
end sub

sub SourceInfo.loadFile(byval location as DecodedLocation)
	'' Read in the whole file content
	var f = freefile()
	if open(*name, for binary, access read, as #f) then
		oopsLocation(location, "could not open file: '" + *name + "'")
	end if

	dim as ulongint filesize = lof(f)
	if filesize > &h40000000ull then
		oopsLocation(location, "a header file bigger than 1 GiB? no way...")
	end if

	'' An extra 0 byte at the end of the buffer so we can look ahead
	'' without bound checks, and don't need to give special treatment
	'' to empty files.
	dim as integer sizetoload = filesize
	deallocate(text)
	text = callocate(sizetoload + 1)
	if text = NULL then
		oops("SourceInfo.loadFile() memory allocation failed")
	end if

	if sizetoload > 0 then
		var sizeloaded = 0
		var result = get(#f, , *cptr(ubyte ptr, text), sizetoload, sizeloaded)
		if result or (sizeloaded <> sizetoload) then
			oopsLocation(location, "file I/O failed")
		end if
	end if

	close #f

	'' Currently we store file content as null-terminated strings,
	'' and null indicates EOF to the lexer, so can't allow embedded nulls.
	for i as integer = 0 to sizetoload - 1
		if text[i] = 0 then
			oopsLocation(location, "file '" + *name + "' has embedded nulls, please fix that first!")
		end if
	next

	this.linecount = this.countLines()
end sub

const function SourceInfo.countLines() as integer
	dim eols as integer = 0
	dim i as const ubyte ptr = text
	do
		select case *i
		case 0
			exit do
		case CH_LF
			i += 1
			eols += 1
		case CH_CR
			i += 1
			if *i = CH_LF then
				i += 1
			end if
			eols += 1
		case else
			i += 1
		end select
	loop

	'' Every line is terminated by an EOL, except maybe the last line (text at EOF but without EOL).
	var lines = eols
	if i > text then
		var lastchar = i[-1]
		if (lastchar <> CH_CR) and (lastchar <> CH_LF) then
			lines += 1
		end if
	else
		'' Empty file
		assert(lines = 0)
	end if

	return lines
end function

destructor SourceContext()
	for i as integer = 0 to sources.room - 1
		var item = sources.items + i
		if item->s then
			delete cptr(SourceInfo ptr, item->data)
		end if
	next
end destructor

function SourceContext.lookupOrAddSource(byval sourcename as const zstring ptr) as SourceInfo ptr
	var hash = hashHash(sourcename)
	var item = sources.lookup(sourcename, hash)
	if item->s = NULL then
		var source = new SourceInfo(sourcename)
		if source = NULL then
			oops("SourceInfo memory allocation failed")
		end if
		sources.add(item, hash, source->name, source)
	end if
	return item->data
end function

sub SourceContext.addSourceBase(byval source as SourceInfo ptr)
	'' Can only process if not yet done
	'' (prevent a source from being added multiple times to the locationbases() array)
	assert(source->arrayindex < 0)

	'' Empty files don't have locations, don't add them
	if source->linecount = 0 then
		return
	end if

	var last = ubound(locationbases)
	last += 1

	redim preserve locationbases(0 to last)
	redim preserve locationsources(0 to last)

	if last = 0 then
		'' First source's base value is 1, since 0 is reserved for invalid/unset locations
		locationbases(last) = 1
	else
		var previousbase = locationbases(last - 1)
		var previouslinecount = locationsources(last - 1)->linecount
		assert(culngint(previousbase) + culngint(previouslinecount) < &hFFFFFFFFu)
		locationbases(last) = previousbase + previouslinecount
	end if

	locationsources(last) = source
	source->arrayindex = last

	'' Check whether the source's number of lines fit into the remaining available range of values.
	'' We use ulong, so there are 32 bits available, but previously processed files may
	'' already have used up lots of that space.
	var basevalue = locationbases(last)
	#assert sizeof(basevalue) = sizeof(ulong)
	var sum = culngint(basevalue) + culngint(source->linecount)
	if (source->linecount < 1) or (cuint(source->linecount) > &hFFFFFFFFu) or (sum > &hFFFFFFFFu) then
		oops("SourceContext.addSourceBase(): line count " & source->linecount & " is too big/out of range (source " + *source->name + ", basevalue " & basevalue & ")")
	end if
end sub

function SourceContext.addInternalSource(byval additionaltitle as const zstring ptr, byval text as const zstring ptr) as SourceInfo ptr
	var sourcename = "<" & *additionaltitle
	if internalcount > 0 then
		sourcename &= " " & internalcount
	end if
	sourcename += ">"
	internalcount += 1
	var source = lookupOrAddSource(sourcename)
	assert(source->text = NULL)
	source->is_file = FALSE
	source->setText(text)
	addSourceBase(source)
	return source
end function

function SourceContext.addFileSource(byval filename as const zstring ptr, byval location as TkLocation) as SourceInfo ptr
	var source = lookupOrAddSource(filename)
	'' Load file if not yet done
	if source->text = NULL then
		source->is_file = TRUE
		source->loadFile(decode(location))
		addSourceBase(source)
	end if
	return source
end function

const function SourceContext.findEntryFor(byval location as TkLocation) as integer
	var count = ubound(locationbases) + 1

	'' If this location is from the last source in the list, we can skip the binary search.
	'' (This might be a common case when dealing with locations from lexing a newly added file)
	if (count > 0) andalso (locationbases(count - 1) < location.value) then
		return count - 1
	end if

	'' Fallback: binary search
	dim low as integer = 0
	dim high as integer = count
	while low < high
		'' Choose element in the middle of remaining range, rounding down
		var split = low + ((high - low) \ 2)
		if location.value < locationbases(split) then
			high = split
		else
			low = split + 1
		end if
	wend

	var i = low - 1
	if (i < 0) or (i >= count) then
		'' Should never have an invalid TkLocation
		'oops("SourceContext.findEntryFor(): failed to find source in list, for location value " & location.value)
		return -1
	end if
	return i
end function

const function SourceContext.decode(byval location as TkLocation) as DecodedLocation
	'print "decode(location.value=" & location.value & ")"
	'dumpBases()

	'' Shortcut for invalid/unset locations
	if location.value = 0 then
		return type<DecodedLocation>(NULL, -1)
	end if

	var i = findEntryFor(location)
	if i < 0 then
		return type<DecodedLocation>(NULL, -1)
	end if
	assert(location.value >= locationbases(i))

	dim result as DecodedLocation
	result.source = locationsources(i)
	result.linenum = location.value - locationbases(i) + 1

	if result.linenum > result.source->linecount then
		return type<DecodedLocation>(NULL, -1)
	end if

	assert(result.source->arrayindex = i)
	return result
end function

const function SourceContext.encode(byval location as DecodedLocation) as TkLocation
	'print "encode(linenum=" & linenum & ")"
	'dumpBases()

	assert((location.source->arrayindex >= 0) and (location.source->linecount > 0))

	'' Line number must be in this source's range
	'' (can't add more lines later, since it would overlap with following source's base values)
	assert((location.linenum >= 1) and (location.linenum <= location.source->linecount))

	var value = locationbases(location.source->arrayindex) + (location.linenum - 1)
	return type<TkLocation>(value)
end function

function SourceContext.dump(byval location as TkLocation) as string
	return location.value & "=" & dump(decode(location))
end function

function SourceContext.dump(byval location as DecodedLocation) as string
	var s = iif(location.source, *location.source->name, "<NULL>")
	s += "(" & location.linenum & ")"
	return s
end function

const sub SourceContext.dumpBases()
	print "SourceContext locationbases:"
	for i as integer = 0 to ubound(locationbases)
		print , i, "basevalue " & locationbases(i), "source " & *locationsources(i)->name
	next
end sub

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
function hReport(byval location as DecodedLocation, byval message as zstring ptr) as string
	if location.source then
		function = pathStripCurdir(*location.source->name) + "(" & location.linenum & "): " + *message
	else
		function = *message
	end if
end function

sub oopsLocation(byval location as DecodedLocation, byval message as zstring ptr)
	print hReport(location, message)
	end 1
end sub
