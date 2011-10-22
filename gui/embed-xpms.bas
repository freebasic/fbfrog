'' Generates xpms.bas from the frog*.xpm files

#define TRUE (-1)
#define FALSE 0
#define NULL 0

private sub oops(byref message as string)
	print "oops, " & message
	end 1
end sub

private sub oops_cantopen(byref file as string)
	oops("could not open file: '" & file & "'")
end sub

private function str_replace _
	( _
		byref text as string, _
		byref a as string, _
		byref b as string _
	) as string

	dim as string result
	dim as string keep

	result = text

	dim as integer alen = len(a)
	dim as integer blen = len(b)

	dim as integer p = 0
	do
		p = instr(p + 1, result, a)
		if (p = 0) then
			exit do
		end if

		keep = mid(result, p + alen)
		result = left(result, p - 1)
		result += b
		result += keep
		p += blen - 1
	loop

	return result
end function

private sub parse_xpm(byref code as string, byref xpmfile as string)
	dim as string varname = str_replace(xpmfile, ".", "_")
	code += !"\ndim shared as zstring ptr "
	code += varname + "_data"
	code += !"(0 to ...) = _\n{ _\n"

	dim as integer f = freefile()
	if (open(xpmfile, for input, as #f)) then
		oops_cantopen(xpmfile)
	end if

	dim as string ln

	'' Check for the header line
	line input #f, ln
	if (ucase(ln) <> "/* XPM */") then
		oops("invalid xpm header")
	end if

	'' Check for lines containing strings (color and pixel lines)
	'' Other lines (declaration line, empty lines, C comments, ...) aren't
	'' explicitely handled, but should automatically be ignored, as long as
	'' they don't contain strings.
	dim as integer saw_rows = FALSE
	while (eof(f) = FALSE)
		line input #f, ln

		'' Strip everything in front of the first '"'
		ln = right(ln, len(ln) - (instr(ln, """") - 1))

		'' Strip everything behind the second '"'
		ln = left(ln, instr(2, ln, """"))

		'' Got something left?
		if (len(ln) > 0) then
			'' Add an entry to the array, in a new line, separated by a comma,
			'' if it's not the first one.
			if (saw_rows) then
				code += !", _\n"
			end if
			code += !"\t@" + ln
			saw_rows = TRUE
		end if
	wend

	if (saw_rows) then
		'' Line break after the last entry
		code += !" _ \n"
	else
		oops("no image data found")
	end if

	close #f

	code += !"}\n\n"
	code += "extern as zstring ptr ptr " & varname & !"\n"
	code += "dim shared as zstring ptr ptr " & varname & _
		" = @" & varname & !"_data(0)\n"
end sub


	dim as string code
	code += !"'' Automatically generated from the frog*.xpm files\n"
	parse_xpm(code, "frog16.xpm")
	parse_xpm(code, "frog24.xpm")
	parse_xpm(code, "frog32.xpm")
	parse_xpm(code, "frog48.xpm")

	const OUT_FILENAME = "xpms.bas"

	kill(OUT_FILENAME)
	dim as integer fo = freefile()
	if (open(OUT_FILENAME, for output, as #fo)) then
		oops_cantopen(OUT_FILENAME)
	end if
	print #fo, code;
	close #fo
