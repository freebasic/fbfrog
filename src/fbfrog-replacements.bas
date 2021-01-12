#include once "fbfrog-replacements.bi"
#include once "chars.bi"
#include once "util.bi"

constructor ReplacementsParser(byref filename as string)
	this.filename = filename
	f = freefile()
	if open(filename, for input, as #f) <> 0 then
		oops("couldn't open file '" + filename + "'")
	end if
end constructor

destructor ReplacementsParser()
	close #f
end destructor

sub ReplacementsParser.nextLine()
	do
		if reachedeof then
			ln = "<EOF>"
			exit do
		end if

		linenum += 1
		line input #f, ln

		'' Neither empty line, nor a comment?
		if len(ln) > 0 then
			if ln[0] <> CH_HASH then
				exit do
			end if
		end if

		reachedeof = eof(f)
	loop
end sub

sub ReplacementsParser.parseOops(byref message as string)
	print filename + "(" & linenum & "): error: " + message + ":"
	print "    " + ln
	end 1
end sub

function ReplacementsParser.parseCode(byref keyword as string) as string
	'' Any code behind the keyword?
	var code = hTrim(right(ln, len(ln) - len(keyword)))
	nextLine()
	if len(code) > 0 then
		return code
	end if

	'' Any indentation in the first line of the FB code block is treated as part
	'' of the replacements file, not the FB code block.
	''  * the first line must have some indentation
	''  * all lines of an FB code block must have at least the same indentation
	''    as the first line
	var trimmedln = hLTrim(ln)
	code += trimmedln
	var indentation = left(ln, len(ln) - len(trimmedln))
	assert(indentation + trimmedln = ln)
	if len(indentation) = 0 then
		parseOops("missing indentation in code block")
	end if
	nextLine()

	do
		'' Treat all following indented lines as part of the code block
		if (len(ln) = 0) orelse ((ln[0] <> CH_SPACE) and (ln[0] <> CH_TAB)) then
			exit do
		end if

		if left(ln, len(indentation)) <> indentation then
			parseOops("indentation here doesn't match the first line of this code block")
		end if
		code += !"\n" + right(ln, len(ln) - len(indentation))
		nextLine()
	loop until reachedeof

	function = code
end function

sub ReplacementsParser.parse(byref api as ApiInfo)
	nextLine()

	while reachedeof = FALSE
		'' Read convert/to line pair

		'' "convert" line
		if strStartsWith(ln, ConvertKeyword) = FALSE then
			parseOops("expected '" + ConvertKeyword + "' line, but found something else")
		end if
		var fromcode = parseCode(ConvertKeyword)

		'' "to" line
		dim tocode as string
		dim tofb as integer
		if strStartsWith(ln, ToCKeyword) then
			tocode = parseCode(ToCKeyword)
		elseif strStartsWith(ln, ToFbKeyword) then
			tocode = parseCode(ToFbKeyword)
			tofb = TRUE
		else
			parseOops("expected line to start with '" + ToCKeyword + "' or '" + ToFbKeyword + "', but found something else")
		end if

		api.addReplacement(fromcode, tocode, tofb)
	wend
end sub
