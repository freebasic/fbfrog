#include once "tk.bi"
#include once "common.bi"

type EmitterStuff
	as integer fo '' Output file
end type

dim shared as EmitterStuff emit

sub tk_emit_file(byref filename as string)
	emit.fo = freefile()
	if (open(filename, for binary, access write, as #emit.fo)) then
		xoops("could not open output file: '" & filename & "'")
	end if

#if 0
	for i as integer = 0 to (tk_count() - 1)
		if (tk_id(i) = TK_EOL) then
			text = @
		dim as zstring ptr text = tk_text(i)
		if (put(#emit.fo, , )) then
			xoops("file I/O failed")
		end if
	next
#endif

	close #emit.fo
	emit.fo = 0
end sub
