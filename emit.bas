type EmitStuff
	as integer fo
end type

dim shared as EmitStuff emit

private sub write_out(byref what as string)
	put #emit.fo, , what
end sub

sub emit_open(byref filename as string)
	emit.fo = freefile()
	if (open(filename, for binary, access write, as #emit.fo)) then
		xoops("could not open output file: '" & filename & "'")
	end if
end sub

sub emit_close()
	close #emit.fo
	emit.fo = 0
end sub
