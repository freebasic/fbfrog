'' Token emitter

#include once "fbfrog.bi"

type EmitterStuff
	as integer fo '' Output file
	as integer todocount
	as integer filecount
end type

dim shared as EmitterStuff stuff

private sub emit(byval text as zstring ptr)
	dim as integer length = len(*text)
	if (put(#stuff.fo, , *cptr(ubyte ptr, text), length)) then
		oops("file I/O failed")
	end if
end sub

sub emit_token(byval x as integer)
	select case as const (tk_get(x))
	case TK_EOL
		#if defined(__FB_WIN32__) or defined(__FB_DOS__)
			emit(!"\r\n")
		#else
			emit(!"\n")
		#endif

	case TK_TODO
		dim as integer linecomment = (tk_get(x + 1) = TK_EOL)
		dim as zstring ptr text = tk_text(x)

		if (linecomment) then
			emit("'' ")
		else
			emit("/' ")
		end if

		emit("TODO")

		if (text) then
			emit(": ")
			emit(text)
		end if

		if (linecomment = FALSE) then
			emit(" '/")
		end if

		stuff.todocount += 1

	case TK_COMMENT
		emit("/'")
		emit(tk_text(x))
		emit("'/")

	case TK_LINECOMMENT
		emit("''")
		emit(tk_text(x))

	case TK_DECNUM
		emit(tk_text(x))

	case TK_HEXNUM
		emit("&h")
		emit(tk_text(x))

	case TK_OCTNUM
		emit("&o")
		emit(tk_text(x))

	case TK_STRING
		emit("""")
		emit(tk_text(x))
		emit("""")

	case TK_WSTRING
		emit("wstr(""")
		emit(tk_text(x))
		emit(""")")

	case TK_ESTRING
		emit("!""")
		emit(tk_text(x))
		emit("""")

	case TK_EWSTRING
		emit("wstr(!""")
		emit(tk_text(x))
		emit(""")")

	case else
		dim as zstring ptr text = tk_text(x)
		if (text) then
			emit(text)
		else
			text = token_text(tk_get(x))
			ASSUMING(text)
			emit(text)
		end if

	end select
end sub

private sub emit_init(byref filename as string)
	stuff.fo = freefile()
	if (open(filename, for binary, access write, as #stuff.fo)) then
		oops("could not open output file: '" & filename & "'")
	end if
end sub

private sub emit_end()
	close #stuff.fo
	stuff.fo = 0
end sub

sub emit_write_file(byref filename as string)
	emit_init(filename)

	dim as integer x = 0
	while (tk_get(x) <> TK_EOF)
		emit_token(x)
		x += 1
	wend

	emit_end()

	stuff.filecount += 1
end sub

sub emit_stats()
	dim as string message = "& TODO"
	if (stuff.todocount <> 1) then
		message &= "s"
	end if
	message &= " in & file"
	if (stuff.filecount <> 1) then
		message &= "s"
	end if
	print using message; stuff.todocount; stuff.filecount
end sub
