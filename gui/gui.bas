'' Main GUI module that can use either the gtk or win32 "backends"
''
'' This handles (at least some of) the event callbacks, and the worker
'' thread that runs fbfrog when needed.
''
'' Note: The worker thread must use gui_threadsafe_*() functions,
'' while the UI thread code (e.g. event callbacks) must use the normal gui_*()
'' functions.

#include once "gui.bi"

dim shared as zstring ptr control_strings(0 to (C__COUNT - 1)) = _
{ _
	@"Add file...", _
	@"Add directory...", _
	@"Reset", _
	@"Translate now!", _
	@"Concatenate files that are not #included anywhere", _
	@"Try to find #included files and translate them too", _
	@"Replace #includes by the file content if possible", _
	@"Show debugging information (file search results, statistics)", _
	@"", _
	@"" _
}

dim shared as zstring ptr dialog_strings(0 to 1) = _
{ _
	@"Select an input file", _
	@"Select an input directory" _
}

enum
	WORKER_READY = 0
	WORKER_WORK
	WORKER_EXIT
end enum

type GuiStuff
	as any ptr mutex
	as any ptr news
	as any ptr worker
	as integer workerstate
	as string frogargs
end type

dim shared as GuiStuff gui

private sub note(byval message as zstring ptr)
	gui_threadsafe_add_text(1, message)
end sub

private sub oops_not_found(byref cmd as string)
	note("oops, could not execute '" & cmd & "'")
end sub

private sub launch_fbfrog(byref args as string)
	#ifdef __FB_WIN32__
		static as string fbfrog
		if (len(fbfrog) = 0) then
			fbfrog = exepath() + "\fbfrog.exe"
		end if
	#else
		const fbfrog = "./fbfrog"
	#endif

	dim as string cmd = fbfrog & " " & args
	dim as integer f = freefile()

	if (open pipe(cmd, for input, as #f)) then
		oops_not_found(cmd)
		return
	end if

	note("$ " & cmd)

	dim as integer saw_text = FALSE
	dim as string ln
	while (eof(f) = FALSE)
		line input #f, ln

		if (len(ln) = 0) then
			continue while
		end if

		saw_text = TRUE
		note(ln)
	wend

	close #f

	if (saw_text = FALSE) then
		#ifdef __FB_WIN32__
			'' The win32 Open Pipe doesn't seem to return <> 0
			'' if fbfrog.exe isn't there (the shell prints an
			'' error on stderr, though that's usually hidden due
			'' to -s gui). So that's why here there is this
			'' additional check to detect that situation.
			if (exec(fbfrog, "") <> 0) then
				fbfrog_not_found(fbfrog)
				return
			end if
		#endif
		note("oops, no output retrieved, fbfrog missing?!")
	end if
end sub

private function process_job _
	( _
		byval state as integer, _
		byref args as string _
	) as integer

	select case (state)
	case WORKER_WORK
		gui_threadsafe_clear_text(1)
		launch_fbfrog(args)

	case WORKER_EXIT
		return FALSE

	end select

	return TRUE
end function

private sub worker_thread(byval userdata as any ptr)
	dim as string args

	do
		dim as integer state = any

		mutexlock(gui.mutex)
			'' Check the current state before starting to wait,
			'' because it might be set to exit already in case
			'' we missed the signal.
			'' This also works off the startup state.
			if (process_job(gui.workerstate, gui.frogargs) = FALSE) then
				mutexunlock(gui.mutex)
				exit do
			end if

			'' Wait for news: The condwait() will implicitly
			'' release the mutex lock and put the thread to sleep.
			'' That allows the main GUI thread to lock, signal,
			'' and unlock, after which the condwait() will
			'' automatically relock the mutex and return.
			condwait(gui.news, gui.mutex)

			state = gui.workerstate
			args = gui.frogargs
			gui.workerstate = WORKER_READY
		mutexunlock(gui.mutex)

		'' Work off state after signal, e.g. exit due to signal
		if (process_job(state, args) = FALSE) then
			exit do
		end if
	loop
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

private function build_frog_args() as string
	static as zstring ptr options(C_CONCAT to C_VERBOSE) = _
	{ _
		@"concat", @"follow", @"merge", @"verbose" _
	}

	dim as string args

	'' Collect the options from enable checkboxes
	for i as integer = C_CONCAT to C_VERBOSE
		if (gui_is_checkbox_enabled(i)) then
			args &= " --" & *options(i)
		end if
	next

	dim as string s = gui_get_text(0)

	'' The text retrieved from the edit boxes is one huge string
	'' containing newlines. Each line is treated as one file/dir name.
	'' It's possible to use something like *.h too.
	'' Here we just need to add quotes in case the filenames contain spaces.
	dim as string path
	while (len(s) > 0)
		dim as integer i = instr(s, any !"\r\n")
		path = left(s, i - 1)
		if (len(path) > 0) then
			'' TODO: Escape special chars (probably too hard)
			args += " """ + path + """"
		end if

		s = right(s, len(s) - i)
	wend

	return args
end function

sub gui_on_clicked(byval i as integer)
	select case (i)
	case C_FILE
		gui_add_text(0, gui_dialog(0))

	case C_DIR
		gui_add_text(0, gui_dialog(1))

	case C_CLEAR
		gui_clear_text(0)

	case C_GO
		mutexlock(gui.mutex)
		gui.frogargs = build_frog_args()
		gui.workerstate = WORKER_WORK
		condsignal(gui.news)
		mutexunlock(gui.mutex)

	end select
end sub

	gui_init()
	gui_set_window_title("fbfrog")

	'' Start worker thread with first job: calling "fbfrog --version"
	gui.mutex = mutexcreate()
	gui.news = condcreate()
	gui.frogargs = "--version"
	gui.workerstate = WORKER_WORK
	gui.worker = threadcreate(@worker_thread, NULL)

	gui_run()

	'' Wait for worker thread to exit and clean up
	mutexlock(gui.mutex)
	gui.workerstate = WORKER_EXIT
	condsignal(gui.news)
	mutexunlock(gui.mutex)

	threadwait(gui.worker)
	conddestroy(gui.news)
	mutexdestroy(gui.mutex)

	gui_end()
