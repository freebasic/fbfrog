'' GTK+ GUI for Linux & co
''
''    [ Browse ]  [ Clear ]  [x] Merge    [ Go! ]
''    +-------------------+  +-------------------+
''    | input files/dirs  |  | fbfrog output...  |
''    | editable...       |  |                   |
''    | one per line      |  |                   |
''    |                   |  |                   |
''    +-------------------+  +-------------------+
''

#define NULL 0

#inclib "gthread-2.0"

#undef TRUE
#undef FALSE
#include once "gtk/gtk.bi"
#undef TRUE
#undef FALSE
#define TRUE (-1)
#define FALSE 0

enum
	WORKER_READY = 0
	WORKER_WORK
	WORKER_EXIT
end enum

type GuiStuff
	as GtkWidget ptr win

	as GtkWidget ptr     text(0 to 1)
	as GtkTextBuffer ptr buffer(0 to 1)
	as GtkTextMark ptr   mark(0 to 1)
	as integer text0_ready
	as gulong tag_button_release_event_text0
	as GtkWidget ptr checkboxes(0 to 3)

	as any ptr mutex
	as any ptr news
	as any ptr worker
	as integer workerstate
	as string frogargs
end type

dim shared as GuiStuff gui

sub gui_set_window_title(byval text as zstring ptr)
	gtk_window_set_title(GTK_WINDOW(gui.win), text)
end sub

sub gui_quit()
	gtk_main_quit()
end sub

private sub append_text(byval i as integer, byval text as zstring ptr)
	dim as GtkTextIter iter = any
	gtk_text_buffer_get_iter_at_mark(gui.buffer(i), @iter, gui.mark(i))
	gtk_text_buffer_insert(gui.buffer(i), @iter, text, -1)
end sub

private sub append_line(byval i as integer, byval text as zstring ptr)
	append_text(i, text)
	append_text(i, !"\n")
	gtk_text_view_scroll_mark_onscreen(GTK_TEXT_VIEW(gui.text(i)), gui.mark(i))
end sub

private sub clear_buffer(byval i as integer)
	gtk_text_buffer_set_text(gui.buffer(i), "", -1)
end sub

private function get_checkbox_enabled(byval i as integer) as integer
	return (gtk_toggle_button_get_active( _
				GTK_TOGGLE_BUTTON(gui.checkboxes(i))) <> FALSE)
end function

private function get_buffer_text(byval i as integer) as string
	dim as GtkTextIter head = any, tail = any
	gtk_text_buffer_get_bounds(gui.buffer(i), @head, @tail)

	dim as gchar ptr text = _
		gtk_text_buffer_get_text(gui.buffer(i), @head, @tail, FALSE)

	function = *cptr(zstring ptr, text)

	g_free(text)
end function

private sub make_text0_ready()
	if (gui.text0_ready = FALSE) then
		clear_buffer(0)
		gui.text0_ready = TRUE

		'' The button-release-event is no longer needed
		g_signal_handler_disconnect(gui.text(0), gui.tag_button_release_event_text0)
	end if
end sub

private sub launch_fbfrog(byref args as string)
	dim as string cmd = "../fbfrog " + args
	dim as integer f = freefile()

	if (open pipe(cmd, for input, as #f)) then
		gdk_threads_enter()
		append_line(1, "oops, could not execute '" & cmd & "'")
		gdk_threads_leave()
		return
	end if

	gdk_threads_enter()
	append_line(1, "$ " & cmd)
	gdk_threads_leave()

	dim as string s
	dim as string ln
	while (eof(f) = FALSE)
		line input #f, ln

		if (len(ln) = 0) then
			continue while
		end if

		gdk_threads_enter()
		append_line(1, ln)
		gdk_threads_leave()
	wend

	close #f
end sub

private function process_job _
	( _
		byval state as integer, _
		byref args as string _
	) as integer

	select case (state)
	case WORKER_WORK
		gdk_threads_enter()
		clear_buffer(1)
		gdk_threads_leave()
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

private function get_frog_options() as string
	static as zstring ptr options(0 to 3) = _
	{ _
		@"concat", @"follow", @"merge", @"verbose" _
	}

	dim as string args

	'' Collect the options from enable checkboxes
	for i as integer = 0 to 3
		if (get_checkbox_enabled(i)) then
			args &= " --" & *options(i)
		end if
	next

	return args
end function

private function get_input_files() as string
	function = str_replace(get_buffer_text(0), !"\n", " ")
end function

private function on_delete_event cdecl _
	( _
		byval widget as GtkWidget ptr, _
		byval event as GdkEvent ptr, _
		byval userdata as any ptr _
	) as integer

	''on_want_close()
	gui_quit()

	'' Don't close the window now, we do that ourselves later
	return 1
end function

private sub on_clicked_go cdecl _
	( _
		byval button as GtkButton ptr, _
		byval userdata as gpointer _
	)

	make_text0_ready()

	mutexlock(gui.mutex)
	gui.frogargs = get_frog_options() & " " & get_input_files()
	gui.workerstate = WORKER_WORK
	condsignal(gui.news)
	mutexunlock(gui.mutex)
end sub

private sub on_clicked_browse cdecl _
	( _
		byval button as GtkButton ptr, _
		byval userdata as gpointer _
	)

	make_text0_ready()

	dim as zstring ptr title = any
	dim as GtkFileChooserAction action = any
	if (cint(userdata)) then
		'' File browse button
		title = @"Select an input file"
		action = GTK_FILE_CHOOSER_ACTION_OPEN
	else
		'' Directory browse button
		title = @"Select an input directory"
		action = GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER
	end if

	dim as GtkWidget ptr dialog = _
			gtk_file_chooser_dialog_new( _
				title, _
				GTK_WINDOW(gui.win), _
				action, _
				GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, _
				GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT, _
				NULL)

	if (gtk_dialog_run(GTK_DIALOG(dialog)) = GTK_RESPONSE_ACCEPT) then
		dim as gchar ptr selection = _
			gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog))
		append_line(0, selection)
		g_free(selection)
	end if

	gtk_widget_destroy(dialog)
end sub

private sub on_clicked_reset cdecl _
	( _
		byval button as GtkButton ptr, _
		byval userdata as gpointer _
	)
	make_text0_ready()
	clear_buffer(0)
end sub

private function on_button_release_event_text0 cdecl _
	( _
		byval widget as GtkWidget ptr, _
		byval even as GdkEvent ptr, _
		byval userdata as gpointer _
	) as gboolean
	make_text0_ready()
	return FALSE
end function

private function create_text_view(byval i as integer) as GtkWidget ptr
	'' Text view
	gui.text(i) = gtk_text_view_new()

	if (i = 0) then
		gui.tag_button_release_event_text0 = _
			g_signal_connect(G_OBJECT(gui.text(0)), _
					"button-release-event", _
					G_CALLBACK(@on_button_release_event_text0), _
					NULL)
	end if

	gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(gui.text(i)), GTK_WRAP_NONE)

	'' Text buffer
	gui.buffer(i) = gtk_text_view_get_buffer(GTK_TEXT_VIEW(gui.text(i)))

	'' Add a mark that represents the end of the buffer,
	'' that's where we want to append text.
	'' The marks will stay valid even across buffer modifications,
	'' unlike plain iters.
	dim as GtkTextIter tail
	gtk_text_buffer_get_end_iter(gui.buffer(i), @tail)
	gui.mark(i) = gtk_text_buffer_create_mark(gui.buffer(i), "tail", @tail, FALSE)

	'' Add scroll bars by putting the textview inside a scrolledwindow
	dim as GtkWidget ptr scrolled = gtk_scrolled_window_new(NULL, NULL)
	gtk_container_add(GTK_CONTAINER(scrolled), gui.text(i))

	return scrolled
end function

private function create_in() as GtkWidget ptr
	'' The input part consists of a vbox with two entries:
	''  1) At the top: the browse/reset buttons
	''  2) At the bottom: a text input field

	dim as GtkWidget ptr vbox = gtk_vbox_new(FALSE, 0)

	'' The browse/reset buttons need a hbox to be arranged horizontally.
	dim as GtkWidget ptr hbox = gtk_hbox_new(FALSE, 0)
	gtk_box_pack_start(GTK_BOX(vbox), hbox, FALSE, TRUE, 0)

	dim as GtkWidget ptr w = any

	'' File browse button
	w = gtk_button_new_with_label("Add file...")
	g_signal_connect(G_OBJECT(w), "clicked", G_CALLBACK(@on_clicked_browse), cast(gpointer, TRUE))
	gtk_box_pack_start(GTK_BOX(hbox), w, TRUE, TRUE, 0)

	'' Dir browse button
	w = gtk_button_new_with_label("Add directory...")
	g_signal_connect(G_OBJECT(w), "clicked", G_CALLBACK(@on_clicked_browse), cast(gpointer, FALSE))
	gtk_box_pack_start(GTK_BOX(hbox), w, TRUE, TRUE, 0)

	'' Clear/reset button
	w = gtk_button_new_with_label("Reset")
	g_signal_connect(G_OBJECT(w), "clicked", G_CALLBACK(@on_clicked_reset), NULL)
	gtk_box_pack_start(GTK_BOX(hbox), w, TRUE, TRUE, 0)

	'' Big text box at the bottom
	gtk_box_pack_start(GTK_BOX(vbox), create_text_view(0), TRUE, TRUE, 0)

	return vbox
end function

private function create_out() as GtkWidget ptr
	'' The output part consists of a vbox with two entries:
	''  1) At the top: the option checkboxes and the go button
	''  2) At the bottom: a text input field (used for fbfrog output)

	dim as GtkWidget ptr vbox = gtk_vbox_new(FALSE, 0)

	static as zstring ptr checkbox_texts(0 to 3) = _
	{ _
		@"Concatenate files that are not #included anywhere", _  '' Concat
		@"Try to find #included files and translate them too", _ '' Follow
		@"Replace #includes by the file content if possible", _   '' Merge
		@"Show debugging information (file search results, statistics)" _ '' Verbose
	}

	for i as integer = 0 to 3
		'' Concat
		gui.checkboxes(i) = gtk_check_button_new_with_label(checkbox_texts(i))
		gtk_box_pack_start(GTK_BOX(vbox), gui.checkboxes(i), FALSE, TRUE, 0)
	next

	'' Go button
	dim as GtkWidget ptr button = gtk_button_new_with_label("Translate now!")
	g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(@on_clicked_go), NULL)
	gtk_box_pack_start(GTK_BOX(vbox), button, FALSE, TRUE, 0)

	'' Big text box at the bottom
	gtk_box_pack_start(GTK_BOX(vbox), create_text_view(1), TRUE, TRUE, 0)

	return vbox
end function

private function create_controls() as GtkWidget ptr
	'' Main horizontal paned container
	dim as GtkWidget ptr paned = gtk_hpaned_new()

	'' On the left side: input stuff
	gtk_paned_pack1(GTK_PANED(paned), create_in(), TRUE, TRUE)

	'' On the right side: output stuff
	gtk_paned_pack2(GTK_PANED(paned), create_out(), TRUE, TRUE)

	return paned
end function

sub gui_init()
	g_thread_init(NULL)
	gdk_threads_init()
	gtk_init(NULL, NULL)

	gui.win = gtk_window_new(GTK_WINDOW_TOPLEVEL)
	g_signal_connect(G_OBJECT(gui.win), "delete-event", G_CALLBACK(@on_delete_event), NULL)
	gtk_container_set_border_width(GTK_CONTAINER(gui.win), 4)
	''gtk_window_move(GTK_WINDOW(gui.win), x, y)
	gtk_window_set_default_size(GTK_WINDOW(gui.win), 640, 480)
	''gtk_window_maximize(GTK_WINDOW(gui.win))

	''dim as GtkWidget ptr align = gtk_alignment_new(0, 0, 1, 1)
	''gtk_box_pack_start(GTK_BOX(parentbox), align, FALSE, FALSE, 0)
	''gtk_container_add(GTK_CONTAINER(align), box)

	gtk_container_add(GTK_CONTAINER(gui.win), create_controls())

	gtk_widget_show_all(gui.win)
end sub

sub gui_run()
	'' Main loop
	gdk_threads_enter()
	gtk_main()
	gdk_threads_leave()
end sub

sub gui_end()
	gtk_widget_destroy(gui.win)
end sub

	gui_init()
	gui_set_window_title("fbfrog")
	append_line(0, "Enter input files/directories here")
	append_line(0, "(or use the Browse button)")
	append_line(1, "fbfrog output will appear here")
	gui.text0_ready = FALSE

	'' We'll just start the thread now. The first thing it will do
	'' when started is to call "fbfrog --version" and add the output
	'' to the text box. Then it will start condwait()ing for news.
	'' i.e. all signals will be ignored until it's condwait()ing.
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
