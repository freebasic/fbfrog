'' GTK+ GUI for Linux & co

#include once "gui.bi"

#undef TRUE
#undef FALSE
#include once "gtk/gtk.bi"
#undef TRUE
#undef FALSE
#define TRUE (-1)
#define FALSE 0

'' Needed for g_thread_init(), however the headers should do that...
#inclib "gthread-2.0"

type GtkStuff
	as GtkWidget ptr      win
	as GtkWidget ptr      text(0 to 1)
	as GtkTextBuffer ptr  buffer(0 to 1)
	as GtkTextMark ptr    mark(0 to 1)
	as GtkWidget ptr      checkboxes(0 to 3)
end type

dim shared as GtkStuff gtk

private function on_delete_event cdecl _
	( _
		byval widget as GtkWidget ptr, _
		byval event as GdkEvent ptr, _
		byval userdata as any ptr _
	) as integer

	gui_quit()

	'' Don't close the window now, we do that ourselves later
	return 1
end function

private sub on_clicked cdecl _
	( _
		byval button as GtkButton ptr, _
		byval userdata as gpointer _
	)
	gui_on_clicked(cint(userdata))
end sub

private function create_text_view(byval i as integer) as GtkWidget ptr
	'' Text view
	gtk.text(i) = gtk_text_view_new()

	gtk_text_view_set_wrap_mode(GTK_TEXT_VIEW(gtk.text(i)), GTK_WRAP_NONE)

	'' Text buffer
	gtk.buffer(i) = gtk_text_view_get_buffer(GTK_TEXT_VIEW(gtk.text(i)))

	'' Add a mark that represents the end of the buffer,
	'' that's where we want to append text.
	'' The marks will stay valid even across buffer modifications,
	'' unlike plain iters.
	dim as GtkTextIter tail
	gtk_text_buffer_get_end_iter(gtk.buffer(i), @tail)
	gtk.mark(i) = gtk_text_buffer_create_mark(gtk.buffer(i), "tail", @tail, FALSE)

	'' Add scroll bars by putting the textview inside a scrolledwindow
	dim as GtkWidget ptr scrolled = gtk_scrolled_window_new(NULL, NULL)
	gtk_container_add(GTK_CONTAINER(scrolled), gtk.text(i))

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

	for i as integer = C_FILE to C_CLEAR
		dim as GtkWidget ptr w = any
		w = gtk_button_new_with_label(control_strings(i))
		g_signal_connect(G_OBJECT(w), "clicked", G_CALLBACK(@on_clicked), cast(gpointer, i))
		gtk_box_pack_start(GTK_BOX(hbox), w, TRUE, TRUE, 0)
	next

	'' Big text box at the bottom
	gtk_box_pack_start(GTK_BOX(vbox), create_text_view(0), TRUE, TRUE, 0)

	return vbox
end function

private function create_out() as GtkWidget ptr
	'' The output part consists of a vbox with two entries:
	''  1) At the top: the option checkboxes and the go button
	''  2) At the bottom: a text input field (used for fbfrog output)

	dim as GtkWidget ptr vbox = gtk_vbox_new(FALSE, 0)

	for i as integer = C_CONCAT to C_VERBOSE
		gtk.checkboxes(i) = gtk_check_button_new_with_label(control_strings(i))
		gtk_box_pack_start(GTK_BOX(vbox), gtk.checkboxes(i), FALSE, TRUE, 0)
	next

	'' Go button
	dim as GtkWidget ptr button = gtk_button_new_with_label(control_strings(C_GO))
	g_signal_connect(G_OBJECT(button), "clicked", G_CALLBACK(@on_clicked), cast(gpointer, C_GO))
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

function gui_is_checkbox_enabled(byval i as integer) as integer
	return (gtk_toggle_button_get_active( _
	        GTK_TOGGLE_BUTTON(gtk.checkboxes(i))) <> FALSE)
end function

function gui_get_text(byval i as integer) as string
	dim as GtkTextIter head = any, tail = any
	gtk_text_buffer_get_bounds(gtk.buffer(i), @head, @tail)

	dim as gchar ptr text = _
		gtk_text_buffer_get_text(gtk.buffer(i), @head, @tail, FALSE)

	function = *cptr(zstring ptr, text)

	g_free(text)
end function

private sub add_text(byval i as integer, byval text as zstring ptr)
	if (text = NULL) then return
	if (text[0] = 0) then return
	dim as GtkTextIter iter = any
	gtk_text_buffer_get_iter_at_mark(gtk.buffer(i), @iter, gtk.mark(i))
	gtk_text_buffer_insert(gtk.buffer(i), @iter, text, -1)
end sub

sub gui_add_text(byval i as integer, byval text as zstring ptr)
	add_text(i, text)
	add_text(i, !"\n")
	gtk_text_view_scroll_mark_onscreen(GTK_TEXT_VIEW(gtk.text(i)), gtk.mark(i))
end sub

sub gui_threadsafe_add_text(byval i as integer, byval text as zstring ptr)
	gdk_threads_enter()
	gui_append_line(i, text)
	gdk_threads_leave()
end sub

sub gui_clear_text(byval i as integer)
	gtk_text_buffer_set_text(gtk.buffer(i), "", -1)
end sub

sub gui_threadsafe_clear_text(byval i as integer)
	gdk_threads_enter()
	gui_clear_text(i)
	gdk_threads_leave()
end sub

function gui_dialog(byval i as integer) as string
	dim as GtkWidget ptr dialog = _
		gtk_file_chooser_dialog_new( _
			dialog_strings(i), _
			GTK_WINDOW(gtk.win), _
			iif((i = 0), _
			    GTK_FILE_CHOOSER_ACTION_OPEN, _
			    GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER), _
			GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, _
			GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT, _
			NULL)

	if (gtk_dialog_run(GTK_DIALOG(dialog)) = GTK_RESPONSE_ACCEPT) then
		dim as gchar ptr selection = _
			gtk_file_chooser_get_filename(GTK_FILE_CHOOSER(dialog))

		function = *cptr(zstring ptr, selection)

		g_free(selection)
	end if

	gtk_widget_destroy(dialog)
end function

sub gui_set_window_title(byval text as zstring ptr)
	gtk_window_set_title(GTK_WINDOW(gtk.win), text)
end sub

sub gui_quit()
	gtk_main_quit()
end sub

sub gui_init()
	g_thread_init(NULL)
	gdk_threads_init()
	gtk_init(NULL, NULL)

	gtk.win = gtk_window_new(GTK_WINDOW_TOPLEVEL)
	g_signal_connect(G_OBJECT(gtk.win), "delete-event", G_CALLBACK(@on_delete_event), NULL)
	gtk_container_set_border_width(GTK_CONTAINER(gtk.win), 4)
	''gtk_window_move(GTK_WINDOW(gtk.win), x, y)
	gtk_window_set_default_size(GTK_WINDOW(gtk.win), 640, 480)
	''gtk_window_maximize(GTK_WINDOW(gtk.win))

	''dim as GtkWidget ptr align = gtk_alignment_new(0, 0, 1, 1)
	''gtk_box_pack_start(GTK_BOX(parentbox), align, FALSE, FALSE, 0)
	''gtk_container_add(GTK_CONTAINER(align), box)

	gtk_container_add(GTK_CONTAINER(gtk.win), create_controls())

	gtk_widget_show_all(gtk.win)
end sub

sub gui_run()
	'' Main loop
	gdk_threads_enter()
	gtk_main()
	gdk_threads_leave()
end sub

sub gui_end()
	gtk_widget_destroy(gtk.win)
end sub
