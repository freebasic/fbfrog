#define TRUE (-1)
#define FALSE 0
#define NULL 0

enum
	C_FILE    '' Browse file button
	C_DIR     '' Browse dir button
	C_CLEAR   '' Clear button
	C_GO      '' Go button

	C_CONCAT  '' Concat checkbox
	C_FOLLOW  '' Follow checkbox
	C_MERGE   '' Merge checkbox
	C_VERBOSE '' Verbose checkbox

	C_IN      '' Input text box
	C_OUT     '' Output text box

	C__COUNT
end enum

extern as zstring ptr control_strings(0 to (C__COUNT - 1))
extern as zstring ptr dialog_strings(0 to 1)
declare sub gui_on_clicked(byval i as integer)

declare function gui_is_checkbox_enabled(byval i as integer) as integer
declare function gui_get_text(byval i as integer) as string
declare sub gui_add_text(byval i as integer, byval text as zstring ptr)
declare sub gui_threadsafe_add_text(byval i as integer, byval text as zstring ptr)
declare sub gui_clear_text(byval i as integer)
declare sub gui_threadsafe_clear_text(byval i as integer)
declare function gui_dialog(byval i as integer) as zstring ptr
declare sub gui_set_window_title(byval text as zstring ptr)
declare sub gui_quit()
declare sub gui_init()
declare sub gui_run()
declare sub gui_end()
