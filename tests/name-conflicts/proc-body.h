// In the procedure body we're using the procedure itself and a parameter.
// Both the declarations and the uses should be renamed together if renaming is
// needed.
void screen(int color) {
	screen(color);
}
