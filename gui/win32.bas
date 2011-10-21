'' Win32 GUI

#include once "gui.bi"

#define WIN_INCLUDEALL
#include once "windows.bi"
#include once "win/commctrl.bi"
#include once "win/commdlg.bi"
#include once "win/shlobj.bi"

type ControlInfo
	as uinteger exstyles
	as zstring ptr class
	as uinteger styles
end type

const TEXTBOX_STYLES = _
		WS_HSCROLL or WS_VSCROLL or _
		ES_AUTOHSCROLL or ES_AUTOVSCROLL or _
		ES_MULTILINE or ES_NOHIDESEL

dim shared as ControlInfo ctlinfo(0 to (C__COUNT - 1)) = _
{ _
	(0, @WC_BUTTON, WS_GROUP), _
	(0, @WC_BUTTON, 0), _
	(0, @WC_BUTTON, 0), _
	(0, @WC_BUTTON, BS_DEFPUSHBUTTON or WS_GROUP), _
	(0, @WC_BUTTON, BS_AUTOCHECKBOX or WS_GROUP), _
	(0, @WC_BUTTON, BS_AUTOCHECKBOX), _
	(0, @WC_BUTTON, BS_AUTOCHECKBOX), _
	(0, @WC_BUTTON, BS_AUTOCHECKBOX), _
	(WS_EX_CLIENTEDGE, @WC_EDIT, TEXTBOX_STYLES), _
	(WS_EX_CLIENTEDGE, @WC_EDIT, TEXTBOX_STYLES) _
}

type Win32Stuff
	as HINSTANCE hinst
	as HFONT font
	as HBRUSH background_brush
	as HWND win
	as integer lineheight '' Height of a line of text
	as HWND controls(0 to (C__COUNT - 1))
end type

dim shared as Win32Stuff w32

private sub oops(byref message as string)
	print "oops, " & message
	MessageBox(NULL, message, "fbfrog oops", MB_OK or MB_ICONERROR)
	end 1
end sub

private sub oops_win32(byval func as zstring ptr)
	dim as DWORD e = GetLastError()

	dim as string message = *func & "() failed"
	message += " (error " & e & ")"

	dim as zstring ptr p = NULL
	if (FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER or _
	                  FORMAT_MESSAGE_FROM_SYSTEM, _
	                  NULL, e, 0, cptr(LPTSTR, @p), 0, NULL)) then
		message += ": " & *p
		LocalFree(p)
	end if

	oops(message)
end sub

private function load_resource_icon _
	( _
		byval id as integer, _
		byval small as integer _
	) as HICON

	dim as HICON icon = _
		LoadImage(w32.hinst, MAKEINTRESOURCE(id), _
			IMAGE_ICON, _
			GetSystemMetrics(iif(small, SM_CXSMICON, SM_CXICON)), _
			GetSystemMetrics(iif(small, SM_CYSMICON, SM_CYICON)), _
			LR_DEFAULTCOLOR)

	'' Fallback to something else if that failed
	if (icon = NULL) then
		icon = LoadImage(NULL, MAKEINTRESOURCE(OIC_SAMPLE), _
		                 IMAGE_ICON, 0, 0, LR_SHARED or LR_DEFAULTSIZE)
	end if

	return icon
end function

private function load_cursor() as HCURSOR
	return LoadImage(NULL, MAKEINTRESOURCE(OCR_NORMAL), IMAGE_CURSOR, _
	                 0, 0, LR_SHARED or LR_DEFAULTSIZE)
end function

private function load_default_font() as HFONT
	dim as NONCLIENTMETRICS m
	m.cbSize = sizeof(m)
	SystemParametersInfo(SPI_GETNONCLIENTMETRICS, sizeof(m), @m, 0)
	return CreateFontIndirect(@m.lfMenuFont)
end function

private sub init_font()
	w32.font = load_default_font()
end sub

private sub delete_font()
	if (w32.font) then
		DeleteObject(w32.font)
	end if
end sub

private sub init_lineheight()
	dim as HDC dc = GetDC(w32.win)

	dim as RECT rc
	DrawText(dc, "TEST", -1, @rc, DT_CALCRECT)

	w32.lineheight = rc.bottom - rc.top

	ReleaseDC(w32.win, dc)
end sub

private sub set_nicer_font(byval h as HWND)
	SendMessage(h, WM_SETFONT, cast(WPARAM, w32.font), 0)
end sub

private sub create_controls()
	const BASE_STYLES = WS_CHILD or WS_VISIBLE or WS_TABSTOP
	for i as integer = 0 to (C__COUNT - 1)
		w32.controls(i) = _
			CreateWindowEx(ctlinfo(i).exstyles, _
			               ctlinfo(i).class, _
			               control_strings(i), _
			               ctlinfo(i).styles or BASE_STYLES, _
			               0, 0, 0, 0, _
			               w32.win, cast(HMENU, i), w32.hinst, 0)
		set_nicer_font(w32.controls(i))
	next
end sub

private sub on_size()
	'' Relayout the controls

	static as RECT rects(0 to (C__COUNT - 1))
	const PAD = 2

	dim as RECT area
	GetClientRect(w32.win, @area)

	dim as integer w = area.right - area.left
	dim as integer h = area.bottom - area.top

	'' Left/right side fairly split (TODO: add splitter control)
	dim as integer half = w / 2

	'' 3 buttons (file/dir/clear) on the left side
	dim as integer leftbutton = (half - (PAD * 4))\ 3

	'' Set each control's x offsets
	dim as integer x = PAD
	rects(C_IN     ).left   = x
	rects(C_FILE   ).left   = x : x += leftbutton
	rects(C_FILE   ).right  = x : x += PAD
	rects(C_DIR    ).left   = x : x += leftbutton
	rects(C_DIR    ).right  = x : x += PAD
	rects(C_CLEAR  ).left   = x : x += leftbutton
	rects(C_CLEAR  ).right  = x
	rects(C_IN     ).right  = x : x = half + PAD
	rects(C_CONCAT ).left   = x
	rects(C_FOLLOW ).left   = x
	rects(C_MERGE  ).left   = x
	rects(C_VERBOSE).left   = x
	rects(C_GO     ).left   = x
	rects(C_OUT    ).left   = x : x = max(x, w)
	rects(C_CONCAT ).right  = x
	rects(C_FOLLOW ).right  = x
	rects(C_MERGE  ).right  = x
	rects(C_VERBOSE).right  = x
	rects(C_GO     ).right  = x
	rects(C_OUT    ).right  = x

	'' Now the y offsets too
	dim as integer buttonheight = w32.lineheight + (w32.lineheight / 2)
	dim as integer checkheight  = w32.lineheight + (w32.lineheight / 4)
	dim as integer y = PAD
	rects(C_FILE   ).top    = y
	rects(C_DIR    ).top    = y
	rects(C_CLEAR  ).top    = y : y += buttonheight
	rects(C_FILE   ).bottom = y
	rects(C_DIR    ).bottom = y
	rects(C_CLEAR  ).bottom = y : y += PAD
	rects(C_IN     ).top    = y : y = PAD
	rects(C_CONCAT ).top    = y : y += checkheight
	rects(C_CONCAT ).bottom = y : y += PAD
	rects(C_FOLLOW ).top    = y : y += checkheight
	rects(C_FOLLOW ).bottom = y : y += PAD
	rects(C_MERGE  ).top    = y : y += checkheight
	rects(C_MERGE  ).bottom = y : y += PAD
	rects(C_VERBOSE).top    = y : y += checkheight
	rects(C_VERBOSE).bottom = y : y += PAD
	rects(C_GO     ).top    = y : y += buttonheight
	rects(C_GO     ).bottom = y : y += PAD
	rects(C_OUT    ).top    = y : y = max(y, h)
	rects(C_OUT    ).bottom = y
	rects(C_IN     ).bottom = y

	dim as double a = timer()
#if 0
	for i as integer = 0 to (C__COUNT - 1)
		MoveWindow(w32.controls(i), _
		           rects(i).left, _
		           rects(i).top, _
		           rects(i).right - rects(i).left, _
		           rects(i).bottom - rects(i).top, _
		           TRUE)
	next
#else
	dim as HDWP defer = BeginDeferWindowPos(C__COUNT)
	for i as integer = 0 to (C__COUNT - 1)
		defer = DeferWindowPos(defer, w32.controls(i), NULL, _
		                       rects(i).left, _
		                       rects(i).top, _
		                       rects(i).right - rects(i).left, _
		                       rects(i).bottom - rects(i).top, _
		                       SWP_NOZORDER)
	next
	EndDeferWindowPos(defer)
#endif
end sub

private function on_message _
	( _
		byval win as HWND, _
		byval message as UINT, _
		byval wparam as WPARAM, _
		byval lparam as LPARAM _
	) as LRESULT

	select case as const (message)
	case WM_CREATE
		w32.win = win

		'' Make the main window use the nice font too, so we can use it
		'' for text measuring without having to worry about
		'' SelectObject()ing the w32.font...
		set_nicer_font(w32.win)
		init_lineheight()

		create_controls()

	case WM_DESTROY
		return 0

	case WM_CTLCOLORBTN, WM_CTLCOLORSTATIC
		return cast(LRESULT, w32.background_brush)

	case WM_SIZE
		if (wparam <> SIZE_MINIMIZED) then
			on_size()
		end if
		return 0

	case WM_THEMECHANGED
		'' Reload the default font
		delete_font()
		init_font()
		init_lineheight()

		'' Relayout the controls
		on_size()

	case WM_QUERYENDSESSION
		return 1

	case WM_CLOSE, WM_ENDSESSION
		gui_quit()
		return 0

	case WM_COMMAND
		if (lparam) then
			gui_on_clicked(loword(wparam))
		end if

	end select

	return DefWindowProc(win, message, wparam, lparam)
end function

private function is_global_message(byval msg as MSG ptr) as integer
	if (msg->message = WM_KEYDOWN) then
		select case (msg->wParam)
		case VK_W, VK_Q
			if (GetKeyState(VK_CONTROL) < 0) then
				gui_quit()
				return TRUE
			end if
		end select
	end if
	return FALSE
end function

function gui_is_checkbox_enabled(byval i as integer) as integer
	return (SendMessage(w32.controls(i), BM_GETCHECK, 0, 0) = BST_CHECKED)
end function

function gui_get_text(byval i as integer) as string
	dim as HWND h = w32.controls(C_IN + i)
	dim as integer length = SendMessage(h, WM_GETTEXTLENGTH, 0, 0)
	dim as zstring ptr s = allocate(length + 1)
	if (s) then
		SendMessage(h, WM_GETTEXT, length + 1, cast(LPARAM, s))
		s[length] = 0
		function = *s
	end if
	deallocate(s)
end function

sub gui_add_text(byval i as integer, byval text as zstring ptr)
	'' Passing a NULL pointer into EM_REPLACESEL causes a crash,
	'' better avoid that...
	if (text = NULL) then return
	if (text[0] = 0) then return

	'' Note: This is used from both the UI thread and the worker thread,
	'' it must use SendMessage() to ensure the edit box recieves the text
	'' pointer while it's valid.
	dim as HWND h = w32.controls(C_IN + i)
	SendMessage(h, EM_SETSEL, -1, -1)
	SendMessage(h, EM_REPLACESEL, FALSE, cast(LPARAM, text))
	SendMessage(h, EM_REPLACESEL, FALSE, cast(LPARAM, @!"\r\n"))
	SendMessage(h, EM_SCROLLCARET, 0, 0)
end sub

sub gui_threadsafe_add_text(byval i as integer, byval text as zstring ptr)
	gui_add_text(i, text)
end sub

sub gui_clear_text(byval i as integer)
	'' Same deal as in gui_add_text()
	SendMessage(w32.controls(C_IN + i), WM_SETTEXT, 0, cast(LPARAM, @""))
end sub

sub gui_threadsafe_clear_text(byval i as integer)
	gui_clear_text(i)
end sub

'' SHBrowseForFolder() callback
private function dir_callback _
	( _
		byval hwnd as HWND, _
		byval message as UINT, _
		byval lParam as LPARAM, _
		byval lpData as LPARAM _
	) as integer
	if (message = BFFM_INITIALIZED) then
		'' Select the path visited the last time
		SendMessage(hwnd, BFFM_SETSELECTION, 1, lpData)
	end if
	return 0
end function

function gui_dialog(byval i as integer) as zstring ptr
	const PATH_SIZE = 1 shl 15
	static as zstring * (PATH_SIZE + 1) path

	path[0] = 0

	select case (i)
	case 0
		dim as OPENFILENAME info
		info.lStructSize = sizeof(info)
		info.hwndOwner = w32.win
		info.lpstrFile = @path
		info.nMaxFile = MAX_PATH + 1
		info.lpstrTitle = dialog_strings(0)
		info.Flags = OFN_ALLOWMULTISELECT or OFN_EXPLORER or OFN_HIDEREADONLY _
			     or OFN_FILEMUSTEXIST or OFN_PATHMUSTEXIST

		print "GetOpenFileName(): ";GetOpenFileName(@info)

	case 1
		static as zstring * (MAX_PATH + 1) lastvisited

		print "<";lastvisited;">"

		dim as BROWSEINFO info
		info.hwndOwner = w32.win
		info.pszDisplayName = @path
		info.lpszTitle = dialog_strings(1)
		info.ulFlags = BIF_USENEWUI or BIF_RETURNFSANCESTORS or BIF_RETURNONLYFSDIRS

		'' If we remembered a path from last time, go back to it
		if (lastvisited[0]) then
			info.lpfn = @dir_callback
			info.lParam = cast(LPARAM, @lastvisited)
		end if

		dim as LPITEMIDLIST id = SHBrowseForFolder(@info)
		if (id) then
			SHGetPathFromIDList(id, @path)
			CoTaskMemFree(id)
		end if

		lastvisited = path

	end select

	print "<";path;">"

	return @path
end function

sub gui_set_window_title(byval text as zstring ptr)
	SetWindowText(w32.win, text)
end sub

sub gui_quit()
	PostQuitMessage(0)
end sub

sub gui_init()
	CoInitialize(NULL) '' For SHBrowseForFolder()

	dim as INITCOMMONCONTROLSEX iccx
	iccx.dwSize = sizeof(iccx)
	iccx.dwICC = 0 '' ICC_STANDARD_CLASSES
	if (InitCommonControlsEx(@iccx) = FALSE) then
		oops_win32("InitCommonControlsEx")
	end if

	w32.hinst = GetModuleHandle(NULL)
	w32.background_brush = GetStockObject(WHITE_BRUSH)
	init_font()

	dim as WNDCLASSEX wc
	wc.cbSize = sizeof(wc)
	wc.lpfnWndProc = @on_message
	wc.hInstance = w32.hinst
	wc.hIcon = load_resource_icon(2, FALSE)
	wc.hCursor = load_cursor()
	wc.hbrBackground = w32.background_brush
	wc.lpszClassName = @"main"
	wc.hIconSm = load_resource_icon(2, TRUE)
	if (RegisterClassEx(@wc) = 0) then
		oops_win32("RegisterClassEx")
	end if

	w32.win = CreateWindowEx(0, "main", NULL, WS_OVERLAPPEDWINDOW, _
	                          CW_USEDEFAULT, CW_USEDEFAULT, 640, 480, _
	                          NULL, NULL, w32.hinst, NULL)
	if (w32.win = NULL) then
		oops_win32("CreateWindowEx")
	end if

	ShowWindow(w32.win, SW_SHOWNORMAL)
end sub

sub gui_run()
	'' Main loop
	do
		dim as MSG msg = any
		dim as integer result = GetMessage(@msg, NULL, 0, 0)

		'' Result is 0 for WM_QUIT and -1 in case of an error
		if (result < 1) then
			if (result > -2) then
				exit do
			end if
		end if

		if (is_global_message(@msg) = FALSE) then
			if (IsDialogMessage(w32.win, @msg) = 0) then
				TranslateMessage(@msg)
				DispatchMessage(@msg)
			end if
		end if
	loop
end sub

sub gui_end()
	DestroyWindow(w32.win)
	delete_font()
end sub
