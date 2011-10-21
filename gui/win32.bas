'' Win32 GUI

#define WIN_INCLUDEALL
#include once "windows.bi"
#include once "win/commctrl.bi"
#include once "win/commdlg.bi"
#include once "win/shlobj.bi"

enum
	C_FILE    '' Browse file button
	C_DIR     '' Browse dir button
	C_CLEAR   '' Clear button
	C_IN      '' Input text box
	C_CONCAT  '' Concat checkbox
	C_FOLLOW  '' Follow checkbox
	C_MERGE   '' Merge checkbox
	C_VERBOSE '' Verbose checkbox
	C_GO      '' Go button
	C_OUT     '' Output text box
	C__COUNT
end enum

type ControlInfo
	as uinteger exstyles
	as zstring ptr class
	as zstring ptr text
	as uinteger styles
end type

const TEXTBOX_STYLES = _
		WS_HSCROLL or WS_VSCROLL or _
		ES_AUTOHSCROLL or ES_AUTOVSCROLL or _
		ES_MULTILINE or ES_NOHIDESEL

dim shared as ControlInfo ctlinfo(0 to (C__COUNT - 1)) = _
{ _
	(0, @WC_BUTTON, @"Add file..."     , WS_GROUP), _
	(0, @WC_BUTTON, @"Add directory...", 0), _
	(0, @WC_BUTTON, @"Reset"           , 0), _
	(WS_EX_CLIENTEDGE, @WC_EDIT, NULL , TEXTBOX_STYLES), _
	(0, @WC_BUTTON, @"Concatenate files that are not #included anywhere"           , BS_CHECKBOX or WS_GROUP), _
	(0, @WC_BUTTON, @"Try to find #included files and translate them too"          , BS_CHECKBOX), _
	(0, @WC_BUTTON, @"Replace #includes by the file content if possible"           , BS_CHECKBOX), _
	(0, @WC_BUTTON, @"Show debugging information (file search results, statistics)", BS_CHECKBOX), _
	(0, @WC_BUTTON, @"Translate now!"  , BS_DEFPUSHBUTTON or WS_GROUP), _
	(WS_EX_CLIENTEDGE, @WC_EDIT, NULL , TEXTBOX_STYLES) _
}

type GuiStuff
	as HINSTANCE hinst
	as HFONT font
	as HBRUSH background_brush
	as HWND win
	as integer lineheight '' Height of a line of text
	as HWND controls(0 to (C__COUNT - 1))
end type

dim shared as GuiStuff gui

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
		LoadImage(gui.hinst, MAKEINTRESOURCE(id), _
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
	gui.font = load_default_font()
end sub

private sub delete_font()
	if (gui.font) then
		DeleteObject(gui.font)
	end if
end sub

private sub init_lineheight()
	dim as HDC dc = GetDC(gui.win)

	dim as RECT rc
	DrawText(dc, "TEST", -1, @rc, DT_CALCRECT)

	gui.lineheight = rc.bottom - rc.top

	ReleaseDC(gui.win, dc)
end sub

sub gui_quit()
	PostQuitMessage(0)
end sub

private function browse_dir_callback _
	( _
		byval hwnd as HWND, _
		byval message as UINT, _
		byval lParam as LPARAM, _
		byval lpData as LPARAM _
	) as integer

	''select case (message)
	''case BFFM_INITIALIZED
		''print *cptr(zstring ptr, lpData)
		''SendMessage(hwnd, BFFM_SETSELECTION, 1, lpData)
		''MoveWindow(hwnd, 0, 0, 500, 500, TRUE)
	''end select
	function = 0
end function

private function browse_dialog() as zstring ptr
	static as zstring * (MAX_PATH + 1) folder
	folder[0] = 0

	dim as BROWSEINFO info
	info.hwndOwner = gui.win
	'info.pidlRoot = gui.browsepidl
	info.pszDisplayName = @folder
	info.lpszTitle = @"Select an input directory"
	info.ulFlags = BIF_USENEWUI or BIF_RETURNFSANCESTORS or BIF_RETURNONLYFSDIRS
	info.lpfn = @browse_dir_callback
	''info.lParam = initialdir

	dim as LPITEMIDLIST id = SHBrowseForFolder(@info)
	if (id) then
		SHGetPathFromIDList(id, @folder)
		CoTaskMemFree(id)
	end if

	return @folder
end function

private function browse_file() as zstring ptr
	static as zstring * (MAX_PATH + 1) filename
	filename[0] = 0

	dim as OPENFILENAME info
	info.lStructSize = sizeof(info)
	info.hwndOwner = gui.win
	info.lpstrFile = @filename
	info.nMaxFile = MAX_PATH + 1
	info.lpstrTitle = @"Select an input file"
	info.Flags = OFN_ALLOWMULTISELECT or OFN_EXPLORER or OFN_HIDEREADONLY _
	             or OFN_FILEMUSTEXIST or OFN_PATHMUSTEXIST

	GetOpenFileName(@info)

	return @filename
end function

private sub set_nicer_font(byval h as HWND)
	SendMessage(h, WM_SETFONT, cast(WPARAM, gui.font), 0)
end sub

private sub create_controls()
	const BASE_STYLES = WS_CHILD or WS_VISIBLE or WS_TABSTOP
	for i as integer = 0 to (C__COUNT - 1)
		gui.controls(i) = _
			CreateWindowEx(ctlinfo(i).exstyles, _
			               ctlinfo(i).class, _
			               ctlinfo(i).text, _
			               ctlinfo(i).styles or BASE_STYLES, _
			               0, 0, 0, 0, _
			               gui.win, 0, gui.hinst, 0)
		set_nicer_font(gui.controls(i))
	next
end sub

private sub on_size()
	'' Relayout the controls

	static as RECT rects(0 to (C__COUNT - 1))
	const PAD = 2

	dim as RECT area
	GetClientRect(gui.win, @area)

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
	rects(C_CLEAR  ).right  = x : x = half
	rects(C_IN     ).right  = x : x += PAD
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
	dim as integer buttonheight = gui.lineheight + (gui.lineheight / 2)
	dim as integer checkheight  = gui.lineheight + (gui.lineheight / 4)
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
		MoveWindow(gui.controls(i), _
		           rects(i).left, _
		           rects(i).top, _
		           rects(i).right - rects(i).left, _
		           rects(i).bottom - rects(i).top, _
		           TRUE)
	next
#else
	dim as HDWP defer = BeginDeferWindowPos(C__COUNT)
	for i as integer = 0 to (C__COUNT - 1)
		defer = DeferWindowPos(defer, gui.controls(i), NULL, _
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
		gui.win = win

		'' Make the main window use the nice font too, so we can use it
		'' for text measuring without having to worry about
		'' SelectObject()ing the gui.font...
		set_nicer_font(gui.win)
		init_lineheight()

		create_controls()

	case WM_ERASEBKGND
		return 1

	case WM_PAINT
		dim as PAINTSTRUCT pnt = any
		BeginPaint(win, @pnt)

		SetBkColor(pnt.hdc, GetSysColor(COLOR_WINDOW))
		ExtTextOut(pnt.hdc, 0, 0, ETO_OPAQUE, @pnt.rcPaint, NULL, 0, NULL)

		EndPaint(win, @pnt)
		return 0

	case WM_CTLCOLORBTN, WM_CTLCOLORSTATIC
		return cast(LRESULT, gui.background_brush)

	case WM_SIZE
		if (wparam <> SIZE_MINIMIZED) then
			on_size()
		end if
		return 0

	case WM_QUERYENDSESSION
		return 1

	case WM_CLOSE, WM_ENDSESSION
		gui_quit()
		return 0

	case WM_DESTROY
		return 0

	case WM_GETMINMAXINFO

	case WM_WINDOWPOSCHANGING

	case WM_THEMECHANGED
		'' Reload the default font
		delete_font()
		init_font()
		init_lineheight()

		'' Relayout the controls
		on_size()

	''case WM_ACTIVATE
		''is_minimized = (HIWORD(wParam) <> 0)
		''on_activated((LOWORD(wParam) <> WA_INACTIVE))

	end select

	return DefWindowProc(win, message, wparam, lparam)
end function

private function ctrl_pressed() as integer
	return (GetKeyState(VK_CONTROL) < 0)
end function

private function on_global_keydown(byval vk as uinteger) as integer
	select case as const (vk)
	case VK_W, VK_Q
		if (ctrl_pressed()) then
			gui_quit()
			return TRUE
		end if

	end select

	return FALSE
end function

private function is_global_message(byval msg as MSG ptr) as integer
	if (msg->message = WM_KEYDOWN) then
		return on_global_keydown(msg->wParam)
	end if
	return FALSE
end function

private sub init_window_class()
	dim as WNDCLASSEX wc
	wc.cbSize = sizeof(wc)
	wc.lpfnWndProc = @on_message
	wc.hInstance = gui.hinst
	wc.hIcon = load_resource_icon(2, FALSE)
	wc.hCursor = load_cursor()
	wc.hbrBackground = gui.background_brush
	wc.lpszClassName = @"main"
	wc.hIconSm = load_resource_icon(2, TRUE)

	if (RegisterClassEx(@wc) = 0) then
		oops_win32("RegisterClassEx")
	end if
end sub

private sub init_comctl()
	dim as INITCOMMONCONTROLSEX iccx
	iccx.dwSize = sizeof(iccx)
	iccx.dwICC = 0 '' ICC_STANDARD_CLASSES
	if (InitCommonControlsEx(@iccx) = FALSE) then
		oops_win32("InitCommonControlsEx")
	end if
end sub

	CoInitialize(NULL) '' For SHBrowseForFolder()
	init_comctl()

	gui.hinst = GetModuleHandle(NULL)
	gui.background_brush = GetStockObject(WHITE_BRUSH)
	init_font()

	init_window_class()

	gui.win = CreateWindowEx(0, "main", NULL, WS_OVERLAPPEDWINDOW, _
	                          CW_USEDEFAULT, CW_USEDEFAULT, 640, 480, _
	                          NULL, NULL, gui.hinst, NULL)
	if (gui.win = NULL) then
		oops_win32("CreateWindowEx")
	end if

	''SendMessage(gui.win, WM_SETICON, ICON_BIG  , cast(LPARAM, big))
	''SendMessage(gui.win, WM_SETICON, ICON_SMALL, cast(LPARAM, small))

	ShowWindow(gui.win, SW_SHOWNORMAL)

	SetWindowText(gui.win, "fbfrog test")

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
			if (IsDialogMessage(gui.win, @msg) = 0) then
				TranslateMessage(@msg)
				DispatchMessage(@msg)
			end if
		end if
	loop

	DestroyWindow(gui.win)
	delete_font()
