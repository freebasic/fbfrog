#if defined(__FB_WIN32__) or defined(__FB_DOS__)
	const HOST_PATHDIV = $"\"
	#ifdef ENABLE_COMMON_PATHDIV
		'' use forward slash internally, even on windows
		'' this helps with compatibility in shell scripts
		const PATHDIV = "/"
	#else
		const PATHDIV = HOST_PATHDIV
	#endif
#else
	const HOST_PATHDIV = "/"
	const PATHDIV = HOST_PATHDIV
#endif

declare function pathStripExt(byref path as const string) as string
declare function pathExtOnly(byref path as const string) as string
declare function pathOnly(byref path as const string) as string
declare function pathStrip(byref path as const string) as string
declare function pathAddDiv(byref path as const string) as string
declare function pathIsAbsolute(byref s as const string) as integer
declare function pathMakeAbsolute(byref path as const string) as string
declare function hExepath() as string
declare function hCurdir() as string
declare function pathStripCurdir(byref path as const string) as string
declare function hReadableDirExists(byref path as const string) as integer
declare function pathIsDir(byref s as const string) as integer
declare function pathNormalize(byref path as const string) as string
declare function pathNormalizePathDiv(byref path as const string) as string