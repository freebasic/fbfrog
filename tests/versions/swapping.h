// The #if block generation should be smart enough to prefer simple #if#
// expressions over complex ones. (in case of a #if/#else block there are
// two code paths, and we can choose which of them to test for in the #if)

#ifdef _WIN32
	void win32_1(void);
#else
	void rest_1(void);
#endif

extern int separator;

#if defined __unix__ || defined __DJGPP__
	void unixdos_2(void);
#else
	void rest_2(void);
#endif

extern int separator;

// But not if there's #elseif's involved, because then the order of the blocks matters.
// (maybe not if only checking exclusively #defined symbols, but it can matter in other cases,
// so for now it's just safer not to do anything)
#ifdef _WIN32
	void win32_3(void);
#elif __DJGPP__
	void dos_3(void);
#else
	void rest_3(void);
#endif

extern int separator;

// E.g. here it matters, because we check for Linux before checking for Unix,
// but Linux defines __unix__ too. So here those blocks really mustn't be swapped.
#if defined __linux__ || defined _WIN32 || defined __CYGWIN__
	void linux_win32_cygwin_4(void);
#elif __unix__
	void unix_4(void);
#else
	void rest_4(void);
#endif
