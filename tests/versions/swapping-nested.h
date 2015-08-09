// @fbfrog -declarebool UNICODE -ifdef UNICODE -define UNICODE 1 -endif -target windows
// When swapping verblocks (or really not only then, but in general) we shouldn't
// forget about processing nested verblocks.

#ifdef __x86_64__
	#pragma pack(push, 1)
#endif

struct UDT {
	#ifdef UNICODE
		int unicode;
	#else
		int ansi;
	#endif
};
