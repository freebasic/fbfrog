// @fbfrog -removedefine API

#ifdef _WIN32
	#define API __declspec(dllimport)
#else
	#define API
#endif

API void f(void);
