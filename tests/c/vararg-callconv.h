// vararg functions are always cdecl; the __stdcall attribute should be ignored here
__attribute__((stdcall)) void f(int n, ...);
