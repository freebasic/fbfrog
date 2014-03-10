extern "C"

'' void f1(char pEXPANDME2[32] = "EXPANDME2");
'' void f2(char EXPANDME2p[32] = "EXPANDME2");

declare sub f1(byval pEXPANDME2 as zstring ptr = "EXPANDME2")
declare sub f2(byval EXPANDME2p as zstring ptr = "EXPANDME2")
declare sub f3(byval pEXPANDME2x as zstring ptr = "EXPANDME2")

end extern
