extern "C"

'' @fbfrog -whitespace -nonamefixup -keepundefs

const A = 0
const A = 1
const A = 11
const A = &o0
const A = &o1
const A = &o123
const A = &h0
const A = &h0
const A = &h1
const A = &hFF
const A = 1
const A = 1
const A = 1
const A = 0.1
const A = 0
const A = 1.123
const A = 10
const A = 10
const A = 1
const A = 1
const A = 1
const A = 1
const A = 1

#define B "foo"
#define B wstr( "foo" )
#define B !"foo\n"
#define B """foo"""
#define B asc( "a" )
#define B asc( wstr( "a" ) )

#define C """"
#define C "'"
#define C "'"
#define C "?"
#define C !"\\"
#define C !"\a"
#define C !"\b"
#define C !"\f"
#define C !"\n"
#define C !"\r"
#define C !"\t"
#define C !"\v"

#define C asc( "'" )
#define C asc( """" )
#define C asc( """" )
#define C asc( "?" )
#define C asc( !"\\" )
#define C asc( !"\a" )
#define C asc( !"\b" )
#define C asc( !"\f" )
#define C asc( !"\n" )
#define C asc( !"\r" )
#define C asc( !"\t" )
#define C asc( !"\v" )

#define D !"\0"
#define D !"\0"
#define D !"\0"
#define D !"\0000"
#define D !"\1"
#define D !"\1"
#define D !"\0001"

#define E !"\0a"
#define E !"\0a"
#define E !"\0a"
#define E !"\0g"
#define E !"\0g"
#define E !"\0g"

#define F !"\1"
#define F !"\t"
#define F "I"
#define F "I1"

#define G !"\0010"
#define G !"\0270"
#define G !"\2550"

#define H "a@b"
#define H "1@2"
#define H !"\0"
#define H !"\0"
#define H !"\255"
#define H !"\170"
#define H !"a\0g"

'' Even stray form feed characters

declare sub f()  '' Some escaped newlines 

'' single-line \
'' comment \
'' across \
'' multiple \
'' lines
'' escaped
''
'' \
'' newlines
'' inside\
'' multi-line\
'' comment 

#define A "abcdef"
#define A "abcdef"
#define A ""
#define A ""

end extern
