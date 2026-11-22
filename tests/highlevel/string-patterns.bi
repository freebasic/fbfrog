#pragma once

extern "C"

' @fbfrog -string 'string*' -nostring 'byteArray*' -string UDT1.name -string UDT*.my*Path -string f1.param1 -string f3.0 -string f5.1 -string PFDoodle.1 -string procPtrParam.1 -string f7*
extern string1 as zstring * 10
extern string2 as zstring * 10
extern string3 as zstring * 10
extern byteArray1(0 to 9) as byte
extern byteArray2(0 to 9) as byte
extern byteArray3(0 to 9) as byte

type UDT1
	name as zstring * 10 ' should be matched
	forComparison(0 to 9) as byte
	myPath as zstring * 10 ' should be matched
end type

type UDT2
	name(0 to 9) as byte ' shouldn't be matched
	myFullPath as zstring * 10 ' should be matched
end type

extern myPartialPath(0 to 9) as byte ' shouldn't be matched
'' TODO: void f1(unsigned char *param1 [TK_FBCODE /' should be matched '/]);
'' TODO: void f2(unsigned char *param1 [TK_FBCODE /' shouldn't be matched '/]);
declare sub f3(byval as zstring ptr) ' should be matched
declare sub f4(byval as ubyte ptr) ' shouldn't be matched
declare sub f5(byval as ubyte ptr, byval as zstring ptr, byval as ubyte ptr) ' matching 2nd parameter only
type PFDoodle as sub(byval as ubyte ptr, byval as zstring ptr) ' match 2nd parameter of procptr subtype of a typedef
declare sub f6(byval as long, byval procPtrParam as sub(byval as ubyte ptr, byval as zstring ptr), byval as long) ' same, but as param instead of typedef
'' TODO: void f7(unsigned char *param [TK_FBCODE /' shouldn't be matched '/]);

end extern
