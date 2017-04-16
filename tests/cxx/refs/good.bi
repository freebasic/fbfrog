#pragma once

extern "C"

declare sub f(byref ref as long)
declare sub f(byref ref as const long)
declare sub f(byref ref as const long)

extern byref ref as long
extern byref ref as long
extern byref ref as long
extern byref ref as long
extern byref ref_to_ptr as long ptr
dim shared functionptr as sub()
dim shared byref ref_to_functionptr as sub() = functionptr
dim shared byref ref_to_functionptr as sub() = functionptr
dim shared byref ref_to_functionptr as sub() = functionptr
dim shared byref ref_to_functionptr as sub() = functionptr
dim shared functionptr as function() as functionresult
dim shared byref ref_to_functionptr as function() as functionresult = functionptr
dim shared byref ref_to_functionptr as function() as functionresult = functionptr
dim shared byref ref_to_functionptr as function() as functionresult = functionptr
dim shared byref ref_to_functionptr as function() as functionresult = functionptr

end extern
