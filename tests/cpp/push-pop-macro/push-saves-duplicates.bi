#pragma once

const A = 1
' Save A 1
#undef A
const A = 2
' Save A 2
' Save A 2 again
dim shared a_2 as long = 2
#undef A
const A = 3

dim shared b_3 as long = 3
' Restore to second A 2
dim shared c_2 as long = 2
' Restore to first A 2
dim shared d_2 as long = 2
' Restore to A 1
dim shared e_1 as long = 1
