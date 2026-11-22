#pragma once

' Expansion after pasting
#define m1(x) m##x
#define m2 x2
#define m3 x3
dim shared x2 as long ' -> m2 -> x2
dim shared x3 as long ' -> m3 -> x3
