#pragma once

' Indirect recursion
#define m1 m2
#define m2 m1
dim shared m1 as long ' -> m2 -> m1
dim shared m2 as long ' -> m1 -> m2
