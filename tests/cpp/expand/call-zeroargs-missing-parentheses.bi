#pragma once

#define m1() 123
' m1 should not be expanded here, missing "()" for that
dim shared m1 as long
