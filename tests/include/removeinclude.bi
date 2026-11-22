#pragma once

#include once "1.bi"

' @fbfrog -emit '*/removeinclude.h' <dir>removeinclude.bi -removeinclude 2.h -removeinclude does-not-exist.h
