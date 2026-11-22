#pragma once

#include once "other1.bi"
#include once "other2.bi"

' @fbfrog -emit '*/main.h' <dir>main.bi -emit '*/other1.h' <dir>other1.bi -emit '*/other2.h' <dir>other2.bi
const A1 = 123
const A2 = A1
