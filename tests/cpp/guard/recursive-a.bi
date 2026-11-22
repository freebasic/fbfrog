#pragma once

' a.h doesn't have an #include guard, but we won't know until seeing that we
' don't reach EOF after the #endif. In the meantime, the inclusion of b.h
' triggers a recursive inclusion of a.h. At that point we should not think
' that a.h has an #include guard (because we haven't finished checking a.h).
#define A
' @ignore
' a.h doesn't have an #include guard, but we won't know until seeing that we
' don't reach EOF after the #endif. In the meantime, the inclusion of b.h
' triggers a recursive inclusion of a.h. At that point we should not think
' that a.h has an #include guard (because we haven't finished checking a.h).

#inclib "a"
#inclib "b"
#inclib "a"
