#pragma once

extern "C"

'' @fbfrog -whitespace -nonamefixup -removedefine m

declare sub f()

declare sub foo()

'' The "(...)" following the macro id isn't a parameter list, as it's separated

declare sub f()

end extern
