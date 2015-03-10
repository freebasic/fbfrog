// Callconvs inside macro bodies mustn't be hidden, because the macro could be
// expanded outside the header's Extern block.
#define A ((void (*)(void)) getFunctionPtr())
