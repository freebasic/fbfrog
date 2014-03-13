// @fail @fbfrog -removedefine m
// undefined behaviour
#define m a ## ## b
m
