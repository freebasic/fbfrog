// @fail @fbfrog -removedefine comma
#define comma ,
#define m(x, y)

// "comma" shouldn't be expanded before parsing the arg list, there's only 1
// arg given here, but 2 required.
m(1 comma 2)
