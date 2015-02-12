// @fbfrog -removedefine m

#define m void\
f\
(void);
m
#undef m

#define m void \
f \
(void);
m
#undef m

#define m typedef a\
b;
m
#undef m

#define m typedef a \
b;
m
#undef m

#define m typedef a\
 b;
m
#undef m

#define m typedef a\ 
b;
m
#undef m
