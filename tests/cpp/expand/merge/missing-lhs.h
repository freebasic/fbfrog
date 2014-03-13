// @fail @fbfrog -removedefine m
#define m(x) ##x
m(0)
