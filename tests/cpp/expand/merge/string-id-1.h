// @fail @fbfrog -removedefine m
#define m(l, r) #l ## r
m(a, b)
