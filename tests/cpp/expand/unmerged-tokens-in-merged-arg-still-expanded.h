// @fbfrog -removedefine m2

#define identifier a
#define m1(x) stat##x
m1(ic int identifier;)

#define datatype int
#define m2(x) x##123
m2(static datatype foo);
