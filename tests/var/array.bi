dim shared a(0 to 1 - 1) as long
dim shared a(0 to 10 - 1) as long
dim shared a(0 to 2 - 1, 0 to 2 - 1) as long
dim shared a(0 to 2 - 1, 0 to 2 - 1, 0 to 2 - 1) as long
dim shared a(0 to 2 - 1, 0 to 3 - 1, 0 to 4 - 1, 0 to 5 - 1, 0 to 6 - 1) as long
dim shared p(0 to 4 - 1) as sub( )
dim shared p(0 to 2 - 1, 0 to 3 - 1) as sub( )

extern     a(0 to 10 - 1) as long
dim shared a(0 to 10 - 1) as long
extern a(0 to 10 - 1) as long

dim shared a(0 to (((10 + 10) + (10 * 2)) + 1) - 1) as long
