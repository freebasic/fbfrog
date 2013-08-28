#define EXPANDME1 void\
f\
(void);
EXPANDME1

#define EXPANDME2 void \
f \
(void);
EXPANDME2

#define EXPANDME3 typedef a\
b;
EXPANDME3

#define EXPANDME4 typedef a \
b;
EXPANDME4

#define EXPANDME5 typedef a\
 b;
EXPANDME5

#define EXPANDME6 typedef a\ 
b;
EXPANDME6
