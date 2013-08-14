__attribute__((noreturn)) void f(void);
void __attribute__((noreturn)) f(void);
void f(void) __attribute__((noreturn));
void (__attribute__((noreturn)) f)(void);

__attribute__((warn_unused_result))     void f(void);
__attribute__((__warn_unused_result__)) void f(void);

__attribute__((noreturn))     void f(void);
__attribute__((__noreturn__)) void f(void);

__attribute__((malloc))     void f(void);
__attribute__((__malloc__)) void f(void);

__attribute__((deprecated))     void f(void);
__attribute__((__deprecated__)) void f(void);
