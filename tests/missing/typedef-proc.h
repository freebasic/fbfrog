// @fail
/*
 * typedefs to procdecls (FB doesn't know something like that):
 *
 *    typedef int T(int);
 *    T t;
 *    t(5);
 *
 * Same as this, right?
 *
 *    int t(int);
 *    t(5);
 */
typedef int T(int);
