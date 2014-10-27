// @fbfrog -v

#if unexpanded
#endif

#ifdef A
#endif

#ifndef B
#endif

#if defined C
#endif

#if !defined D
#endif

#if 0 && defined E
#endif

#if 1 || defined F
#endif

#if 1 ? defined G : defined H
#endif

#ifdef __NOTREPORTED
#endif

#ifdef _NotReported
#endif

#ifdef _reported
#endif
