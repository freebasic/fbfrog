#ifndef __VERSION__
	#define __VERSION__ "1"
#endif

#if (__VERSION__ <> "1") and (__VERSION__ <> "2")
	#error "'__VERSION__' is #defined to an unsupported value; expected one of: "1", "2""
#endif

extern a as short

#if __VERSION__ = "1"
	declare sub morev1( )

#else
	declare sub morev2( )

#endif
