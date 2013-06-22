#if 0
	extern     a1 as short
	dim shared a1 as short
#endif
extern     separator1 as short
dim shared separator1 as short

#if 1
	extern     a1 as short
	dim shared a1 as short
#endif
extern     separator2 as short
dim shared separator2 as short

#if 0
	extern     a1 as short
	dim shared a1 as short
#else
	extern     b1 as short
	dim shared b1 as short
#endif
extern     separator3 as short
dim shared separator3 as short

#if 1
	extern     a1 as short
	dim shared a1 as short
#else
	extern     b1 as short
	dim shared b1 as short
#endif
extern     separator4 as short
dim shared separator4 as short

#if 0
	extern     a1 as short
	dim shared a1 as short
#elseif 0
	extern     b1 as short
	dim shared b1 as short
#endif
extern     separator5 as short
dim shared separator5 as short

#if 0
	extern     a1 as short
	dim shared a1 as short
#elseif 1
	extern     b1 as short
	dim shared b1 as short
#endif
extern     separator6 as short
dim shared separator6 as short

#if 1
	extern     a1 as short
	dim shared a1 as short
#elseif 0
	extern     b1 as short
	dim shared b1 as short
#endif
extern     separator7 as short
dim shared separator7 as short

#if 1
	extern     a1 as short
	dim shared a1 as short
#elseif 1
	extern     b1 as short
	dim shared b1 as short
#endif
extern     separator8 as short
dim shared separator8 as short

#if 0
	extern     a1 as short
	dim shared a1 as short
#elseif 0
	extern     b1 as short
	dim shared b1 as short
#else
	extern     c1 as short
	dim shared c1 as short
#endif
extern     separator9 as short
dim shared separator9 as short

#if 0
	extern     a1 as short
	dim shared a1 as short
#elseif 1
	extern     b1 as short
	dim shared b1 as short
#else
	extern     c1 as short
	dim shared c1 as short
#endif
extern     separator10 as short
dim shared separator10 as short

#if 1
	extern     a1 as short
	dim shared a1 as short
#elseif 0
	extern     b1 as short
	dim shared b1 as short
#else
	extern     c1 as short
	dim shared c1 as short
#endif
extern     separator11 as short
dim shared separator11 as short

#if 1
	extern     a1 as short
	dim shared a1 as short
#elseif 1
	extern     b1 as short
	dim shared b1 as short
#else
	extern     c1 as short
	dim shared c1 as short
#endif
extern     separator12 as short
dim shared separator12 as short

#if 1
	#if 0
		'' TODO: unknown construct
		a
	#endif
#elseif 0
	'' TODO: unknown construct
	b
#else
	'' TODO: unknown construct
	c
#endif
