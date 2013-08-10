extern separator1 as short

#if 1 andalso defined( foo )
#endif
extern separator2 as short

#if 0 orelse defined( foo )
#endif
extern separator3 as short

extern yes as short
extern separator4 as short

extern yes as short
extern separator5 as short

#if defined( FOO ) andalso (FOO = 123)
#endif
extern separator6 as short
