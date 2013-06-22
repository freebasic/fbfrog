enum
	A = 0
	B
	'' TODO: unknown construct
	C = (1 << 4),
	D
	E = 1
	F
	G
end enum

enum E
	A
	#if 1
		B
	#endif
	C
end enum

enum
	A = 1
	B
end enum

enum E
	A
	B
end enum
enum E
	A
	B
end enum
enum E
	A
	B
end enum
