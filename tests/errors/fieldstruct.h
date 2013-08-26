/* Named nested unions/structs -- not supported in FB */
struct T {
	union {
		int a;
	} foo;
};
