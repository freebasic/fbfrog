void f1(void) {
}

void f2(void) {
	int i = 1;
	printf("%i\n", 1);
}

int f3(int i) {
	return i << 3 & 0xFF;
	return 0;
}

// Just a RETURN
void f4(void) {
	return;
}

// Anonymous parameter + body
int f(int) {
	f(0);
	return 0;
}

int f(void) { f(); }
void f(void) { f(); }
int f(int i) { return i; }
int f(int i) { return i + i; }
int f(int a, int b) { return a + b; }
int f(int a, int b) { return a + b + b; }
int f(int i) { return i * 4; }
int f(int i) { return sizeof(i); }
