// @fbfrog -nofunctionbodies

int f1(void) {
	// One that can be parsed just fine
	return 123;
}

int f2(int i) {
	// One with a parser error
	return i++;
}
