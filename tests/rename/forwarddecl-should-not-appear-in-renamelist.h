// @fbfrog -rename Unrelated Unrelated_

// A forward declaration will be added for "UDT" -- and it shouldn't be added
// to the renamelist just because there is some -rename option.

extern struct UDT *p;

struct UDT {
	int dummy;
};
