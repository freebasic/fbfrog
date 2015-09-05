do ; while (1);

do
	printf("1");
while (1);

do {
	printf("1");
} while (1);

do {
	printf("1");
	printf("2");
	printf("3");
} while (1);

do {
	printf("1");
} while (0);

if (1) {
	do {
		printf("1");
	} while (0);
}

while (1);

while (1)
	printf("1");

while (1) {
	printf("1");
}

while (1) {
	printf("1");
	printf("2");
	printf("3");
}

while (0) {
	printf("1");
}

if (1) {
	while (0) {
		printf("1");
	}
}

#define M1 do { printf("foo"); } while (0)
#define M2 do { printf("foo"); } while (1)
#define M3 while (0) { printf("foo"); }
#define M4 while (1) { printf("foo"); }

do ; while (a == b);
while (a == b);

while (a)
	(b);
