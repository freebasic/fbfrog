if (1)
	printf("true");

if (1)
	printf("true");
else
	printf("false");

if (1) {
	printf("true");
} else {
	printf("false");
}

if (1)
	;
else
	;

if (1)
	printf("true");
else
	;

if (1)
	printf("true");
else {
	printf("false");
}

if (1) {
	printf("true");
} else
	printf("false");

if (1) {
	printf("true");
	printf("true");
	printf("true");
} else {
	printf("false");
	printf("false");
	printf("false");
}

{
	if (1) printf("true");
	printf("1");
	printf("2");

	if (1) ; else printf("false");
	printf("1");
	printf("2");
}

if ("1") {
	if ("1+.1")
		printf("1+.1+");
	else
		printf("1+.1-");

	if ("1+.2")
		printf("1+.2+");
	else
		printf("1+.2-");
} else {
	if ("1-.1")
		printf("1-.1+");
	else
		printf("1-.1-");

	if ("1-.2")
		printf("1-.2+");
	else
		printf("1-.2-");
}

void f(void) {
	if ("1") {
		if ("1+.1")
			printf("1+.1+");
		else
			printf("1+.1-");

		if ("1+.2")
			printf("1+.2+");
		else
			printf("1+.2-");
	} else {
		if ("1-.1")
			printf("1-.1+");
		else
			printf("1-.1-");

		if ("1-.2")
			printf("1-.2+");
		else
			printf("1-.2-");
	}
}

if (1 == 2);

if (1)
	printf("1");
else if (2)
	printf("2");
else
	printf("3");

if (1)
	printf("1");
else if (2)
	printf("2");
else if (3)
	printf("3");
else if (4)
	printf("4");
else
	printf("5");

if (1)
	printf("1");
else if (2)
	printf("2");

if (1);
else if (2);

if (1);
else if (2);
else;

if (1) {
	printf("1");
} else if (2) {
	printf("2");
} else {
	printf("3");
}

if (1) {
	printf("1");
} else {
	if (2) {
		printf("2.1");
	} else {
		printf("2.2");
	}
}

#define M1 if (1) ; else ;

if (a)
	(b);
