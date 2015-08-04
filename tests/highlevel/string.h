// @fbfrog -string directByteArrayToString -string directUbyteArrayToString -string indirectByteArrayToString -string indirectUbyteArrayToString -string p1 -string p4 -string p5 -nostring CHAR -nostring WCHAR -string charArray -string stringField -string wcharArray

extern signed char directByteArrayToString[10];
extern signed char directByteArrayLeftAsIs[10];
extern unsigned char directUbyteArrayToString[10];
extern unsigned char directUbyteArrayLeftAsIs[10];

typedef unsigned char mybyte;
typedef unsigned char myubyte;
extern mybyte indirectByteArrayToString[10];
extern mybyte indirectByteArrayLeftAsIs[10];
extern myubyte indirectUbyteArrayToString[10];
extern myubyte indirectUbyteArrayLeftAsIs[10];

void f1(signed char *p1, unsigned char *p2);
void f2(signed char *p3, unsigned char *p4);
void f3(myubyte *p5);

typedef char CHAR;
extern CHAR singleChar;
extern CHAR charArray[10];

typedef CHAR CHAR2;
struct charUDT {
	CHAR2 bufferField[10];
	CHAR2 stringField[10];
};

typedef wchar_t WCHAR;
extern WCHAR singleWChar;
extern WCHAR wcharArray[10];

typedef WCHAR WCHAR2;
struct wcharUDT {
	WCHAR2 bufferField[10];
	WCHAR2 stringField[10];
};
