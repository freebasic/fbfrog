struct      { int a; } f1(void);
struct UDT2 { int a; } f2(void);

extern struct      { int a; } f3(void);
extern struct UDT4 { int a; } f4(void);

static struct      { int a; } a5, b5;
static struct UDT6 { int a; } a6, b6;

union      { int a; } f7(void);
union UDT8 { int a; } f8(void);

enum       { A = 0 } f9(void);
enum UDT10 { B = 0 } f10(void);

struct UDT11 {
	struct       { int a; } field1;
	struct UDT12 { int a; } field2;

	// Both should be CONST
	struct { int a; } const a, b;
};
