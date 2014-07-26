struct A1_default { int i; };

#pragma pack(push, 1)
struct A2_field1 { int i; };
#pragma pack(pop)

struct A3_default { int i; };

#pragma pack(push, 2)
struct A4_field2 { int i; };
#pragma pack(push, 4)
struct A5_field4 { int i; };
#pragma pack(push, 8)
struct A6_field8 { int i; };
#pragma pack(pop)
#pragma pack(pop)
#pragma pack(pop)

struct A7_default { int i; };

#pragma pack(push, 1)
struct A8_field1 { int i; };
#pragma pack(4)
struct A9_field4 { int i; };
#pragma pack(pop)

struct A10_default { int i; };

#pragma pack(push, 1)
#pragma pack(push, 2)
struct A11_field2 { int i; };
#pragma pack()
struct A12_default { int i; };
#pragma pack(pop)
struct A13_field1 { int i; };
#pragma pack(pop)
