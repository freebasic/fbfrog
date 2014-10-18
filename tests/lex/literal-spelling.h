#define stringify(s) #s

static char a1[10] = stringify(0);
static char a2[10] = stringify(00);
static char a3[10] = stringify(0x0);
static char a4[10] = stringify(123);
static char a5[10] = stringify(0X0);

static char dec1[10] = stringify(1);
static char dec2[10] = stringify(1l);
static char dec3[10] = stringify(1ll);
static char dec4[10] = stringify(1u);
static char dec5[10] = stringify(1ul);
static char dec6[10] = stringify(1ull);
static char dec7[10] = stringify(1lu);
static char dec8[10] = stringify(1llu);
static char dec10[10] = stringify(1i8);
static char dec12[10] = stringify(1i16);
static char dec13[10] = stringify(1i32);
static char dec14[10] = stringify(1i64);
static char dec15[10] = stringify(1ui8);
static char dec16[10] = stringify(1ui16);
static char dec17[10] = stringify(1ui32);
static char dec18[10] = stringify(1ui64);

static char oct1[10] = stringify(01);
static char oct2[10] = stringify(01l);
static char oct3[10] = stringify(01ll);
static char oct4[10] = stringify(01u);
static char oct5[10] = stringify(01ul);
static char oct6[10] = stringify(01ull);
static char oct7[10] = stringify(01lu);
static char oct8[10] = stringify(01llu);

static char hex1[10] = stringify(0x1);
static char hex2[10] = stringify(0x1l);
static char hex3[10] = stringify(0x1ll);
static char hex4[10] = stringify(0x1u);
static char hex5[10] = stringify(0x1ul);
static char hex6[10] = stringify(0x1ull);
static char hex7[10] = stringify(0x1lu);
static char hex8[10] = stringify(0x1llu);

static char float1[10] = stringify(1.0);
static char float2[10] = stringify(1.0d);
static char float3[10] = stringify(1.0f);
static char float4[10] = stringify(.1);
static char float5[10] = stringify(1.);
static char float6[10] = stringify(1f);
static char float7[10] = stringify(1d);
