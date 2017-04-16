void f(int& ref);
void f(const int& ref);
void f(int const& ref);

extern int &ref;
extern int &(ref);
extern int (&ref);
extern int (&(ref));

extern int*& ref_to_ptr;

static void (*functionptr)(void);
static void (*&ref_to_functionptr)(void) = functionptr;
static void (*&(ref_to_functionptr))(void) = functionptr;
static void (*(&ref_to_functionptr))(void) = functionptr;
static void (*(&(ref_to_functionptr)))(void) = functionptr;

static functionresult (*functionptr)(void);
static functionresult (*&ref_to_functionptr)(void) = functionptr;
static functionresult (*&(ref_to_functionptr))(void) = functionptr;
static functionresult (*(&ref_to_functionptr))(void) = functionptr;
static functionresult (*(&(ref_to_functionptr)))(void) = functionptr;
