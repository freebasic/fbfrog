// @fbfrog -renameproc A1 A2

void A1(void);

// procptr subtypes shouldn't cause issues
extern void (*p)(void);
