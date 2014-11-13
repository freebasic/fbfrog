// @fbfrog -fixunsizedarrays

extern int array1[];   // Needs to be fixed up, if size is unknown
extern int array2[10]; // Size known, do nothing

// Same
static int array3[];
static int array4[10];

// Conflict with FB keyword too
extern int string[];
