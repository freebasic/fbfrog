// @fbfrog -rename a b

// fbfrog should be able to rename symbols even if they already have an alias,
// and the renamelist should mention the original C identifier, not the alias...

extern int a asm("c");
