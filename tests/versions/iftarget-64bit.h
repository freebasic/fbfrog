// @fbfrog -iftarget 64bit -define OS my_64bit -else -define OS my_32bit -endif

extern int OS;
