## fbfrog: A binding generator for FreeBASIC

fbfrog is a command line tool which reads .h files (C API declarations) and generates corresponding .bi file(s) (FreeBASIC API declarations).
This tool was needed because the FreeBASIC Compiler is lacking the ability to #include C headers directly,
and instead requires all data type and function declarations to be translated to the FB syntax. At least fbc is ABI-compatible to gcc,
so only the translated headers are needed, no binary wrappers (for C++ bindings it's a different story though). fbfrog doesn't support C++, except for some simple cases like reference parameters.

1. Compile fbfrog via `make` or `fbc *.bas -m fbfrog`. The test suite can be run via `make tests` and then `git diff`.
2. Run fbfrog and pass the *.h file(s) that you want to #include on the command line:

    ```
    ./fbfrog foo.h
    ```

3. Check the produced .bi file. It needs to be reviewed and tested. TODOs may have
   to be fixed up manually, or require special options like `-incdir` (to find missing #includes)
   or `-define WINAPI __stdcall` (to expand missing preprocessor macros), and re-run.
   Often TODOs are caused by untranslatable #defines. If not needed, they can be ignored
   or removed with `-removedefine`.


## Examples


#### zlib

```
wget http://zlib.net/zlib-1.2.11.tar.xz
tar xf zlib-1.2.11.tar.xz

./fbfrog zlib-1.2.11/zlib.h \
  -renametypedef Byte Byte_ \
  -renametypedef uLong uLong_ \
  -renamedefine zlib_version zlib_version_
```

#### bzip2 library

```
wget http://www.bzip.org/1.0.6/bzip2-1.0.6.tar.gz
tar xf bzip2-1.0.6.tar.gz

./fbfrog bzip2-1.0.6/bzlib.h \
  -define WINAPI __stdcall \
  -removeinclude windows.h \
  -renameproc BZ2_bzread BZ2_bzread_ \
  -renameproc BZ2_bzwrite BZ2_bzwrite_
```

#### freeglut library

```
wget http://sourceforge.net/projects/freeglut/files/freeglut/3.0.0/freeglut-3.0.0.tar.gz/download -O freeglut-3.0.0.tar.gz
tar xf freeglut-3.0.0.tar.gz

./fbfrog \
  -incdir freeglut-3.0.0/include \
  -include GL/freeglut.h \
  \
  -emit '*/GL/freeglut.h'     freeglut.bi \
  -emit '*/GL/freeglut_ext.h' freeglut_ext.bi \
  -emit '*/GL/freeglut_std.h' freeglut_std.bi \
  \
  -define FREEGLUT_LIB_PRAGMAS 1     \
  -define GLUT_DISABLE_ATEXIT_HACK 1 \
  -define NDEBUG 1                   \
  -iftarget windows                  \
    -declarebool FREEGLUT_STATIC     \
    -ifdef FREEGLUT_STATIC           \
      -define FREEGLUT_STATIC 1      \
    -endif                           \
  -endif
```

## Additional information

* [Command Line Options](doc/options.md)
* [Overview](doc/overview.md)
* [Automatic Translation Features](doc/automatic-translation-features.md)
* [Extra Features](doc/extra-features.md)
* [About the -declare*/-select/-ifdef options](doc/script-options.md)
* [Bugs](doc/bugs.md)
* [Old to-do list](doc/todo.md)
* [Future improvements](doc/future.md)
