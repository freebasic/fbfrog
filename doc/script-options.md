## About the -declare*/-select/-ifdef options

fbfrog is able to read multiple headers or multiple versions of the same
header (preprocessed differently) and merge them into a single binding.
1. This is used to support multiple targets (DOS/Linux/Win32, x86/x86_64): Instead of looking for #ifs in the input headers and possibly trying to preserve those, fbfrog preprocesses and parses the input header files multiple times (using different predefines each time), and then merges the resulting target-specific APIs into one final binding, by (re-)inserting #ifs (such as `#ifdef __FB_WIN32__`) where needed.
2. By using the `-declare*` command line options you can combine pretty much any APIs, for example version 1.0 and 2.0 of a library, or the ANSI and UNICODE versions of a Win32-specific header. Of course it only makes sense if the APIs belong together. Sometimes the merging algorithm produces a rather ugly result though, especially if the differences between the APIs are too big, so it's not always useful.

Assuming we have the header files foo1.h and foo2.h, let's use the following
fbfrog options:

```
-declareversions __LIBFOO_VERSION 1 2
-selectversion
-case 1
    foo1.h
-case 2
    foo2.h
-endselect
```

Save those options into a foo.fbfrog helper file (because it's too much to
type at the command line), and pass it to fbfrog:

```
./fbfrog foo.fbfrog
```

The created binding will allow the user to #define __LIBFOO_VERSION to 1 or
2 in order to select that specific API version:

```
[...declarations that existed in both foo1.h and foo2.h...]
#if __LIBFOO_VERSION = 1
    [...declarations that existed only in foo1.h...]
#else
    [...declarations that existed only in foo2.h...]
#endif
[...etc...]
```

You can use -declare* options as wanted to support multiple APIs in 1 binding:

```
-declareversions <symbol> <numbers...>
    Useful to allow selecting an API by version. This will produce #if
    blocks such as #if <symbol> = <number>.

-declarebool <symbol>
    Useful to allow API selection based on whether a certain symbol is 
    defined or not. For example, this could be used to support 
    distinguishing between UNICODE and ANSI versions of a binding 
    (-declarebool UNICODE -> #ifdef UNICODE) or the shared library/DLL 
    version and the static library version, etc.
```

If multiple -declare* options are given, they multiply. For example, `-declarebool A -declarebool B` produces these APIs:

```
     defined(A)  and      defined(B)
     defined(A)  and (not defined(B))
(not defined(A)) and      defined(B)
(not defined(A)) and (not defined(B))
```

You can use the -select/-ifdef logic options to create different "code paths"
where some options will only be used for some APIs (instead of applying to
all APIs). This also works with -declare* options, allowing you to build
even complex API condition trees.

