## Bugs

* Struct decl nested in function pointer parameter triggers assert(): `void (*p)(struct Param { int param; } param);`
* In FB, anon UDTs inherit their parent's FIELD alignment, that's not gcc-compatible.
  fbfrog needs to generate FIELD=8 on anon UDTs if the parent has a FIELD but the anon doesn't.
  http://www.freebasic.net/forum/viewtopic.php?f=3&t=19514
* C parser needs to verify #directives, since they can be inserted by "to c" -replacements,
  which aren't verified by the CPP.
* In winapi, there is a case where an auto-generated tagid conflicts
  with a real typedef, which is errornously renamed. Luckily fbc detects this
  problem easily (recursive UDT).

    ```
    struct Foo {
      struct {
        HWND hwnd;
      } HWND;
    };
    ```

* fbfrog produces `wstr("a") wstr("b")` which isn't allowed in FB; fbfrog needs to insert `+` string concat operators.
