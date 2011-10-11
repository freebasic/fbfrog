// type T as T_
// (this way we only need to translate the <struct T ...> body as
// <type T_ ...>, that's easier than inserting T_ fwdref in place of T
// everywhere where it's used before the T body)
struct T;
