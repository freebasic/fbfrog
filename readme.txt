FB frog - the wacky h2bi type of translator

We should read in all .h's from the command line and spit out corresponding .bi
files. Preprocessor directives need to be preserved as well as possible, and
we should not do macro expansion, so a real C parser probably can't be used.

We need to use a tree to hold all code before emitting it, in order to be able
to rearrange things (like #defines in between function parameters, which isn't
possible in FB) or delay emitting (like typedefs/structs that have their name
listed after the fields, while in FB the UDT name comes first).

While we should be able to produce the same results no matter whether working
on one or many .h's at once, we should also consider saying "passing all .h's
gives us more knowledge and may result in a better translation". That means the
intermediate tree of each .h would be kept around until all .h's are read in,
and only then would it be emitted. We could use that to improve automatic
decision making, e.g. if a struct foo is only ever used as 'struct foo', and
there is no typedef for it, then we can just emit it as "type foo", instead of
emitting "struct_foo" everywhere in order to avoid conflicting with a typedef.

We should have tests, i.e. pairs of input & output headers. We may need an
extra command line option to prevent all kinds of "dynamic" notes/warnings
that we may add to the output, otherwise the tests would too easily break.
