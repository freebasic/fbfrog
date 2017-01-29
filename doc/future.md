## Future improvements

Nowadays, I think it would be best to work on adding a C parser to fbc (i.e. the ability
to #include .h files into FB programs), making fbfrog unnecessary.

* Advantage for users: no more outdated/incompatible/missing bindings, no more binding maintenance.
* Advantage with regards to binding generation: fbc only has to deal with one target system or library version at a time, no more parsing 20 times and slow merging.
* fbc could allow specifying translation hints to handle TODOs if needed. It could come with a set of hints for common libraries. This is the same idea as with fbfrog options.
* Only C-to-FB, no FB-to-C interaction, except maybe trivial #defines to give access to user-configurable parts of the C headers.
