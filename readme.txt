FB frog - the wacky h2bi type of translator

We should:
> Read in all .h's from the command line and spit out corresponding .bi's
> Not have configuration options (always do the right thing automatically)
> Preserve preprocessor directives/unexpanded macros (can't use a real C parser)
> Have some sort of intermediate form to be able to rearrange things,
  e.g. #defines intermixed in a declaration
