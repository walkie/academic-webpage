Variational programming supports efficiently executing many related programs at
once by encoding all of the programs in one "variational program" that captures
the differences among them statically and explicitly. An open problem in
variational programming is how to handle side effects--if two program variants
perform different side effects, we cannot separate the effect of one variant
from the other since the outside world is not variational. A potential solution
is to create variation-aware execution environments for variational programs,
for example, a variational file system that keeps track of file variants
corresponding to program variants. However, it is infeasible to do this for all
kinds of effects. Also, there are different ways to handle the interaction of
effects and variation that are incompatible with each other, preventing a
one-size-fits-all solution. In this thesis, we argue that algebraic effects can
be used to resolve the problem of combining variation and effects by enabling
programmers to flexibly and incrementally extend a variational programming
environment to handle new kinds of effects. We present a proof-of-concept
prototype in the Eff programming language that demonstrates how a variational
programming environment can be extended to support file input/output.
Crucially, such extensions are done at the library level, which enables
handling new kinds of effects and handling existing effects in multiple ways,
both of which are essential in variational programming applications.
