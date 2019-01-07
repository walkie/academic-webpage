Changing a program in response to a type error plays an important
part in modern software development. However, the generation of good
type error messages remains a problem for highly expressive type
systems.
Existing approaches often suffer from a lack of precision in locating errors
and proposing remedies. Specifically, they either fail to locate the source of
the type error consistently, or they report too many potential error
locations. Moreover, the change suggestions offered are often incorrect. This
makes the debugging process tedious and ineffective.

We present an approach to the problem of type debugging that is based on
generating and filtering a comprehensive set of type-change suggestions.
Specifically, we generate <i>all</i> (program-structure-preserving) type
changes that can possibly fix the type error. These suggestions will be ranked
and presented to the programmer in an iterative fashion. In some cases we
also produce suggestions to change the program.
In most situations, this strategy delivers the correct change suggestions
quickly, and at the same time never misses any rare suggestions.
The computation of the potentially huge set of type-change suggestions is
efficient since it is based on a variational type inference algorithm that
type checks a program with variations only once, efficiently reusing type
information for shared parts.

We have evaluated our method and compared it with previous approaches. Based
on a large set of examples drawn from the literature, we have found that our
method outperforms other approaches and provides a viable alternative.