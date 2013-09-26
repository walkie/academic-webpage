---
title: Choice Calculus
---

# Choice Calculus

The choice calculus is a simple, formal language for representing *variation*.
It supports research in areas that deal with variation in software, such as
[software product lines](http://en.wikipedia.org/wiki/Software_product_lines).
Within this domain, it intends to fulfill a role similar to the lambda calculus
in programming languages research, providing a minimal basis for communicating
ideas and sharing results.

The core choice calculus is very small---just two constructs, *choices* and
*tree nodes*---but it can be easily extended with new language features that
support different ways of developing, maintaining, and analyzing variability.
The core choice calculus is also generic, emphasizing that variation can
often be treated orthogonally to other concerns, but it can be instantiated
by different object languages and data types when they interact fundamentally
with variability.

The primary reference for the choice calculus is the original TOSEM paper
[[@tosem11](#tosem11-choice-calculus)], however, the most comprehensive and
up-to-date introduction is my PhD thesis [[@thesis](#thesis-choice-calculus)].
The tutorial from the 2011 GTTSE Summer School
[[@gttse11](#gttse11-variation-programming)] is also a good (and hopefully fun)
entry point. The tutorial promotes the idea of "variation anywhere" and of
generically lifting non-variational algorithms to variational ones.

Using the choice calculus we have developed safe transformations, quality
criteria, and new abstraction techniques for variational programs. We have also
developed a notion of variational types and extended the Damas-Milner algorithm
to perform efficient type inference on a variational lambda calculus
[[@toplas13](#toplas13-variational-typing),[@icfp12](#icfp12-variational-type-errors)].

The choice calculus also has its own web page:
[choicecalculus.org](http://choicecalculus.org).


## Introductions to the Choice Calculus

<div class="ref-list">
(@thesis) \$thesis-choice-calculus\$
(@gttse11) \$gttse11-variation-programming\$
(@tosem11) \$tosem11-choice-calculus\$

</div>


## Applications of the Choice Calculus

<div class="ref-list resume">
(@toplas13) \$toplas13-variational-typing\$
(@icfp12) \$icfp12-variational-type-errors\$
(@vlhcc11) \$vlhcc11-ifdef-confirmed-harmful\$
(@foser10) \$foser10-program-fields\$

</div>


## Extensions to the Choice Calculus

<div class="ref-list resume">
(@vamos13) \$vamos13-cc-select\$
(@gpce12) \$gpce12-compositional-cc\$

</div>


## Variational Data Structures

<div class="ref-list resume">
(@fosd13) \$fosd13-variational-graphs\$

</div>
