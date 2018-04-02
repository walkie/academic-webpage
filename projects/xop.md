---
title: Explanation-Oriented Programming
---

# Explanation-Oriented Programming

*Explanation-oriented programming* (XOP) is motivated by two observations:

 1. Programs often produce unexpected results.
 2. Programs have value not only for instructing computers, but as a medium of
    communication between people.

When a program produces an unexpected result, a user is presented with several
questions. Is the result correct? If so, what is the user's misunderstanding?
If not, what is wrong and how can it be fixed? In these situations an
*explanation* of how the result was generated or why it is correct would be
very helpful. Although some tools exist for addressing these questions, such as
debuggers, their explanations (e.g. stepping through the program and observing
its state) are expensive to produce and have low explanatory value, especially
to non-programmers.

One goal of XOP is to shift the focus on explaining programs into the language
design phase, promoting *explainability* as an explicit design goal. In
particular, when defining a language, designers should consider not only how
the syntax relates to the production of results (execution semantics), but also
how it relates to explanations of how those results are produced and why they
are correct (an explanation semantics).

Besides applications to debugging, XOP suggests a new class of domain-specific
languages where the explanation itself, rather than the final value, is the
primary output of a program. This emphasizes the second observation above, that
programs are useful for communication between people. Using such a DSL, an
*explanation designer*, who is an expert in the application domain, can create
and distribute explanation artifacts (programs) to explain problems to
non-expert *explanation consumers*. 


## Publications
 
<div class="ref-list">
(@sigcse18) \$sigcse18-algorithm-explanations\$
(@jvlc13) \$jvlc13-probula\$
(@dsl11) \$dsl11-causation-dsl\$
(@vlhcc10) \$vlhcc10-neuron-diagrams\$
(@vlhcc09) \$vlhcc09-visual-explanations-probability\$
(@dsl09) \$dsl09-explaining-probability\$
(@vlhcc08) \$vlhcc08-explaining-strategies\$

</div>
