Software engineers primarily use two orthogonal means to reduce susceptibility
to faults: software testing and static type checking. While many strategies
exist to evaluate the effectiveness of a test suite in catching bugs, there are
few that evaluate the effectiveness of type annotations in a program. This
problem is most relevant in the context of gradual or optional typing, where
programmers are free to choose which parts of a program to annotate and in what
detail. Mutation analysis is one strategy that has proven useful for measuring
test suite effectiveness by emulating potential software faults. We propose
that mutation analysis can be used to evaluate the effectiveness of type
annotations too. We analyze mutants produced by the MutPy mutation framework
against both a test suite and against type-annotated programs. We show that,
while mutation analysis can be useful for evaluating the effectiveness of type
annotations, we require stronger mutation operators that target type
information in programs to be an effective mutation analysis tool.
