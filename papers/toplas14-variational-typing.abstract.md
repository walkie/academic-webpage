Through the use of conditional compilation and related tools, many software
projects can be used to generate a huge number of related programs. The problem
of typing such variational software is difficult. The brute-force strategy of
generating all variants and typing each one individually is (1) usually
infeasible for efficiency reasons and (2) produces results that do not map well
to the underlying variational program. Recent research has focused mainly on
efficiency and addressed only the problem of type checking. In this work we
tackle the more general problem of variational type inference and introduce
variational types to represent the result of typing a variational program. We
introduce the variational lambda calculus (VLC) as a formal foundation for
research on typing variational programs. We define a type system for VLC in
which VLC expressions are mapped to correspondingly variational types. We show
that the type system is correct by proving that the typing of expressions is
preserved over the process of variation elimination, which eventually results
in a plain lambda calculus expression and its corresponding type. We identify a
set of equivalence rules for variational types and prove that the type
unification problem modulo these equivalence rules is unitary and decidable; we
also present a sound and complete unification algorithm. Based on the
unification algorithm, the variational type inference algorithm is an extension
of algorithm W . We show that it is sound and complete and computes principal
types. We also consider the extension of VLC with sum types, a necessary
feature for supporting variational data types, and demonstrate that the
previous theoretical results also hold under this extension. Finally, we
characterize the complexity of variational type inference and demonstrate the
efficiency gains over the brute-force strategy.
