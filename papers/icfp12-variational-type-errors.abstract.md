Conditional compilation and software product line technologies make it possible
to generate a huge number of different programs from a single software project.
Typing each of these programs individually is usually impossible due to the
sheer number of possible variants. Our previous work has addressed this problem
with a type system for variational lambda calculus (VLC), an extension of
lambda calculus with basic constructs for introducing and organizing variation.
Although our type inference algorithm is more efficient than the brute-force
strategy of inferring the types of each variant individually, it is less robust
since type inference will fail for the entire variational expression if any one
variant contains a type error. In this work, we extend our type system to
operate on VLC expressions containing type errors. This extension directly
supports locating ill-typed variants and the incremental development of
variational programs. It also has many subtle implications for the unification
of variational types. We show that our extended type system possesses a
principal typing property and that the underlying unification problem is
unitary. Our unification algorithm computes partial unifiers that lead to
result types that (1) contain errors in as few variants as possible and (2) are
most general. Finally, we perform an empirical evaluation to determine the
overhead of this extension compared to our previous work, to demonstrate the
improvements over the brute-force approach, and to explore the effects of
various error distributions on the inference process.
