Many applications require not only representing variability in software and
data, but also computing with it. To do so efficiently requires variational
data structures that make the variability explicit in the underlying data and
the operations used to manipulate it. Variational data structures have been
developed ad hoc for many applications, but there is little general
understanding of how to design them or what tradeoffs exist among them.
In this paper, we strive for a more systematic exploration and
analysis of a variational data structure. We want to know how different design
decisions affect the performance and scalability of a variational data
structure, and what properties of the underlying data and operation sequences
need to be considered.
Specifically, we study several alternative designs of a variational stack, a
data structure that supports efficiently representing and computing with
multiple variants of a plain stack, and that is a common building block in many
algorithms. The different variational stacks are presented as a small product
line organized by three design decisions. We analyze how these design decisions
affect the performance of a variational stack with different usage profiles.
Finally, we evaluate how these design decisions affect the performance of the
variational stack in a real-world scenario: in the interpreter \tool{VarexJ} when
executing real software containing variability.