Many applications require not only representing variability in software and
data, but also computing with it. To do so efficiently requires variational
data structures that make variability explicit in the underlying data and the
operations used to manipulate it. Variational data structures have been
developed ad hoc for many applications, but there is little general
understanding of how to design them or what tradeoffs exist among them.

In this thesis, we introduce the concept of holes to represent variational data
structures of different sizes and shapes. Moreover, we strive for a more
systematic exploration and analysis of a variational data structure. We want to
know how different design decisions affect the performance and scalability of a
variational data structure, and what properties of the underlying data and
operation sequences need to be considered.

Specifically, we study several alternative designs of a variational stack and
analyze how these design decisions affect the performance of a variational
stack with different usage profiles. We evaluate variational stacks in a
real-world scenario: in the interpreter VarexJ when executing real software
containing variability. Finally, we discuss different ways of representing
variational priority queues and show how this affects the performance of the
variational Dijkstra's algorithm.
