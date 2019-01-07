When type inference fails, it is often difficult to pinpoint the cause of the
type error among many potential candidates. Generating informative messages to
remove the type error is another difficult task due to the limited availability
of type information. Over the last three decades many approaches have
been developed to help debug type errors. However, most of these methods suffer from one or more of the following problems:
(1) Being incomplete, they miss the real cause.
(2) They cover many potential causes without distinguishing them.
(3) They provide little or no information for how to remove the type error.
Any one of this problems can turn the type-error debugging process into a
tedious and ineffective endeavor.

To address this issue, we have developed a method named <i>counter-factual
typing</i>, which
(1) finds a comprehensive set of error causes in AST leaves,
(2)) computes an informative message on how to get rid of the type error for
each error cause, and
(3) ranks all messages and iteratively presents the message for
the most likely error cause.

The biggest technical challenge is the efficient generation of all error
messages, which seems to be exponential in the size of the expression. We
address this challenge by employing the idea of variational typing that
systematically reuses computations for shared parts and generates all messages
by typing the whole ill-typed expression only once.

We have evaluated our approach over a large set of examples collected from
previous publications in the literature. The evaluation result shows that our
approach outperforms previous approaches and is computationally feasible.
