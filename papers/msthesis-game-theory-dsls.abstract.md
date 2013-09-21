Experimental game theory is the use of game theoretic abstractions—games,
players, and strategies—in experiments and simulations. It is often used in
cases where traditional, analytical game theory fails or is difficult to apply.
This thesis collects three previously published papers that provide
domain-specific language (DSL) support for defining and executing these
experiments, and for explaining their results.

Despite the widespread use of software in this field, there is a distinct lack
of tool support for common tasks like modeling games and running simulations.
Instead, most experiments are created from scratch in general-purpose
programming languages. We have addressed this problem with Hagl, a DSL embedded
in Haskell that allows the concise, declarative definition of games,
strategies, and executable experiments. Hagl raises the level of abstraction
for experimental game theory, reducing the effort to conduct experiments and
freeing experimenters to focus on hard problems in their domain instead of
low-level implementation details.

While analytical game theory is most often used as a prescriptive tool, a way
to analyze a situation and determine the best course of action, experimental
game theory is often applied descriptively to explain why agents interact and
behave in a certain way. Often these interactions are complex and surprising.
To support this explanatory role, we have designed visual DSL for explaining
the interaction of strategies for iterated games. This language is used as a
vehicle to introduce the notational quality of traceability and the new
paradigm of explanation-oriented programming.
