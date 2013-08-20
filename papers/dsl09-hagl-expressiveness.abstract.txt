Experimental game theory is an increasingly important research tool in many
fields, providing insight into strategic behavior through simulation and
experimentation on game theoretic models. Unfortunately, despite relying
heavily on automation, this approach has not been well supported by tools. Here
we present our continuing work on Hagl, a domain-specific language embedded in
Haskell, intended to drastically reduce the development time of such
experiments and support a highly explorative research style.  In this paper we
present a fundamental redesign of the underlying game representation in Hagl.
These changes allow us to better leverage domain knowledge by allowing
different classes of games to be represented differently, exploiting existing
domain representations and algorithms. In particular, we show how this supports
analytical extensions to Hagl, and makes strategies for state-based games
vastly simpler and more efficient.
