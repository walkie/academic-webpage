The choice calculus is a formal language for representing variation in software
artifacts. Variability is organized in the choice calculus through the use of
dimensions, where each dimension represents a decision that must be made in
order to obtain a particular variant. However, the process of selecting
alternatives from dimensions was relegated to an external operation. This
precludes many interesting variation and reuse patterns, such as nested product
lines, and theoretical results, such as a syntactic description of
configuration, that would be possible if selection were a part of the language
itself.

In this paper we add a selection operation to the choice calculus and
illustrate how that increases the expressiveness of the calculus. We
investigate some alternative semantics of this operation and study their impact
and utility. Specifically, we will examine selection in the context of static
and dynamically scoped dimension declarations, as a well as a modest and greedy
form of dimension elimination. We also present a design for a type system to
ensure configuration safety and modularity of nested product lines.
