The design of languages is still more of an art than an engineering discipline.
Although recently tools have been put forward to support the language design
process, such as language workbenches, these have mostly focused on a syntactic
view of languages. While these tools are quite helpful for the development of
parsers and editors, they provide little support for the underlying design of
the languages. In this paper we illustrate how to support the design of
languages by focusing on their semantics first. Specifically, we will show that
powerful and general language operators can be employed to adapt and grow
sophisticated languages out of simple semantics concepts. We use Haskell as a
metalanguage and will associate generic language concepts, such as semantics
domains, with Haskell-specific ones, such as data types. We do this in a way
that clearly distinguishes our approach to language design from the traditional
syntax-oriented one. This will reveal some unexpected correlations, such as
viewing type classes as language multipliers. We illustrate the viability of
our approach with several real-world examples.
