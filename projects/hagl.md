---
title: Hagl
---

# Hagl: Haskell Game Language

Hagl is a DSEL for *experimental game theory* (also called evolutionary game
theory or behavioral economics). It supports defining games and strategies,
then executing them repeatedly in order to collect and observe the results.

Hagl provides built-in support for standard game representations, such as
normal and extensive form, and constructs for defining games in terms of the
manipulation of a shared state (as in tic-tac-toe). New game representations
can be easily added by instantiating a type class.

A monadic strategy DSL supports concise and vaguely English-like definitions of
strategies for playing these games, usually iteratively.

While Hagl provides some basic game analyses, its primary focus is simulation
and experimentation. Games can be executed and strategies can be pitted against
each other for repeated play and in various kinds of tournaments.

The most recent version of
**[Hagl is available on GitHub](https://github.com/walkie/Hagl)**. Older
versions corresponding to specific papers are linked to below.


## Publications

<div class="ref-list">
(@msthesis) \$msthesis-game-theory-dsls\$
(@jfp09) \$jfp09-hagl\$
(@dsl11) \$dsl09-hagl-expressiveness\$

</div>

## Related

<div class="ref-list resume">
(@vlhcc08) \$vlhcc08-explaining-strategies\$

</div>
