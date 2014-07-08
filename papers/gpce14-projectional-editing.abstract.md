Editing the source code of variational software is complicated by the presence
of variation annotations, such as #ifdef statements, and by code that is only
included in some configurations. When we want to edit some configurations and
not others, it would be easier to edit a simplified version of the source code
that contains only the variability we currently care about. In this paper, we
present a projectional editing model for variational software. Using our
approach, a programmer can partially configure a variational program, edit this
simplified view of the code, and then automatically update the original, fully
variational source code. The model is based on an isolation principle where
edits affect only the variants that are visible in the view. We show that this
principle has several nice properties that are suggested by related work on
bidirectional transformations.
