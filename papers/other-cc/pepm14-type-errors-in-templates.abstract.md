Current C++ implementations typecheck templates in two phases:
Before instantiation, those parts of the template are checked that do
not depend on template parameters, while the checking of the remaining parts
is delayed until template instantiation time when the template arguments
become available. This approach is problematic because it causes two major
usability problems. First, it prevents library developers to provide
guarantees about the type correctness for modules involving templates. Second,
it can lead, through the incorrect use of template functions, to inscrutable
error messages. Moreover, errors are often reported far away from the source
of the program fault.

To address this problem, we have developed a type system for Garcia's
type-reflective calculus that allows a more precise characterization of types
and thus a better utilization of type information within template definitions.
This type system allows the static detection of many type errors that could
previously only be detected after template instantiation. The additional
precision and earlier detection time is achieved through the use of so-called
"choice types" and corresponding typing rules that support the static reasoning about
underspecified template types. The main contribution of this paper is a
guarantee of the type safety of C++ templates (general definitions with
specializations) since we can show that well-typed templates only generate
well-typed object programs.
