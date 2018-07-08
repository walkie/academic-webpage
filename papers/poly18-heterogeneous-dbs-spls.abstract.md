Data variations are prevalent while developing software product lines (SPLs). A
SPL enables a software vendor to quickly produce different variants of their
software tailored to variations in their clients' business requirements,
conventions, desired feature sets, and deployment environments. In
database-backed software, the database of each variant may have a different
schema and content, giving rise to numerous data variants. Users often need to
query and/or analyze all variants in a SPL simultaneously. For example, a
software vendor wants to perform common tests or inquiries over all variants.
Unfortunately, there is no systematic approach to managing and querying data
variations and users have to use their intuition to perform such tasks, often
resorting to repeating a task for each variant. We introduce *VDBMS
(Variational Database Management System)*, a system that provides a compact,
expressive, and structured representation of variation in relational databases.
In contrast to data integration systems that provide a unified representation
for all data sources, VDBMS makes variations explicit in both the schema and
query. Although variations can make VDBMS queries more complex than plain
queries, a strong static type system ensures that all variants of the query are
consistent with the corresponding variants of the database. Additionally,
*variational queries* make it possible to compactly represent and efficiently
run queries over a huge range of data variations in a single query. This
directly supports many tasks that would otherwise be intractable in highly
variational database-backed SPLs.
