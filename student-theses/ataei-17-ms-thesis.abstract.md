Data variations are prevalent in real-world applications. For example, software
vendors have to handle numerous variations in the business requirements,
conventions, and environmental settings of a software product. In
database-backed software, the database of each version may have a different
schema and content. As another example, data scientists often need to use a
subset of the available databases because using non-relevant information may
reduce the effectiveness of the results. Such variations give rise to numerous
data variants in these applications. Users often would like to query and/or
analyze all such variants simultaneously. For example, a software vendor would
like to perform common tests over all versions of its product and a data
scientist would like to find the subset of information over which the analytics
algorithm delivers the most accurate results. Currently, there is not any
systematic and principled approach to managing and querying data variations and
users have to use their intuition to perform such analyses. We propose a novel
abstraction called a *variational database* that provides a compact and
structured representation of general forms of data variations for relational
databases. As opposed to data integration approaches that provide a unified
representation of all data sources, variational databases make variations
explicit in both the schema definition and the query language without
introducing too much complexity.
