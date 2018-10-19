The Rust programming language is a systems programming language with a strong
static type system. A central feature of Rust’s type system is its unique
concept of "ownership", which enables Rust to give a user safe, low-level
control over resources without the overhead of garbage collection. In Haskell,
most data is immutable and many of the problems addressed by ownership in Rust
simply don’t arise. However, ownership is still useful in Haskell in the
context of mutable references and concurrency. This project report introduces
the Ownership Monad, a monad that implements aspects of Rust’s ownership system
as a library in Haskell. This will demonstrate many of the rules enforced by
the ownership system in Rust in order to better understand the ownership-based
method of tracking resources. This report will further explore the benefits
such a system can provide for tracking resource use between concurrent threads.
