---
title: Expression Problem
---

# Introduction to the Expression Problem

## Goals

 * Introduce the *expression problem* in simple terms.
 * Motivate *modular extensibility* from a practical perspective.

<div class="no-print">

## Outline

 * [A shape library in Java]
     * [Modular extension: adding a new shape]
     * [Non-modular extension: adding a new operation]
 * [A shape library in Haskell]
     * [Modular extension: adding a new operation]
     * [Non-modular extension: adding a new shape]
 * [Expression problem]
 * [Workarounds and non-solutions]

</div>

## A shape library in Java

Suppose I want to provide a library for working with geometric shapes. In
Java, I might start by creating an abstract class or interface defining all of
the operations I want shapes to support.

```java
public abstract class Shape {
  public abstract float area();
  public abstract float perimeter();
}
```

Then I add several concrete classes that implement this interface, representing
all of the different shapes that my library provides.

```java
public class Circle extends Shape {
  private float r;
  public Circle(float radius) { r = radius; }
  public float area()         { return Math.PI * r * r; }
  public float perimeter()    { return 2 * MATH.PI * r; }
}

public class Square extends Shape {
  private float s;
  public Square(float side) { s = side; }
  public float area()       { return s * s; }
  public float perimeter()  { return 4 * s; }
}
```

Finally, I package my shape library up in `shapes.jar` and put it somewhere on
the internet for everyone to use.

Obviously, this example is super tiny: my shape library provides just two
operations (area and perimeter) and two shapes (circle and square). But pretend
its bigger. Pretend it provides lots of additional operations like scaling the
size of shapes and drawing shapes on a canvas; pretend it supports combining
basic shapes into composite shapes; and, of course, pretend it provides a lot
more shapes!


### Modular extension: adding a new shape

Now you come along. You're starting a project that will do a lot of cool things
with shapes. You don't know me and don't have access to my source code, but you
downloaded `shapes.jar` and it looks pretty useful. You want to use it in your
program. Unfortunately, it's missing a few features that you really need.

The first thing you need is support for an exotic basic shape that I didn't
think of. No problem, you just add a new subclass of `Shape` and implement all
of its operations.

```java
public class Triangle extends Shape {
  private float s;
  public Triangle(float side) { s = side; }
  public float area()         { return s * s * Math.sqrt(3) / 4; }
  public float perimeter()    { return 3 * s; }
}
```

Congratulations, you have just extended my shape library in a modular way! You
*extended* my shape library by adding a new feature that it didn't have before.
And your extension is *modular* since (1) it didn't require changing the
existing source code of my library, and (2) all of the new code related to the
feature is grouped together.

Grouping related code together is nice for readability and maintenance, but the
restriction that you not change existing source code is a hard requirement.
Since you're the client of my library and not the developer of it, you cannot
add new features by just editing the source code. Therefore, in order to reuse
my library and add new features, you *must have modular extensibility*.


### Non-modular extension: adding a new operation

The next feature you need is support for a new operation on shapes: a method
`diameter()` that returns the length of the longest line segment contained by
the shape. You would like to add the new method to the abstract class `Shape` so
that all shapes will support this new operation.

If you had access to the source code, you could add the new method by modifying
the existing class definitions.

```java
public abstract class Shape {
  // ... original class body ...
  public abstract float diameter();
}

public class Circle extends Shape {
  // ... original class body ...
  public float diameter() { return 2 * r; }
}

public class Square extends Shape {
  // ... original class body ...
  public float diameter() { return Math.sqrt(2 * s * s); }
}
```

But you can't edit the source code since you don't have it!

Unfortunately, there is no good way for you to modularly extend my Java shape
library with new operations... But Java is stodgy language, maybe the situation
looks better in Haskell?



## A shape library in Haskell

Let's play the same game but faster this time.

I want to provide a library for working with shapes. In Haskell, I might start
by defining a data type that enumerates the different kinds of shapes that my
library supports.

```haskell
type Radius = Float
type Side   = Float

data Shape = Circle Radius
           | Square Side
```

Then I add several functions that implement all of the operations on shapes
that my library provides.

```haskell
area :: Shape -> Float
area (Circle r) = pi * r * r
area (Square s) = s * s

perimeter :: Shape -> Float
perimeter (Circle r) = 2 * pi * r
perimeter (Square s) = 4 * s
```

Once again, pretend my library is much bigger. When it's done, I package it up
and put it somewhere on the internet for everyone to use.


### Modular extension: adding a new operation

Now you come along. You found my library, but you don't know me and don't have
access to my source code. You want to use my library, but it's missing a few
features that you really need.

The first thing you need is support for a new operation on shapes: a `diameter`
function that returns the length of the longest line segment contained by the
shape. No problem, you just add a new function.

```haskell
diameter :: Shape -> Float
diameter (Circle r) = 2 * r
diameter (Square s) = sqrt (2 * s * s)
```

Nice! This extension is modular since it didn't require changing the existing
source code of my library and all of the new code is grouped together.

Remember how you couldn't modularly add new operations to my shape library in
Java? Haskell for the win?


### Non-modular extension: adding a new shape

The next thing you need is support for an exotic basic shape that I didn't
think of. If you had access to the source code, you could add the new shape by
extending the data type definition with a new constructor, then extending each
function with a new case for the new shape.

```haskell
data Shape = -- ... original constructors ...
           | Triangle Side

area :: Shape -> Float
-- ... original cases ...
area (Triangle s) = s * s * sqrt 3 / 4

perimeter :: Shape -> Float
-- ... original cases ...
perimeter (Triangle s) = 3 * s
```

But you can't edit the source code since you don't have it!

Unfortunately, there is no good way for you to modularly extend my Haskell shape
library with new shapes... Remember how easy that was in Java?


## Expression problem

The expression problem is about the ability to modularly extend a program with
*new things* and with *new operations* on both old and new things. In the case of
a shape library, we want to be able to modularly add new kinds of shapes and
new operations on all kinds of shapes.

In object-oriented languages like Java, it tends to be easy to add new kinds of
things and hard to add new operations. We add new things by adding new
subclasses. But adding new operations requires adding new methods to all of our
existing class definitions.

In functional languages like Haskell, it tends to be easy to add new operations
and hard to add new things. We add new operations by adding new functions. But
adding new things requires extending an existing data type definition and
extending all functions that pattern match on that data type.

During this seminar, we'll look at many different kinds of solutions to the
expression problem. Some are [design patterns][DP] that can be applied in
languages like Java and Haskell, in order to make your library extensible with
both new things and new operations. Some are new languages or language features
that make it easier to do both kinds of extensions without the foresight that
the design pattern approaches require.

I also think the expression problem is just one view of something more
fundamental than extensibility in object-oriented vs. functional programming.
The [aspect-oriented programming][AOP] folks talk more generally about the
"tyranny of [dominant decomposition][DD]", and many [feature-oriented
programming][FOP] approaches are also focused on different kinds of modular
extensibility.


## Workarounds and non-solutions

You may be able to come up with some workarounds for the problem cases we
identified above. For example, you might modularly extend the Java library with
a new operation by first creating an interface with the new operations you
want, then extending each existing shape class.

```java
public interface HasDiameter {
  public float diameter();
}

public class NewCircle extends Circle implements HasDiameter {
  public float diameter() { return 2 * radius(); }
}

public class NewSquare extends Square implements HasDiameter {
  public float diameter() { return Math.sqrt(2 * side() * side(); }
}
```

Now whenever you want a circle, you can just instantiate `NewCircle`.

For the Haskell library, you can modularly extend the library with a new shape
by defining a new data type that is either an old shape from the library, or
one of the new shapes that you want to add. Then you wrap each operation in a
new function, calling the original function for an old shape and adding cases
for the new shapes. This is essentially the [adapter pattern][Adapter].

```haskell
data NewShape = OldShape Shape
              | Triangle Side

newArea :: Shape -> Float
newArea (OldShape o) = area o
newArea (Triangle s) = s * s * sqrt 3 / 4

newPerimeter :: Shape -> Float
newPerimeter (OldShape o) = perimeter o
newPerimeter (Triangle s) = 3 * s
```

Aside from requiring a lot of boilerplate, these workarounds will not work in
all cases. Can you think of when they will fail?

<br>

**[Back to course web page](index.html)**

[Adapter]: https://en.wikipedia.org/wiki/Adapter_pattern
[AOP]: https://en.wikipedia.org/wiki/Aspect-oriented
[DD]: http://www.aosd.net/wiki/index.php?title=Glossary#Dominant_Decomposition
[DP]: https://en.wikipedia.org/wiki/Software_design_pattern
[FOP]: https://en.wikipedia.org/wiki/Feature-oriented_programming
