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
think of. No problem. You just add a new subclass of `Shape` and implement all
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

This is one half of the expression problem: Unfortunately, there is no good way
for you to modularly extend by Java shape library with a new operation.



## A shape library in Haskell

In Haskell:

```haskell
module Shapes where

type Radius = Float
type Side   = Float

data Shape = Circle Radius
           | Square Side

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Square s) = s * s

perimeter :: Shape -> Float
perimeter (Circle r) = 2 * pi * r
perimeter (Square s) = 4 * s
```


### Modular extension: adding a new operation

In Haskell, adding a new operation is easy:

```haskell
module NewOps where

diameter :: Shape -> Float
diameter (Circle r) = 2 * r
diameter (Square s) = sqrt (2 * s * s)
```


### Non-modular extension: adding a new shape

But adding a new shape is hard.


## Expression problem


## Workarounds and non-solutions

Sometimes there are workarounds. For example, you might create an interface
with the new operations you want, then extend each existing shape class,
implementing the new operations.

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

Now whenever you want a circle, you can just instantiate `NewCircle`. This will
work as long as my library never creates circles on its own. If it does, it
will create old circles that don't 


```haskell
module NewShapes where

data NewShape = OldShape Shape
              | Triangle Side

newArea :: Shape -> Float
newArea (OldShape o) = area o
newArea (Triangle s) = s * s * sqrt 3 / 4

newPerimeter :: Shape -> Float
newPerimeter (OldShape o) = perimeter o
newPerimeter (Triangle s) = 3 * s
```
