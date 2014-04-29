---
title: Expression Problem
---

# Introduction to the Expression Problem

## Base Programs

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

In Java:

```java
public abstract class Shape {
    public abstract float area();
    public abstract float perimeter();
}

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


## Extensions

In Haskell, adding a new operation is easy:

```haskell
module NewOps where

diameter :: Shape -> Float
diameter (Circle r) = 2 * r
diameter (Square s) = sqrt (2 * s * s)
```

But adding a new shape is hard:

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

In Java, adding a new shape is easy:

```java
public class Triangle extends Shape {
  private float s;
  public Triangle(float side) { s = side; }
  public float area()         { return s * s * Math.sqrt(3) / 4; }
  public float perimeter()    { return 3 * s; }
}
```

But adding a new operation is hard:

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
