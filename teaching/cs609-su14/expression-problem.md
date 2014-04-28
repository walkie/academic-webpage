---
title: Expression Problem
---

# Introduction to the Expression Problem

## Base Programs

In Haskell:

```haskell
type Radius = Float
type Width  = Float

data Shape = Circle Radius
           | Square Width

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Square w) = w * w

perimeter :: Shape -> Float
perimeter (Circle r) = 2 * pi * r
perimeter (Square w) = 4 * w
```

In Java:

```java
abstract class Shape {
  float area();
  float perimeter();
}

class Circle extends Shape {
  
  private float r;
  
  Circle(float radius) {
    r = radius;
  }
  
  float area() {
    return pi * r * r;
  }

  float perimeter() {
    return 2 * Math.PI * r;
  }

}

class Square extends Shape {
  
  private float w;

  Square(float width) {
    w = width;
  }

  float area() {
    return w * w;
  }

  float perimeter() {
    return 4 * w;
  }

}
```
