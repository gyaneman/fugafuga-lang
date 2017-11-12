# fugafuga-lang

[![Build Status](https://travis-ci.org/gyaneman/fugafuga-lang.svg?branch=master)](https://travis-ci.org/gyaneman/fugafuga-lang)

fugafuga-lang is a programming language for my studying language implementation.
The goal is ... no ... I cannot see it...

## Requirements

- OCaml 4.04
- oasis 0.4.8

## Build

```
$ make
```

## Example

### Function declaration and call function

```
func add (x int, y int) int {
  ret x + y;
}
var a = 1;
add (a, 2);
```
Result
```
Int(3)
```

### For statement

```
var sum = 0;
var i = 0;
for i = 1; i < 5; i = i + 1 {
  sum = sum + i;
}
sum;
```
Result
```
Int(10)
```

### If statement

```
func is_zero(x) {
  if x == 0 {
    true;
  } else {
    false;
  }
}
is_zero(0) // true
is_zero(1) // false
```
