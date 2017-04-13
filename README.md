<!-- README.md is generated from README.Rmd. Please edit that file -->
{matchwith}
===========

What is this package?
---------------------

match\_with() simulates pattern matching that is used in many functional programming language such as 'case of' in Haskell and 'match with' in OCaml.

Installation
------------

``` r
# install.packages("devtools")
devtools::install_github("tobcap/matchwith")
library("matchwith")
```

Supports
--------

-   Constant pattern
-   Cons pattern
-   Tuple pattern with matching symbols
-   Wildcard pattern
-   Guard clauses (not completed implemented)

Usage
-----

``` r
# Syntax
f <- function(expr) {
  match_with(expr
  , pattern_1 -> res_1
  , pattern_2 -> res_2
             ...
  , pattern_n -> res_n
  )
}
```

Const pattern
=============

`pattern_i` represents numeric literal, charater literal, or NULL. When x is matched with `pattern_i`, `res_i` (the right side of expression) is evalueted in the parent.frame . In the exampe below, when x is 0 or 1, the right side expression is evalated and returned. when x is more than 1, wildcard simbol is matched and `fib(x-1) + fib(x-2)` is evaluated and returned.

``` r
fib <- function(x)
  match_with(x
  , 0 -> 0
  , 1 -> 1
  , . -> fib(x - 1) + fib(x - 2)
  )
fib(10)
#> [1] 55
```

Cons pattern
============

`::` is used for DSL porpose that is representing constructor of head and tail. when pattern is `y::ys`, the first object of `x` is assigned to `y` and rest of that is assigned to `ys`.

``` r
sum1 <- function(x)
  match_with(x
  , integer(0) -> 0L
  , y::ys -> y + sum1(ys)
  )
sum1(1:5)
#> [1] 15
```

``` r
sum2 <- function(x, acc = 0L)
  match_with(x
  , integer(0) -> acc
  , y::ys -> sum2(ys, acc + y)
  )
sum2(1:5)
#> [1] 15
```

Tuple pattern with matching
===========================

When `pattern_i` starts with `list` and includes new symbols as list's arguments, the coresponding values of `x` which must have the same language structure will be assigned to the new symbols, and `res_i` is evaluated with using such symbol-value-associated-list.

``` r
ex1 <- function(x)
  match_with(x
  , list(a, b) -> 10 * a + b
  , list(a, b, c) -> 100 * a + 10 * b + c
  , . -> x
  )
ex1(list(1,2))
#> [1] 12
ex1(list(1,2,3))
#> [1] 123
str(ex1(list(1,2,3,4)))
#> List of 4
#>  $ : num 1
#>  $ : num 2
#>  $ : num 3
#>  $ : num 4
```

Wildcard inside `list` of `pattern_i` can be used.

``` r
fizzbuzz1 <- function(x)
  match_with(list(x %% 3, x %% 5)
  , list(0, 0) -> "fizzbuzz"
  , list(0, .) -> "fizz"
  , list(., 0) -> "buzz"
  , . -> as.character(x)
  )
sapply(1:30, fizzbuzz1)
#>  [1] "1"        "2"        "fizz"     "4"        "buzz"     "fizz"    
#>  [7] "7"        "8"        "fizz"     "buzz"     "11"       "fizz"    
#> [13] "13"       "14"       "fizzbuzz" "16"       "17"       "fizz"    
#> [19] "19"       "buzz"     "fizz"     "22"       "23"       "fizz"    
#> [25] "buzz"     "26"       "fizz"     "28"       "29"       "fizzbuzz"
```

Other than `list`, `x` is compared with `pattern_i` and if it matches, `res_i` is evaluated.

``` r
ex2 <- function(x)
  match_with(x
  , 1:3 -> 2 * x
  , 1:5 -> 3 * x
  , . -> 100 * x
  )
ex2(1:3)
#> [1] 2 4 6
ex2(1:5)
#> [1]  3  6  9 12 15
ex2(1:4)
#> [1] 100 200 300 400
ex2(c(1,2,3)) # 1:3 is integer class, c(1,2,3) is numeric class
#> [1] 100 200 300
ex2(c(1L,2L,3L))
#> [1] 2 4 6
```

Guard clauses
=============

when `pattern_i` uses `Compare` family (==, &gt;, &lt;, !=, &lt;=, &gt;=) (or `!`, `any`, `all`, `identity`, `isTRUE`) or a function whose name starts with `is.` as a top node of language object, it is regarded as guard clauses, and when the result of evaluating `pattern_i` is TRUE, `res_i` will be evaluated and returned.
**Note that &, | possibly return vector of boolean object whose length may have more than 2, so all() or any() is required to use them.**

``` r
ex3 <- function(x)
  match_with(x
  , x > 5 -> x * 100
  , x < 3 -> x * 10
  , . -> -x
  )
sapply(1:10, ex3)
#>  [1]   10   20   -3   -4   -5  600  700  800  900 1000

# thanks to @Keiku issue#1
ex4 <- function(x)
  match_with(x
  , all(1 <= x & x <= 10) -> "1-10"
  , all(11 <= x)          -> "11+"
  , otherwise             -> "other"
  )
ex4(1:10)
#> [1] "1-10"
ex4(8:12)
#> [1] "other"
ex4(11:14)
#> [1] "11+"
ex4(5)
#> [1] "1-10"
ex4(15)
#> [1] "11+"
ex4(-1)
#> [1] "other"
```
