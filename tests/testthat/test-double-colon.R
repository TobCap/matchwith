context("double-colon")

test_that("double-colon", {
  sum1 <- function(ys)
    match_with(ys
    , integer(0) -> 0
    , x::xs -> x + sum1(xs)
    )

  sum2 <- function(ys)
    match_with(ys
    , length(ys) == 0 -> 0
    , x::xs -> x + sum2(xs)
    )

  add1 <- function(ys)
    match_with(ys
    , integer(0) -> integer(0)
    , x::xs -> c(x + 1, add1(xs))
    )

  expect_equal(sum1(1:10), 55)
  expect_equal(sum2(1:10), 55)
  expect_equal(add1(1:5), 2:6)
})
