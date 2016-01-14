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



  sum_int <- function(x) {
    match_with(x
    , integer(0) -> 0L
    , y::ys -> y + sum_int(ys)
    )
  }

  sum_num <- function(x) {
    match_with(x
    , numeric(0) -> 0
    , y::ys -> y + sum_num(ys)
    )
  }

  sum_lst <- function(x) {
    match_with(x
    , list() -> 0L
    , y::ys -> y + sum_lst(ys)
    )
  }

  expect_identical(sum_int(1:3), 6L)
  expect_identical(sum_int(c(1L,2L,3L)), 6L)
  expect_error(sum_int(c(1,2,3)))
  expect_error(sum_int(list(1L,2L,3L)))
  expect_error(sum_int(list(1,2,3)))

  expect_error(sum_num(1:3))
  expect_error(sum_num(c(1L,2L,3L)))
  expect_identical(sum_num(c(1,2,3)), 6)
  expect_error(sum_num(list(1L,2L,3L)))
  expect_error(sum_num(list(1,2,3)))

  expect_error(sum_lst(1:3))
  expect_error(sum_lst(c(1L,2L,3L)))
  expect_error(sum_lst(c(1,2,3)))
  expect_identical(sum_lst(list(1L,2L,3L)), 6L)
  expect_identical(sum_lst(list(1,2,3)), 6)

})
