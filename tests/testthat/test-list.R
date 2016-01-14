context("List(Tuple) Pattern")

test_that("List(Tuple) Pattern", {
  list_pattern <- function(x) {
    match_with(x
    , list(0, 0) -> "(0, 0) pair"
    , list(1, 0) -> "(1, 0) pair"
    , list(0, .) -> "(0, ?) pair"
    , list(., 3) -> "(?, 3) pair"
    , list(., .) -> "(?, ?) pair"
    , otherwise  -> "another pair")
  }

  expect_identical(list_pattern(list(0, 0)), "(0, 0) pair")
  expect_identical(list_pattern(list(1, 0)), "(1, 0) pair")
  expect_identical(list_pattern(list(0, 1)), "(0, ?) pair")
  expect_identical(list_pattern(list(0, 2)), "(0, ?) pair")
  expect_identical(list_pattern(list(0, 3)), "(0, ?) pair")
  expect_identical(list_pattern(list(1, 3)), "(?, 3) pair")
  expect_identical(list_pattern(list(1, 4)), "(?, ?) pair")
  expect_identical(list_pattern(list(0, 4, 1)), "another pair")

  l2 <- function(lst) {
    match_with(lst
    , list(x, 1) -> 2 * x
    , list(x, y) -> x + y
    , . -> -1
    )
  }

  expect_identical(l2(list(1, 1)), 2)
  expect_identical(l2(list(10, 1)), 20)
  expect_identical(l2(list(1, 2)), 3)
  expect_identical(l2(list(1)), -1)
  expect_identical(l2(list(1,2,3)), -1)
  expect_identical(l2(c(1)), -1)
})
