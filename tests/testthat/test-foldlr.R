context("foldlr")

test_that("foldlr", {
  foldr <- function(f, init, lst) {
    match_with(lst
               , length(lst) == 0 -> init
               , x::xs            -> f(x, foldr(f, init, xs))
    )
  }

  foldl <- function(f, init, lst) {
    match_with(lst
               , length(lst) == 0 -> init
               , x::xs            -> foldl(f, f(init, x), xs)
    )
  }

  expect_identical(foldl(`+`, 0, 1:10), 55)
  expect_identical(foldr(`+`, 0, 1:10), 55)
  expect_identical(foldl(`-`, 0, 1:10), -55)
  expect_identical(foldr(`-`, 0, 1:10), -5)

})