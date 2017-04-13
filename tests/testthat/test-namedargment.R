context("Named Argument")

test_that("NamedArgument", {


  expect_identical({
    match_with(x = 1:10
    , length(x) == 1 -> "1"
    , length(x) == 2 -> "2"
    , . -> "so long")
  }, "so long")

  fizzbuzz <- function(z) {
    match_with(x = list(z %% 3, z %% 5)
    , list(0, 0) -> "FizzBuzz"
    , list(0, .) -> "Fizz"
    , list(., 0) -> "Buzz"
    , .  -> as.character(z)
    )
  }

  expect_identical(fizzbuzz(1), "1")
  expect_identical(fizzbuzz(3), "Fizz")
  expect_identical(fizzbuzz(5), "Buzz")
  expect_identical(fizzbuzz(15), "FizzBuzz")

  expect_identical({
    match_with(x = list(1,2,3)
    , x[2] == 1 -> 10
    , x[2] == 2 -> 20
    , x[2] == 3 -> 30
    , . -> -1
  )
  }, 20)

})
