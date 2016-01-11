context("fizzbuzz test")

test_that("fizzbuzz test", {
  fizzbuzz1 <- function(z) {
    match_with(list(z %% 3, z %% 5)
    , list(0, 0) -> "FizzBuzz"
    , list(0, .) -> "Fizz"
    , list(., 0) -> "Buzz"
    , otherwise  -> as.character(z)
    )
  }

  fizzbuzz2 <- function(z) {
     match_with(z
     , z %% 15== 0 -> "FizzBuzz"
     , z %% 3 == 0 -> "Fizz"
     , z %% 5 == 0 -> "Buzz"
     , otherwise  -> as.character(z)
     )
  }

  ans20 <- c("1", "2", "Fizz", "4", "Buzz", "Fizz", "7", "8", "Fizz", "Buzz",
          "11", "Fizz", "13", "14", "FizzBuzz", "16", "17", "Fizz", "19", "Buzz")

  expect_identical(sapply(1:20, fizzbuzz1), ans20)
  expect_identical(sapply(1:20, fizzbuzz2), ans20)
})
