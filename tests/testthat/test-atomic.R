context("test atomic")

test_that("test atomic", {
  expect_true(
    match_with(1
    , 1 -> TRUE
    , . -> FALSE
    )
  )

  expect_true(
    match_with(1L
      , 1 -> TRUE
      , . -> FALSE
    )
  )

  expect_true(
    match_with(1
      , 1L -> TRUE
      , . -> FALSE
    )
  )

  expect_true(
    match_with("1"
    , "1" -> TRUE
    , . -> FALSE
    )
  )

  expect_true(
    match_with(NULL
      , NULL -> TRUE
      , . -> FALSE
    )
  )

})
