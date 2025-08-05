test_that("standard numeric input yields a formatted percentage", {
  expect_equal(percentage_formatter(0.1234), "12.3%")
})

test_that("NA and NaN inputs return NA and NaN strings respectively", {
  expect_identical(percentage_formatter(NA), "NA")
  expect_identical(percentage_formatter(NaN), "NaN")
})

test_that("named vectors retain names when keep.names = TRUE", {
  x <- c(foo = 0.1, bar = 0.2)
  res <- percentage_formatter(x, keep.names = TRUE)
  expect_equal(names(res), names(x))
})
