test_that("format_number_h formats numbers with separators", {
  expect_equal(format_number_h(1234), "1 234")
  expect_equal(format_number_h(1234.56, digits = 6, decimal.mark = ","), "1 234,56")
})

test_that("format_number_h errors on non-numeric input", {
  expect_error(format_number_h("a"))
})
