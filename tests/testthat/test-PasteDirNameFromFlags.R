test_that("PasteDirNameFromFlags handles empty flags", {
  expect_identical(PasteDirNameFromFlags(), "")
  expect_identical(PasteDirNameFromFlags(NULL, NULL), "")
})

test_that("PasteOutdirFromFlags handles empty flags", {
  expect_identical(PasteOutdirFromFlags("results"), "results/")
  expect_identical(PasteOutdirFromFlags("results", NULL), "results/")
})
