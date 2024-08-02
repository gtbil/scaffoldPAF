test_that("interface works", {
  expect_no_error(
    do_both_haps(test_path("testdata", "easy"))
  )
  # expect_no_error(
  #   do_both_haps(test_path("testdata", "hard"))
  # )
})
