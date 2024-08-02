test_that("loading works", {
  expect_no_error(
    read_and_flip(test_path("testdata", "easy.hap1.paf"))
  )
})
