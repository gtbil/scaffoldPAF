test_that("trivial example work", {
  data1 <- data.frame("qname" = c("contig1", "contig2", "contig3"),
                      "start" = c(0L, 1000000L, 2000000L),
                      "end"   = c(1000000L, 2000000L, 3000000L),
                      "str"   = FALSE,
                      "contiglen" = c(1000000L, 1000000L, 1000000L))
  expect_equal(data1, minimize_gaps(data1))
})
