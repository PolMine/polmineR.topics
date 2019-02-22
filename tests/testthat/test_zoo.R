library(testthat)
library(data.table)

testthat::context("zoo")

test_that(
  "zoo and cooccurrences",
  {
    data(BE_lda)
    coocs <- cooccurrences(BE_lda, k = 3L)
    for (i in 1L:5L){
      z <- as.zoo(BE_lda, k = 3L, select = c(coocs[["a"]][i], coocs[["b"]][i]))
      expect_equal(sum(z), coocs[["count_coi"]][i])
    }
  }
)
      