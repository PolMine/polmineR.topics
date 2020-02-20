library(testthat)
library(data.table)
library(topicanalysis)

testthat::context("cooccurrences")

test_that(
  "cooccurrences plausibility checks",
  {
    data(BE_lda)
    dt <- cooccurrences(BE_lda, k = 3L)
    top <- topicmodels::topics(BE_lda, k = 3L)
    
    # Ensure that cooccurrence counts are correct
    for (i in 1L:10L){
      a <- dt[i,][["a"]]
      b <- dt[i,][["b"]]
      expect_equal(
        dt[i,][["count_coi"]],
        table(apply(top, 2, function(x) (a %in% x) && (b %in% x)))[["TRUE"]]
      )
    }
    
    # The first test shall ensure that counts for a->b and b->a, i.e. for the
    # reverse order of the topic are identical
    g <- igraph::graph_from_data_frame(
      d = data.frame(
        from = dt[["a"]],
        to = dt[["b"]],
        n = dt[["count_coi"]],
        chisquare = dt[["chisquare"]]
      ),
      directed = TRUE
    )
    m <- igraph::get.adjacency(g, sparse = FALSE, attr = "n")
    expect_true(isSymmetric(m))
    
    # Do the same for the chisquare test, knowing that there is a potential
    # rounding error
    m <- igraph::get.adjacency(g, sparse = FALSE, attr = "chisquare")
    expect_true(isSymmetric(m, tol = 0.05))
    
    # Now, ensure that the count of the number of cooccurrences and the 
    # occurrences external to the context sum up to the total
    dt_n <- data.table(n = as.matrix(table(as.vector(top)))[,1])
    dt_n[, "topic" := 1L:nrow(dt_n)]
    setkeyv(dt_n, cols = "topic")
    setkeyv(dt, cols = "b")
    dt2 <- dt[dt_n]
    expect_true(all(dt[["count_coi"]] + dt[["count_ref"]] == dt2[["n"]]))

  }
)


test_that(
  "check cooccurrences results against docs()-method (without renumbering)",
  {
    data(BE_lda)
    BE <- Topicanalysis$new(BE_lda)
    BE_coocs <- cooccurrences(BE_lda, k = 5L)
    for (i in 1:5){
      expect_equal(
        BE_coocs[i][["count_coi"]],
        length(BE$docs(x = BE_coocs[i][["a"]], y = BE_coocs[i][["b"]], n = 5L))
      )
    }
  }
)


test_that(
  "check cooccurrences results against docs()-method (with renumbering, without subsetting)",
  {
    data(BE_lda)
    top <- topicmodels::topics(BE_lda, k = 5L)
    benchmark <- table(apply(top, 2, function(x) all(any(1:5 %in% x), any(6:10 %in% x))))[["TRUE"]]
    
    BE <- cooccurrences(BE_lda, k = 5L)
    BE <- Topicanalysis$new(BE_lda)
    
    n_docs_method <- length(BE$docs(x = 1:5, y = 6:10, n = 5L))
    
    expect_identical(benchmark, n_docs_method)
    
    renumber_vec <- 1L:BE_lda@k
    renumber_vec[1:5] <- 1L
    renumber_vec[6:10] <- 6L
    coocs <- cooccurrences(BE_lda, k = 5L, renumber = renumber_vec)
    
    expect_equal(coocs[a == 1][b == 6][["count_coi"]], n_docs_method)
  }
)