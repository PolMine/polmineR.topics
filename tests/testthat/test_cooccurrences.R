library(testthat)
library(data.table)
library(topicanalysis)

testthat::context("zoo")

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
      