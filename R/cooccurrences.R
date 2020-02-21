#' Get topic cooccurrences.
#' 
#' @param .Object Either an object inheriting from the \code{TopicModel} class
#'   (such as \code{LDA_Gibbs}), or a \code{matrix} with the topics present in
#'   documents. If \code{.Object} is a \code{matrix}, each column is expected to
#'   represent the top k topics present in a document. The \code{matrix}
#'   returned by the \code{topics}-method from the \code{topicmodels} package
#'   was used to develop the method, but document-term-matrices generated other
#'   tools may work as well.
#' @param k An \code{integer} value, the \code{k} first topics to consider when
#'   deriving the document-topic-matrix from a trained topicmodel.
#' @param regex If not \code{NULL} (default), the procedure will be limited to
#'   document names matched by the regular expression stated by \code{regex}.
#' @param docs If not \code{NULL}, the procedure will be limited to documents
#'   matching the character string.
#' @param renumber If not \code{NULL} (the default), an \code{integer} vector
#'   (length = number of topics) supplying new topic numbers.
#' @param verbose A \code{logical} value.
#' @param progress A \code{logical} value.
#' @importMethodsFrom polmineR cooccurrences
#' @exportMethod cooccurrences
#' @import data.table
#' @importFrom polmineR chisquare
#' @examples
#' data(BE_lda, BE_labels)
#' dt <- cooccurrences(BE_lda, k = 3L)
#' topics_to_drop <- grep("^\\(.*?\\)$", BE_labels)
#' dt_min <- dt[chisquare >= 10.83][!a %in% topics_to_drop][!b %in% topics_to_drop]
#' dt_min[, "a_label" := BE_labels[ dt_min[["a"]] ] ]
#' dt_min[, "b_label" := BE_labels[ dt_min[["b"]] ] ]
#' 
#' if (requireNamespace("igraph")){
#' g <- igraph::graph_from_data_frame(
#'   d = data.frame(
#'     from = dt_min[["a_label"]],
#'     to = dt_min[["b_label"]],
#'     n = dt_min[["count_coi"]],
#'     stringsAsFactors = FALSE
#'   ),
#'   directed = TRUE
#' )
#' g <- igraph::as.undirected(g, mode = "collapse")
#' if (interactive()){
#'   plot.igraph(
#'     g, shape = "square", vertex.color = "steelblue",
#'     label = V(g)$name, label.family = 11, label.cex = 0.5
#'   )
#' }
#' }
#' @rdname topic_cooccurrences
setMethod(
  "cooccurrences", "TopicModel",
  function(.Object, k, regex = NULL, docs = NULL, renumber = NULL, progress = TRUE, verbose = TRUE){
    if (verbose) message("... getting document-topic-matrix")
    topic_matrix <- topics(.Object, k = k)
    cooccurrences(topic_matrix, regex = regex, docs = docs, renumber = renumber, progress = progress, verbose = verbose)
  }
)

#' @rdname topic_cooccurrences
setMethod(
  "cooccurrences", "matrix",
  function(.Object, regex = NULL, docs = NULL, renumber = NULL, progress = TRUE, verbose = TRUE){
    k <- nrow(.Object)
    if (!is.null(regex)) .Object <- .Object[, grep(regex, colnames(.Object))]
    if (!is.null(docs)) .Object <- .Object[, which(colnames(.Object) %in% docs)]
    if (!is.null(renumber)){
      .Object <- t(apply(.Object, 1, function(x) renumber[x]))
    }
    if (verbose) message("... generating permutations")
    
    tabs <- lapply(
      1L:ncol(.Object),
      function(i){
        x <- unique(.Object[, i])
        if (length(x) == 1L) return(NULL)
        y <- t(combn(x[order(x)], m = 2))
        rbind(y, y[, c(2,1)])
      }
    )
    tab <- do.call(rbind, tabs)
    combinations_dt <- data.table(tab)
    cnt <- combinations_dt[,.N, by = c("V1", "V2")] # count cooccurrences
    setnames(cnt, old = c("V1", "V2", "N"), new = c("a", "b", "ab_count"))
    
    # counting total number of topic occurrence
    vect <- unlist(.Object)
    topic_count <- data.table(
      a = as.integer(names(table(vect))),
      a_total = as.integer(unname(table(vect)))
    )
    # feed total topic counts back into data.table
    setkeyv(topic_count, cols = "a")
    setkeyv(cnt, cols = "a")
    cnt2 <- topic_count[cnt]
    setnames(topic_count, old = c("a", "a_total"), new = c("b", "b_total"))
    setkeyv(topic_count, cols = "b")
    setkeyv(cnt2, cols = "b")
    cnt3 <- topic_count[cnt2]
    
    cnt3[, "count_ref" := cnt3[["b_total"]] - cnt3[["ab_count"]] ]
    cnt3[, "b_total" := NULL]
    
    .fn <- function(.SD){
      obj <- new(
        "features",
        size_coi = as.integer((k - 1L) * .SD[["a_total"]][1]),
        size_ref = as.integer(length(vect) - k * .SD[["a_total"]][1]),
        stat = data.table(
          b = .SD[["b"]],
          count_coi = .SD[["ab_count"]],
          count_ref = .SD[["count_ref"]]
        )
      )
      
      chisquare(obj)@stat
    }
    cnt3[, .fn(.SD), by = "a"]
  }
)
