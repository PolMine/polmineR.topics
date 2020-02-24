#' Get topic cooccurrences.
#' 
#' Topic models describe documents as composed of different topics. This 
#' property can be used to obtain co-occuurrence statistics of topics.
#' 
#' @param .Object Either an object inheriting from the \code{TopicModel} class
#'   (such as \code{LDA_Gibbs}), or a \code{matrix} with the topics present in
#'   documents. If \code{.Object} is a \code{matrix}, each column is expected to
#'   represent the top k topics present in a document. The \code{matrix}
#'   returned by the \code{topics}-method from the \code{topicmodels} package
#'   was used to develop the method, but document-term-matrices derived
#'   otherwise tools may work as well.
#' @param k An \code{integer} value, the \code{k} first topics to consider when
#'   deriving the document-topic-matrix from a trained topicmodel.
#' @param regex If not \code{NULL} (default), the procedure will be limited to
#'   document names matched by the regular expression stated by \code{regex}.
#' @param docs If not \code{NULL}, the procedure will be limited to documents
#'   matching the character string.
#' @param renumber If not \code{NULL} (the default), topics in the
#'   document-topic-matrix will be renumbered according to the argument
#'   \code{renumber}. If \code{renumber} is an integer vector, the length of
#'   this vector is required to match the number of topics in topic model. Each
#'   topic i present in the document-topic matrix will be mapped on the value at
#'   position i of the vector. If \code{renumber} is a \code{list} of integer
#'   vectors, these vectors are considered as groups of topics that represent a
#'   single implict "super-topic". For each integer vector present in the
#'   \code{list}, the topic numbers present in the document-topic-matrix will be
#'   matched on the first value of the vector.
#' @param method The statistic to calculate co-occurrences, "chisquare" by
#'   default.
#' @param verbose A \code{logical} value, whether to output messages on the
#'   state of affairs.
#' @param progress A \code{logical} value, whether to show a progress bar.
#' @return A \code{data.table} with co-occurrence statistics with at least the
#'   following columns:
#' \describe{
#'   \item{a}{number of the topic of interest}
#'   \item{b}{number of the co-occurring topic}
#'   \item{b_total}{number of total occurrences of topic b; if the
#'   document-topic matrix has been renumbered, the times at least one of the
#'   topics in a group occurs in a docuent}
#'   \item{b_total}{number of total occurrences of topic a; if the
#'   document-topic matrix has been renumbered, the times at least one of the
#'   topics in a group occurs in a docuent}
#'   \item{count_coi}{number of joint occurrences of topics a and b}
#'   \item{count_ref}{number of occurrences of b without co-occurring of a}
#' }
#' If argument \code{method} is not \code{NULL}, additional columns will be 
#' included in the topic co-occurrence table. E.g. if \code{method} is "chisquare",
#' a column "exp_coi", will report the expected number of occurrences of b together with a,
#' column "chisquare" will report the value of the chi squared test, and a 
#' column "rank_chisquare" will report the rank of the statistical significance of the 
#' co-occurrence of a and b according to the chi squared test.
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
#' # Using the cooccurrence data for generating a network visualisation
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
#'   igraph::plot.igraph(
#'     g, shape = "square", vertex.color = "steelblue",
#'     label = igraph::V(g)$name, label.family = 11, label.cex = 0.5
#'   )
#' }
#' }
#' 
#' # Example how to use the argument 'renumber' if a concept is represented by 
#' # several topics 
#' renumber_li <- list(
#'   school = grep("Grundschule", BE_labels),
#'   cummunity = grep("Gemeindeentwicklung", BE_labels),
#'   traffic = grep("Verkehrsmittel", BE_labels)
#' )
#' dt <- cooccurrences(BE_lda, k = 3L, renumber = renumber_li)
#' dt[a == grep("Grundschule", BE_labels)[1]][chisquare > 10.83]
#' @rdname topic_cooccurrences
setMethod(
  "cooccurrences", "TopicModel",
  function(.Object, k, regex = NULL, docs = NULL, renumber = NULL, method = "chisquare", progress = TRUE, verbose = TRUE){
    if (verbose) message("... getting document-topic-matrix")
    topic_matrix <- topics(.Object, k = k)
    cooccurrences(topic_matrix, regex = regex, docs = docs, renumber = renumber, method = "chisquare", progress = progress, verbose = verbose)
  }
)

#' @rdname topic_cooccurrences
setMethod(
  "cooccurrences", "matrix",
  function(.Object, regex = NULL, docs = NULL, renumber = NULL, method = "chisquare", progress = TRUE, verbose = TRUE){
    k <- nrow(.Object)
    if (!is.null(regex)) .Object <- .Object[, grep(regex, colnames(.Object))]
    if (!is.null(docs)) .Object <- .Object[, which(colnames(.Object) %in% docs)]
    if (!is.null(renumber)){
      # If renumber is a list, generate vector
      if (is.list(renumber)){
        renumber_vec <- seq_len(max(.Object))
        for (i in seq_len(length(renumber))){
          renumber_vec[renumber[[i]][2L:length(renumber[[i]])]] <- as.integer(renumber[[i]][1L])
        }
        renumber <- renumber_vec
      }
      # Check validity of input
      if (!is.vector(renumber)) stop("Argument renumber is required to be either a list or a vector.")
      if (!is.integer(renumber)){
        renumber <- as.integer(renumber)
        if (any(is.na(renumber))){
          stop("Argument renumber may not contain NAs. NAs may result from coercing non-integer input to integer.")
        }
      }
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
    topics_vector <- unlist(apply(.Object, 2, unique))
    topics_tabled <- table(topics_vector)
    topic_count_dt <- data.table(
      a = as.integer(names(topics_tabled)),
      a_total = as.integer(unname(topics_tabled))
    )
    # feed total topic counts back into data.table
    cnt2 <- topic_count_dt[cnt, on = "a"]
    setnames(topic_count_dt, old = c("a", "a_total"), new = c("b", "b_total"))
    cnt3 <- topic_count_dt[cnt2, on = "b"]
    cnt3[, "count_ref" := cnt3[["b_total"]] - cnt3[["ab_count"]] ]
    
    setnames(cnt3, old = "ab_count", new = "count_coi")
    if (!is.null(method)){
      .fn <- function(.SD){
        obj <- new(
          "features",
          size_coi = as.integer((k - 1L) * .SD[["a_total"]][1]),
          size_ref = as.integer(length(topics_vector) - k * .SD[["a_total"]][1]),
          stat = copy(.SD)
        )
        do.call(method, list(.Object = obj))@stat
      }
      y <- cnt3[, .fn(.SD), by = "a"]
    } else {
      y <- cnt3
    }
    y
  }
)
