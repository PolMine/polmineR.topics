#' Cooccurence of Topics.
#' 
#' Class required to perform the calculation of the chisquare test.
#' 
#' @param sizeCoi size of the coi
#' @param sizeRef size of the ref
#' @param method method
#' @import polmineR
setClass(
  "topicCooccurrence",
  slots = c(
    sizeCoi = "integer",
    sizeRef = "integer",
    method = "character"
    ),
  contains = c("textstat")
)

#' Calculate cooccurrences for a topicmodel
#' 
#' @param .Object the object
#' @param k the k first topics to consider
#' @param regex if provided, the procedure will be limited to document names matching the regex (defaults to NULL)
#' @param docs if provided, the procedure will be limited to documents matching the character string
#' @param renumber if not NULL (the default), an integer vector (length = number of topics) supplying new topic numbers
#' @param verbose logical
#' @examples
#' \dontrun{
#' setwd("/Users/blaette/Lab/repos/arenen/analysis/corpusConsolidation/BT")
#' lda <- readRDS("lda_bt_arbeit_50.RData")
#' topic_cooccurrencs <- cooccurrences(lda, k=3)
#' dt <- subset(topic_cooccurrences, chisquare >= threshold, select=c("x", "y"))
#' igraph::graph_from_data_frame(dt)
#' }
#' @importMethodsFrom polmineR cooccurrences
#' @exportMethod cooccurrences
#' @import data.table
setMethod(
  "cooccurrences", "TopicModel",
  function(.Object, k, regex = NULL, docs = NULL, renumber = NULL, progress = TRUE, verbose = TRUE){
    if (verbose) message("... getting document-topics-matrix")
    topicMatrix <- topics(.Object, k=k)
    if (!is.null(regex)) topicMatrix <- topicMatrix[,grep(regex, colnames(topicMatrix))]
    if (!is.null(docs)) topicMatrix <- topicMatrix[,which(colnames(topicMatrix) %in% docs)]
    if (!is.null(renumber)) topicMatrix <- t(apply(topicMatrix, 1, function(x) renumber[x]))
    if (verbose) message("... generating permutations")
    tabs <- lapply(
      c(1:ncol(topicMatrix)),
      function(i, ...){x <- topicMatrix[,i]; t(combn(x[order(x)], m = 2))}
    )
    tab <- do.call(rbind, tabs)
    DT <- data.table(tab)
    DT2 <- DT[,.N, by=.(V1, V2)] # count cooccurrences
    
    # generate the same table, with V1 and V2 twisted
    DTrev <- copy(DT2)
    setcolorder(DTrev, neworder = c("V2", "V1", "N"))
    setnames(DTrev, old = c("V2", "V1"), new = c("V1", "V2"))
    DT2 <- rbindlist(list(DT2, DTrev))
    
    setnames(DT2, old=c("V1", "V2", "N"), new=c("x", "y", "y_count_coi"))
    # counting total number of topic occurrence
    vect <- unlist(topicMatrix)
    topicCount <- data.table(table(vect))
    setnames(topicCount, old = c("vect", "N"), new = c("x", "x_count"))
    topicCount[,x := as.integer(topicCount[["x"]])] # coerce to integer
    # feed total topic counts back into data.table
    setkeyv(topicCount, cols = "x")
    setkeyv(DT2, cols = "x")
    DT3 <- topicCount[DT2]
    setnames(topicCount, old = c("x", "x_count"), new = c("y", "y_count"))
    setkeyv(topicCount, cols = "y")
    setkeyv(DT3, cols = "y")
    DT4 <- topicCount[DT3]
    
    DT4[, y_count_ref := y_count - y_count_coi]
    DT4[, y_count := NULL]
    setnames(DT4, old=c("y_count_coi", "y_count_ref"), new=c("count_coi", "count_ref"))
    # setcolorder(DT4, c("x", "x_count", "y", "y_count_coi", "y_count_ref"))
    
    .cooc <- function(X){
      obj <- new(
        "topicCooccurrence",
        sizeCoi=as.integer(k * X[["x_count"]][1] - X[["x_count"]][1]),
        sizeRef=as.integer(length(vect) - k * X[["x_count"]][1]),
        stat=copy(X)
      )
      chisquare(obj)@stat
    }
    DT5 <- DT4[, .cooc(.SD), by=.(x)]
    DT5
  })
