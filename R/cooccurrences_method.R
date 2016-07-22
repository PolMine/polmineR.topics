#' topicCooccurence 
#' 
#' Class required to perform the calculation of the chisquare test.
#' 
#' @param sizeCoi size of the coi
#' @param sizeRef size of the ref
#' @param method method
setClass(
  "topicCooccurrence",
  slots=c(sizeCoi="integer", sizeRef="integer", method="character"),
  contains=c("textstat")
)

#' Calculate cooccurrences for a topicmodel
#' 
#' @param .Object the object
#' @param k the k first topics to consider
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
setMethod("cooccurrences", "TopicModel", function(.Object, k, regex=NULL, docs=NULL, progress=TRUE, verbose=TRUE){
  topicMatrix <- topics(.Object, k=k)
  if (!is.null(regex)) topicMatrix <- topicMatrix[,grep(regex, colnames(topicMatrix))]
  if (!is.null(docs)) topicMatrix <- topicMatrix[,which(colnames(topicMatrix) %in% docs)]
  tabs <- lapply(
    c(1:ncol(topicMatrix)),
    function(i){x <- topicMatrix[,i]; t(combn(x[order(x)], m=2))}
    )
  tab <- do.call(rbind, tabs)
  DT <- data.table(tab)
  DT2 <- DT[,.N, by=.(V1, V2)]
  setnames(DT2, old=c("V1", "V2", "N"), new=c("x", "y", "y_count_coi"))
  vect <- unlist(topicMatrix)
  topicCount <- data.table(table(vect))
  setnames(topicCount, old=c("vect", "N"), new=c("x", "x_count"))
  topicCount[,x := as.integer(topicCount[["x"]])]
  setkey(topicCount, x)
  setkey(DT2, x)
  DT3 <- topicCount[DT2]
  setnames(topicCount, old=c("x", "x_count"), new=c("y", "y_count"))
  setkey(topicCount, "y")
  setkey(DT3, "y")
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

# 
# setMethod("cooccurrences", "TopicModel", function(.Object, k, regex=NULL, docs=NULL, progress=TRUE, verbose=TRUE){
#   if (verbose) message("... get topics for docs")
#   topicMatrix <- topics(.Object, k=k)
#   topicDf <- as.data.frame(topicMatrix)
#   # only necessary if you assume that topics have been filtered out before
#   if (!is.null(regex)) topicDf <- topicDf[,grep(regex, colnames(topicDf))]
#   if (!is.null(docs)) topicDf <- topicDf[,which(colnames(topicDf) %in% docs)]
#   if (ncol(topicDf) == 0) return(NULL)
#   topics <- unique(unlist(topicDf))
#   topics <- topics[order(topics)]
#   if (verbose) message("... calculations")
#   if (progress) pb <- txtProgressBar(max=length(topics), style = 3)
#   .cooccurrence <- function(topic) {
#     # get docs in which topic is present
#     if (progress) setTxtProgressBar(pb, value=topic)
#     xPresent <- sapply(topicDf, function(top) topic %in% top)
#     count <- table(xPresent)
#     xCount <- unclass(count)["TRUE"]
#     yCount <- table(unlist(topicDf[which(xPresent == TRUE)]))
#     DT <- data.table(
#       x = topic,
#       y = as.integer(names(yCount)),
#       count_coi = as.vector(yCount)
#     )
#     setkey(DT, y)
#     prep <- table(unlist(topicDf[which(xPresent == FALSE)]))
#     REF <- data.table(y=as.integer(names(prep)), count_ref=as.vector(prep))
#     setkey(REF, y)
#     DT <- DT[REF]
#     DT <- subset(DT, is.na(topic) == FALSE)
#     DT[, "x_count" := xCount, with = FALSE]
#     obj <- new(
#       "topicCooccurrence",
#       sizeCoi=length(unlist(topicDf[which(xPresent == TRUE)])) - xCount,
#       sizeRef=length(unlist(topicDf[which(xPresent == FALSE)])),
#       stat=DT
#     )
#     obj <- chisquare(obj)
#     RET <- obj@stat
#     setcolorder(RET, c("x", "x_count", "y", "count_coi", "count_ref", "exp_coi", "chisquare", "rank_chisquare"))
#     setnames(RET, old=c("count_coi", "count_ref"), new=c("y_count_coi","y_count_ref"))
#     RET
#   }
#   tabs <- lapply(topics, .cooccurrence)
#   if (progress) close(pb)
#   if (verbose) message("... wrapping things up")
#   rbindlist(tabs)
# })
