#' @rdname as.zoo
setGeneric("as.zoo", function(x, ...) standardGeneric("as.zoo"))


#' Generate Time Series Object From Topicmodel.
#' 
#' For each document, the \code{k} top topics are evaluated. 
#' 
#' @param x A topicmodel inheriting from class \code{LDA}.
#' @param k An \code{integer} value, thenumber of top topics evaluated during
#'   aggregation.
#' @param regex Regular expression to extract the date from the document name,
#'   defaults to match YYYY-MM-DD.
#' @param select topics to select
#' @param aggregation Either "year", "quarter" or "month", if NULL, no
#'   aggregation is performed.
#' @param ... Further arguments.
#' @importFrom zoo as.zoo as.yearqtr as.yearmon index as.Date.yearmon as.Date.yearqtr
#' @importFrom utils combn
#' @exportMethod as.zoo
#' @rdname as.zoo
#' @import methods
#' @importFrom zoo zoo
#' @importFrom stats aggregate
#' @importFrom topicmodels topics
#' @examples
#' data(BE_lda, BE_labels)
#' z <- as.zoo(BE_lda, k = 3L, aggregation = "year")
#' colnames(z) <- BE_labels
#' plot(z[,grep("Asyl", BE_labels)])
setMethod("as.zoo", "LDA", function(
  x, k = 3L, regex = "^.*?(\\d{4}-\\d{2}-\\d{2}).*?$", select = NULL, aggregation = NULL
  ){
  stopifnot(is.null(aggregation) || aggregation %in% c("month", "quarter", "year"))
  # extract date from document names
  dateVector <- as.Date(gsub(regex, "\\1", x@documents), format = "%Y-%m-%d")
  topicMatrix <- topics(x, k = k)
  if (length(select) <= 1L){
    topTopicsByDate <- split(topicMatrix, dateVector)
    tabled <- lapply(topTopicsByDate, table)
    DTextensive <- data.table(
      date = as.Date(unlist(lapply(names(tabled), function(x) rep(x, times=length(tabled[[x]]))))),
      topic = unlist(lapply(tabled, names)),
      count = unlist(lapply(tabled, unname))
    )
    DT <- dcast.data.table(DTextensive, date~topic, fun.aggregate = sum, value.var = "count")
    setcolorder(DT, c("date", as.character(sort(as.integer(colnames(DT)[2:ncol(DT)])))))
    if (length(select) == 1L) DT <- DT[,c("date", as.character(select)), with = FALSE]
  } else {
    topicCombinations <- lapply(
      1L:ncol(topicMatrix),
      function(i) t(combn(sort(topicMatrix[,i]), 2))
    )
    noCombinations <- nrow(topicCombinations[[1]])
    DTcomb <- data.table(
      date = do.call(c, lapply(dateVector, function(x) rep(x, times = noCombinations))),
      do.call(rbind, topicCombinations)
      )
    setnames(DTcomb, old = c("V1", "V2"), new = c("topic1", "topic2"))
    DTcomb <- subset(DTcomb, DTcomb[["topic1"]] %in% select & DTcomb[["topic2"]] %in% select)
    DTcomb[["dummy"]] <- rep(1L, times = nrow(DTcomb))
    DTcount <- DTcomb[, sum(DTcomb[["dummy"]]), by = c("date", "topic1", "topic2"), with = TRUE]
    setnames(DTcount, old = "V1", new = "count")
    DTcount[, "key" := paste(DTcount[["topic1"]], DTcount[["topic2"]], sep = "-")]
    DT <- dcast.data.table(DTcount, date ~ key, fun.aggregate = sum, value.var = "count")
  }
  xtsObject <- as.xts.data.table(DT)
  zooObject <- zoo(xtsObject)
  if (!is.null(aggregation)){
    zooObject <- switch(
      aggregation,
      month = aggregate(zooObject, as.Date(as.yearmon(index(zooObject)))),
      quarter = aggregate(zooObject, as.Date(as.yearqtr(index(zooObject)))),
      year = aggregate(zooObject, as.Date(paste(gsub("^(\\d{4}).*?$", "\\1", index(zooObject)), "-01-01", sep="")))
    )
  }
  zooObject
})
