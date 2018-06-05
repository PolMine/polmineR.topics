#' @rdname as.zoo
setGeneric("as.zoo", function(x, ...) standardGeneric("as.zoo"))


#' Generate Time Series Object From Topicmodel.
#' 
#' @param x A topicmodel inheriting from class \code{LDA}.
#' @param k Number of top topics considered during aggregation.
#' @param regex Regular expression to extract the date from the document name.
#' @param select topics to select
#' @param aggregation either "year", "quarter" or month, if NULL, no aggregation is performed.
#' @param ... further arguments
#' @importFrom zoo as.zoo as.yearqtr as.yearmon index as.Date.yearmon as.Date.yearqtr
#' @importFrom utils combn
#' @rdname as.zoo
#' @import methods
setMethod("as.zoo", "LDA", function(
  x, k = 3, regex = "^.*?(\\d{4}-\\d{2}-\\d{2}).*?$", select = NULL, aggregation = NULL
  ){
  stopifnot(is.null(aggregation) || aggregation %in% c("month", "quarter", "year"))
  # extract date from document names
  dateVector <- as.Date(gsub(regex, "\\1", x@documents), format = "%Y-%m-%d")
  topicMatrix <- topics(x, k = k)
  if (length(select) <= 1){
    topTopicsByDate <- split(topicMatrix, dateVector)
    tabled <- lapply(topTopicsByDate, table)
    DTextensive <- data.table(
      date = as.Date(unlist(lapply(names(tabled), function(x) rep(x, times=length(tabled[[x]]))))),
      topic = unlist(lapply(tabled, names)),
      count = unlist(lapply(tabled, unname))
    )
    DT <- dcast.data.table(DTextensive, date~topic, fun.aggregate=sum, value.var="count")
    setcolorder(DT, c("date", as.character(sort(as.integer(colnames(DT)[2:ncol(DT)])))))
    if (length(select) == 1) DT <- DT[,c("date", as.character(select)), with = FALSE]
  } else {
    topicCombinations <- lapply(
      1L:ncol(topicMatrix),
      function(i) t(combn(sort(topicMatrix[,i]), 2))
    )
    noCombinations <- nrow(topicCombinations[[1]])
    DTcomb <- data.table(
      date=as.Date(unlist(lapply(dateVector, function(x) rep(x, times=noCombinations)))),
      do.call(rbind, topicCombinations)
      )
    setnames(DTcomb, old = c("V1", "V2"), new = c("topic1", "topic2"))
    DTcomb <- subset(DTcomb, topic1 %in% select & topic2 %in% select)
    DTcomb[["dummy"]] <- rep(1L, times = nrow(DTcomp))
    DTcount <- DTcomb[, sum(dummy), by = c("date", "topic1", "topic2"), with = TRUE]
    setnames(DTcount, old = "V1", new = "count")
    DTcount[, key := paste(DTcount[["topic1"]], DTcount[["topic2"]], sep="-")]
    DT <- dcast.data.table(DTcount, date~key, fun = sum, value.var = "count")
  }
  xtsObject <- as.xts.data.table(DT)
  zooObject <- as.zoo(xtsObject)
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
