#' @rdname as.zoo
setGeneric("as.zoo", function(x, ...) standardGeneric("as.zoo"))


#' Generate Time Series Object From Topicmodel.
#' 
#' For each document, the \code{k} top topics are evaluated. 
#' 
#' The document names of the \code{LDA} object are assumed to include a date
#' that can be extracted by using a regular expression (argument \code{regex}).
#' If argument \code{select} is \code{NULL}, the time series is prepared for
#' all topics. If \code{select} is a length-one \code{integer} value, the time
#' series is reported only for single topic. If \code{select} is a length-two
#' \code{integer} vector, the dispersion of the co-occurrence of the two topics
#' across time is returned.
#' 
#' @param x A topicmodel inheriting from class \code{LDA}.
#' @param k An \code{integer} value, the number of top topics evaluated.
#' @param regex Regular expression to extract the date from the document name,
#'   defaults to match pattern YYYY-MM-DD.
#' @param select An \code{integer} vector, topics to select.
#' @param aggregation Either "year", "quarter" or "month", if \code{NULL}, no
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
#' 
#' # The document names are assumed to include a date that can be 
#' # extracted and parsed
#' head(BE_lda@documents)
#' 
#' # Get time series for all topics
#' z <- as.zoo(BE_lda, k = 3L, aggregation = "year")
#' dim(z)
#' colnames(z) <- BE_labels
#' plot(z[,grep("Asyl", BE_labels)]) # subsequent subsetting
#' 
#' # Get time series for a single topic
#' z <- as.zoo(BE_lda, k = 3L, aggregation = "year", select = grep("Asyl", BE_labels))
#' plot(z)
#' 
#' # Get time series for the co-occurrence of two topics
#' z <- as.zoo(BE_lda, k = 3L, select = c(28L, 203L), aggregation = "month")
#' z <- as.zoo(
#'   BE_lda, k = 3L,
#'   select = c(
#'     grep("Asyl", BE_labels),
#'     grep("Integration", BE_labels)
#'   )
#' )
#' 
setMethod("as.zoo", "LDA", function(
  x, k = 3L, regex = "^.*?(\\d{4}-\\d{2}-\\d{2}).*?$", select = NULL, aggregation = NULL
  ){
  stopifnot(is.null(aggregation) || aggregation %in% c("month", "quarter", "year"))
  # extract date from document names
  date_vector <- as.Date(gsub(regex, "\\1", x@documents), format = "%Y-%m-%d")
  topic_matrix <- topics(x, k = k)
  if (length(select) <= 1L){
    topic_dt <- data.table(topic_matrix)
    top_topics_by_date <- split(t(topic_dt), date_vector)
    # top_topics_by_date <- split(topic_matrix, date_vector)
    tabled <- lapply(top_topics_by_date, table)
    dt_ext <- data.table(
      date = as.Date(unlist(lapply(names(tabled), function(x) rep(x, times = length(tabled[[x]]))))),
      topic = unlist(lapply(tabled, names)),
      count = unlist(lapply(tabled, unname))
    )
    dt <- dcast.data.table(dt_ext, date ~ topic, fun.aggregate = sum, value.var = "count")
    setcolorder(dt, c("date", as.character(sort(as.integer(colnames(dt)[2L:ncol(dt)])))))
    if (length(select) == 1L) dt <- dt[, c("date", as.character(select)), with = FALSE]
  } else {
    topic_comb <- lapply(
      1L:ncol(topic_matrix),
      function(i) t(combn(sort(topic_matrix[,i]), 2))
    )
    no_comb <- nrow(topic_comb[[1]])
    dt_comb <- data.table(
      date = do.call(c, lapply(date_vector, function(x) rep(x, times = no_comb))),
      do.call(rbind, topic_comb)
      )
    setnames(dt_comb, old = c("V1", "V2"), new = c("topic1", "topic2"))
    dt_comb <- subset(dt_comb, dt_comb[["topic1"]] %in% select & dt_comb[["topic2"]] %in% select)
    dt_count <- dt_comb[, .N, by = c("date", "topic1", "topic2"), with = TRUE]
    setnames(dt_count, old = "N", new = "count")
    dt_count[, "key" := paste(dt_count[["topic1"]], dt_count[["topic2"]], sep = "-")]
    dt <- dcast.data.table(dt_count, date ~ key, fun.aggregate = sum, value.var = "count")
  }
  xts_obj <- as.xts.data.table(dt)
  zoo_obj <- zoo(xts_obj)
  colnames(zoo_obj) <- colnames(xts_obj)
  if (!is.null(aggregation)){
    zoo_obj <- switch(
      aggregation,
      month = aggregate(zoo_obj, as.Date(as.yearmon(index(zoo_obj)))),
      quarter = aggregate(zoo_obj, as.Date(as.yearqtr(index(zoo_obj)))),
      year = aggregate(zoo_obj, as.Date(paste(gsub("^(\\d{4}).*?$", "\\1", index(zoo_obj)), "-01-01", sep="")))
    )
    colnames(zoo_obj) <- colnames(xts_obj)
  }
  zoo_obj
})
