#' Dot Plot for Terms of a Topic.
#' 
#' @param .Object An object of class \code{LDA} (or inheriting from it).
#' @param topic An \code{integer} value, the number of the topic to be visualized.
#' @param n An \code{integer} value, the number of tokens considered.
#' @param ... Further arguments passed into a call to \code{dotchart}.
#' @importFrom polmineR dotplot
#' @importFrom graphics dotchart
#' @importFrom topicmodels posterior
#' @exportMethod dotplot
#' @examples
#' data(BE_lda)
#' dotplot(BE_lda, topic = 125)
#' dotplot(BE_lda, topic = 210, n = 25)
setMethod("dotplot", "LDA", function(.Object, topic, n = 50L, ...){
  if (missing(topic)) stop("The argument 'topic' is missing, but is required.")
  n <- as.integer(n)
  topic <- as.integer(topic)
  stopifnot(is.integer(topic), is.integer(n))
  
  terms <- posterior(.Object)[["terms"]][topic,]
  terms_sorted <- terms[order(terms, decreasing = TRUE)]
  terms_to_plot <- rev(terms_sorted[1L:n])
  dotchart(x = terms_to_plot, labels = names(terms_to_plot), ...)
})
