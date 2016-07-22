#' @importFrom polmineR dotplot
setMethod("dotplot", "LDA", function(.Object, topic, n=50, ...){
  stopifnot(is.numeric(topic))
  ldaPosteriorTerms <- topicmodels::posterior(.Object)[["terms"]][topic,]
  ldaPosteriorTermsSorted <- ldaPosteriorTerms[order(ldaPosteriorTerms, decreasing=TRUE)]
  termsToPlot <- rev(ldaPosteriorTermsSorted[1:n])
  dotchart(x=termsToPlot, labels=names(termsToPlot), ...)
})
