#' @importFrom topicmodels topics
#' @importMethodsFrom polmineR name
.makeHighlightList <- function(.Object, partitionObject, noTopics=3, noToken=20){
  highlightColors <- c("yellow", "lightgreen", "orange", "blue", "red", "brown")
  if (noTopics == 1){
    # if k=1, topics() returns a vector
    topicsInPartition <- topics(.Object, k=noTopics)[name(partitionObject)]
  } else {
    topicsInPartition <- topics(.Object, k=noTopics)[,name(partitionObject)]
    topicsMatrix <- terms(.Object, k=noToken)[, paste("Topic", topicsInPartition, sep=" ")]
    topicsDf <- as.data.frame(topicsMatrix)
    colorLabels <- paste("[", highlightColors[1:noTopics], "]", sep="")
    # add labels to the data.frame
    for (i in c(1:ncol(topicsDf))) Hmisc::label(topicsDf[[i]]) <- colorLabels[i]
    # View(topicsDf)
  }
  topicsVocab <- lapply(
    topicsInPartition,
    function(i){
      colToGet <- paste("Topic", as.character(i), sep=" ")
      termsToHighlight <- terms(.Object, k=noToken)[, colToGet]
      termsToRemove <- grep('[;,:\\.\\-\\(\\)]', termsToHighlight)
      if (length(termsToRemove) > 0){
        for (j in rev(termsToRemove)) termsToHighlight <- termsToHighlight[-j]
      }
      paste("\\b", termsToHighlight, "\\b", sep="")
    })
  names(topicsVocab) <- highlightColors[c(1:noTopics)]
  topicsVocab
}


#' read-method for TopicModel
#' 
#' foo
#'
#' @param .Object object of the TopicModel class, or inheriting from it
#' @param noTopics the number of the most prevalant topics in a text that will be highlighted
#' @param noToken the number of tokens to be be highlighted
#' @param partitionObject partition to be read
#' @rdname read_TopicModel_method
#' @importMethodsFrom polmineR read
#' @exportMethod read
setMethod("read", "TopicModel", function(.Object, partitionObject, noTopics=3, noToken=20){
  topicsVocab <- .makeHighlightList(.Object=.Object, partitionObject=partitionObject, noTopics=noTopics, noToken=noToken)
  read(partitionObject, highlight=topicsVocab, cqp=FALSE, interjections=TRUE)
})
