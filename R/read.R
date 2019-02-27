#' @importFrom topicmodels topics
#' @importMethodsFrom polmineR name
#' @export get_highlight_list
#' @rdname read_TopicModel_method
get_highlight_list <- function(.Object, partition_obj, no_topics = 3L, no_token = 20L){
  highlight_colors <- c("yellow", "lightgreen", "orange", "blue", "red", "brown")
  if (no_topics == 1){
    topics_in_partition <- topics(.Object, k = no_topics)[name(partition_obj)]
  } else {
    topics_in_partition <- topics(.Object, k = no_topics)[,name(partition_obj)]
    topics_matrix <- terms(.Object, k = no_token)[, paste("Topic", topics_in_partition, sep = " ")]
    topics_df <- as.data.frame(topics_matrix)
    color_labels <- paste("[", highlight_colors[1L:no_topics], "]", sep = "")
    # add labels to the data.frame
    for (i in 1L:ncol(topics_df)) Hmisc::label(topics_df[[i]]) <- color_labels[i]
    # View(topicsDf)
  }
  topics_vocab <- lapply(
    topics_in_partition,
    function(i){
      col_to_get <- paste("Topic", as.character(i), sep = " ")
      terms_to_highlight <- terms(.Object, k = no_token)[, col_to_get]
      terms_to_remove <- grep('[;,:\\.\\-\\(\\)]', terms_to_highlight)
      if (length(terms_to_remove) > 0){
        for (j in rev(terms_to_remove)) terms_to_highlight <- terms_to_highlight[-j]
      }
      # paste("\\b", terms_to_highlight, "\\b", sep = "")
      terms_to_highlight
    })
  names(topics_vocab) <- highlight_colors[1L:no_topics]
  topics_vocab
}


#' Fulltext Inspection for TopicModel.
#' 
#' Highlight the vocabulary indicative of the top terms (number specified by
#' \code{no_topics}) for the top topics (number specified by
#' \code{no_topics}present in a \code{partition}.
#'
#' @param .Object Object of class \code{TopicModel}, or a class inheriting from
#'   it.
#' @param no_topics The number of the most prevalant topics in a text that will be highlighted.
#' @param no_token An \code{integer} value, the number of tokens indicative of a
#'   topic according to the input topic model to be be highlighted
#' @param partition_obj A \code{partition} object, the partition to display.
#' @rdname read_TopicModel_method
#' @importMethodsFrom polmineR read
#' @exportMethod read
#' @examples
#' data(BE_lda)
#' data(BE_labels)
#' data(BE_exclude)
#' 
#' BE <- Topicanalysis$new(topicmodel = BE_lda)
#' BE$labels <- BE_labels
#' BE$type <- "plpr_partition"
#' 
#' topic_flucht <- 125L
#' topic_integration <- 241
#' BE$docs(x = 125L, y = 241L)
#' 
#' # As sample data, we use a corpus of plenary debates in the 
#' # 'Berliner Abgeordnetenhaus'. It is somewhat big and not included
#' # in the package by default. 
#' 
#' \dontrun{
#' polmineR::use("topicanalysis")
#' if (!"BE" %in% corpus()$corpus){
#'   cwbtools::corpus_install(
#'     pkg = "topicanalysis",
#'     tarball = "http://polmine.sowi.uni-due.de/corpora/cwb/berlin/be.tar.gz"
#'   )
#'   polmineR::use("topicanalysis") # to activate corpus 'BE'
#' }
#' 
#' p <- partition("BE", date = "2005-04-28", who = "Körting")
#' p <- polmineR::as.speeches(p, s_attribute_name = "who")[[4]]
#' 
#' read(BE_lda, p, no_token = 150)
#' }
setMethod("read", "TopicModel", function(.Object, partition_obj, no_topics = 3L, no_token = 20L){
  highlight <- get_highlight_list(.Object = .Object, partition_obj = partition_obj, no_topics = no_topics, no_token = no_token)
  read(partition_obj, highlight = highlight, cqp = FALSE, interjections = TRUE)
})
