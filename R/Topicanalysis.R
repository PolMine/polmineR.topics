#' Topicanalysis Class.
#' 
#' Analyse topicmodels.
#' 
#' @field topicmodel A topicmodel of class \code{TopicModel}, generated from
#'   package \code{topicmodels}.
#' @field posterior Slot to store posterior, not used at this point.
#' @field terms The \code{matrix} with the terms of a topicmodel. Keeping the
#'   terms may speed up subsequent operations.
#' @field bundle A \code{partition_bundle}, required to use method \code{read}
#'   to access full text.
#' @field labels A \code{character} vector, labels for the topics.
#' @field name A name for the \code{Topicanalysis} object. Useful if combining
#'   several objects into a bundle.
#' @field categories A \code{character} vector with categories.
#' @field grouping Not used at this stage.
#' @field exclude Topics to exclude from further analysis.
#' @field type Corpus type, necessary for applying correct template for fulltext output.
#' 
#' @section Arguments:
#' \describe{
#'   \item{new}{New value for a label or a category.}
#'   \item{n}{Number of a topic.}
#'   \item{n_words}{An \code{integer}, the number of words to be displayed in a wordcloud.}
#'   \item{x}{Number or name of a topics.}
#'   \item{y}{Number or name of a topic cooccurring with x.}
#'   \item{k}{Number of top topics of a document to consider.}
#'   \item{exclude}{A \code{logical} value, whether to to exclude topics
#'   earmarked in logical vector in field exclude.}
#'   \item{aggregation}{Level of aggregation of \code{as.zoo} method.}
#'   \item{...}{Further parameters passed to worker function (such as
#'   \code{wordcloud::wordcloud} when calling \code{$wordcloud()}, for
#'   instance).}
#'   \item{regex}{A regular expression that will limit the evaluation to those documents only
#'   that are matched by the regular expression.}
#' }
#' 
#' @section Methods:
#' \describe{
#'   \item{\code{$initialize(topicmodel)}}{Instantiate new \code{Topicanalysis}
#'   object. Upon initialization, labels will be the plain numbers of the
#'   topics, all exclude values are \code{FALSE}.}
#'   \item{\code{$cooccurrences(k = 3, regex = NULL, docs = NULL, renumber = NULL,
#'   progress = TRUE, exclude = TRUE)}}{Get cooccurrences of topics. Arguments are
#'   documented with the S4 cooccurrences-method for TopicModel-objects.}
#'   \item{\code{$relabel(n, new)}}{Relabel topic \code{n}, assigning new label
#'   \code{new}.}
#'   \item{\code{$add_category(new)}}{Add \code{new}, a \code{character} vector
#'   as a new category to the \code{character} vector in the field
#'   \code{category}.}
#'   \item{\code{$ignorance(n, new)}}{Exclude topic \code{n} (i.e. add to ignore).}
#'   \item{\code{$wordcloud(n, n = 50, ...)}}{Generate wordcloud for topic
#'   \code{n}, with \code{n_words} words. Further arguments can be passed into
#'   \code{wordcloud::wordcloud} usint the three dots.}
#'   \item{\code{$docs(x, y = NULL, n = 3L, s_attributes = NULL)}}{Get documents
#'   where topic \code{x} occurrs among the top \code{n} topics. If \code{y} is 
#'   provided, documents are returned where \code{x} and \code{y} are among the
#'   \code{n} top topics. If \code{x} or \code{y} are provided as a \code{character}
#'   vector, the method will look up this label in the \code{labels} field.}
#'   \item{\code{$read(x, n = 3, no_token = 100)}}{Read document \code{x},
#'   highlighting the number of topics specified by \code{n}, indicated by
#'   \code{no_token}.}
#'   \item{\code{$as.zoo(x = NULL, y = NULL, k = 3, exclude = TRUE, aggregation
#'   = c(NULL, "month", "quarter", "year"))}}{Generate \code{zoo} object from
#'   topicmodel.}
#'   \item{\code{$compare(x, ...)}}{Compare the similarity of two topicmodels.}
#'   \item{\code{$find_topics(x, n = 100, word2vec = NULL)}}{Find a topic.}
#' }
#' @importFrom polmineR as.speeches
#' @examples
#' data(BE_lda)
#' data(BE_labels)
#' data(BE_exclude)
#' 
#' BE <- Topicanalysis$new(topicmodel = BE_lda)
#' BE$labels <- BE_labels
#' BE$exclude <- BE_exclude
#' BE$exclude <- grepl("^\\((split|)\\)$", BE$labels)
#' BE$name <- "Berlin"
#' BE$type <- "plpr_partition"
#' 
#' z <- BE$as.zoo(x = "Flucht, Asyl, vorläufiger Schutz", aggregation = "year")
#' plot(z)
#' 
#' y <- BE$as.zoo(
#'   x = grep("Asyl", BE_labels),
#'   y = grep("Europ", BE_labels),
#'   aggregation = "year"
#' )
#' plot(y)
#' 
#' BE$exclude <- grepl("^\\(.*?\\)$", BE$labels)
#' dt <- BE$cooccurrences(k = 3L, exclude = TRUE)
#' dt_min <- dt[chisquare >= 10.83]
#' 
#' 
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
#' topic_flucht <- 125L
#' topic_integration <- 241
#' BE$docs(x = "Flucht, Asyl, vorläufiger Schutz")
#' BE$docs(x = grep("Flucht", BE$labels))
#' BE$docs(x = 125L)
#' docs <- BE$docs(x = 125L, y = 241L)
#' 
#' \dontrun{
#' li <- lapply(
#'   docs, 
#'   function(doc){
#'     polmineR::as.speeches(
#'       polmineR::partition(
#'         "BE",
#'         who = gsub("^(.*?)_.*$", "\\1", doc),
#'         date = gsub("^.*(\\d{4}-\\d{2}-\\d{2})_\\d+$", "\\1", doc)
#'       ),
#'       s_attribute_name = "who"
#'     )[[as.integer(gsub("^.*?_(\\d+)$", "\\1", doc))]]
#' })
#' BE$bundle <- as.partition_bundle(li)
#' 
#' read(BE$topicmodel, BE$bundle[[1]], n = 3L, no_token = 250)
#' read(BE$topicmodel, BE$bundle[[2]], n = 3L, no_token = 250)
#' read(BE$topicmodel, BE$bundle[[3]], n = 3L, no_token = 250)
#' for (doc in docs){
#'   print(doc)
#'   p <- BE$bundle[[doc]]
#'   read(BE$topicmodel, p, n = 3L, no_token = 250)
#'   readline(prompt = "Hit any key to continue.")
#' }
#' }
#' 
#' #############################
#' 
#' data(SL_lda)
#' data(SL_labels)
#' data(SL_exclude)
#' 
#' SL <- Topicanalysis$new(topicmodel = SL_lda)
#' SL$labels <- SL_labels
#' SL$exclude <- SL_exclude
#' SL$exclude <- grepl("^\\((split|)\\)$", SL$labels)
#' SL$name <- "Hamburg"
#' 
#' cp_1 <- BE$compare(SL, BE)
#' cp_2 <- BE$compare(SL, BE)
#' 
#' @export Topicanalysis
#' @name Topicanalysis
#' @aliases Topicanalysis
#' @importFrom R6 R6Class
#' @importFrom wordcloud wordcloud
#' @importFrom topicmodels posterior terms
Topicanalysis <- R6Class(
  
  "Topicanalysis",
  
  public = list(

    topicmodel = NULL,
    posterior = NULL,
    terms = NULL,
    bundle = NULL,
    labels = c(),
    name = c(),
    categories = c(),
    grouping = list(),
    exclude = c(),
    type = NULL,
    
    initialize = function (topicmodel){
      self$topicmodel <- topicmodel
      self$labels <- as.character(1:topicmodel@k)
      self$exclude <- as.logical(rep(FALSE, times = topicmodel@k))
      self$categories <- character()
      invisible(self)
    },
    
    relabel = function(n, new){
      stopifnot(is.character(new))
      if (as.character(n) %in% names(self$labels)){
        self$labels[which(names(self$labels) == as.character(n))] <- new  
      } else {
        self$labels[n] <- new
      }
    },

    add_category = function(new){
      stopifnot(is.character(new))
      if (new != ""){
        new_categories <- c(new, self$categories)
        self$categories <- new_categories[order(new_categories)]
      }
    },

    ignorance = function(n, new){
      stopifnot(is.numeric(n), is.logical(new))
      self$exclude[n] <- new
    },

    cooccurrences = function(k = 3, regex = NULL, docs = NULL, renumber = NULL, progress = TRUE, exclude = TRUE){
      if (is.null(renumber)){
        cooc <- cooccurrences(
          self$topicmodel, k = k, regex = regex, docs = docs,
          progress = progress
        )
        cooc[, "a_label" := self$labels[ cooc[["a"]] ] ]
        cooc[, "b_label" := self$labels[cooc[["b"]] ] ]
        if (exclude){
          cooc <- cooc[!cooc$a %in% which(self$exclude == TRUE),]
          cooc <- cooc[!cooc$b %in% which(self$exclude == TRUE),]
        }
        return(cooc)
      } else {
        stopifnot(is.integer(renumber))
        cooc <- cooccurrences(
          self$topicmodel, k = k, regex = regex, docs = docs, renumber = unname(renumber),
          progress = progress
        )
        if (exclude){
          cooc <- cooc[x > 0][y > 0]
          labelVector <- renumber[which(renumber > 0)]
          labelVector2 <- sapply(split(labelVector, names(labelVector)), function(x) unique(x))
          labelVector3 <- sort(labelVector2)
          labelVector4 <- names(labelVector3)
          names(labelVector4) <- as.character(labelVector3)
          if (!is.null(names(renumber))){
            cooc[, x_label := labelVector4[as.character(cooc[["x"]])]]
            cooc[, y_label := labelVector4[as.character(cooc[["y"]])]]
          }
          
        }
        return(cooc)
      }
    },

    wordcloud = function(n, n_words = 50, ...){
      tokens <- posterior(self$topicmodel)[["terms"]][n, ]
      tokens <- tokens[order(tokens, decreasing = TRUE)][1:n_words]
      wordcloud::wordcloud(words = names(tokens), freq = unname(tokens), ...)
    },

    docs = function(x, y = NULL, n = 3L, s_attributes = NULL, regex = NULL){
      topic_matrix <- topics(self$topicmodel, k = n)
      if (!is.null(regex)) topic_matrix <- topic_matrix[,grepl(regex, colnames(topic_matrix))]
      if (is.character(x)) x <- which(self$labels == x)
      x_present <- apply(topic_matrix, 2, function(column) any(x %in% column))
      
      if (!is.null(y)){
        if (is.character(y)) y <- which(self$labels == y)
        y_present <- apply(topic_matrix, 2, function(column) any(y %in% column))
        xy_present <- apply(matrix(data = c(x_present, y_present), ncol = 2), 1, all)
        if (length(xy_present) > 0){
          return( colnames(topic_matrix)[xy_present] )
        } else {
          return( character() )
        }
      } else {
        if (length(x_present) > 0){
          return( colnames(topic_matrix)[x_present] )
        } else {
          return( character() )
        }
      }
    },

    read = function(x, n = 3L, no_token = 100L){
      read(self$topicmodel, as(self$bundle[[x]], self$type), no_topics = n, no_token = no_token)
    },

    as.zoo = function(x = NULL, y = NULL, k = 3L, exclude = TRUE, aggregation = c(NULL, "month", "quarter", "year")){
      if (is.null(y)){
        retval <- as.zoo(self$topicmodel, k = k, aggregation = aggregation)
        colnames(retval) <- self$labels[as.integer(colnames(retval))]
        
        if (!is.null(x)) retval <- retval[,x]
        if (exclude) retval <- retval[, -which(self$exclude == TRUE)]
        return(retval)
      } else {
        if (is.character(y)){
          labelPosition <- sapply(y, function(x) grep(x, self$labels))
          y <- as.integer(names(self$labels)[labelPosition])
        }
        z <- as.zoo(self$topicmodel, k = k, select = c(x, y), aggregation = aggregation)
        colnamesSplit <- strsplit(colnames(z), "-")
        colnames(z) <- sapply(
          colnamesSplit,
          function(x) paste(self$labels[x[1]], self$labels[x[2]], sep = " <-> ")
        )
        return(z)
      }
    },

    compare = function(...){
      # missing here: check that names exist and are unique
      ta_list <- list(self, ...)
      names(ta_list) <- sapply(ta_list, function(x) x$name)
      DTs <- lapply(
        names(ta_list),
        function(ta_name){
          message("... generating DT for ", ta_name)
          m <- topicmodels::posterior(ta_list[[ta_name]]$topicmodel)[["terms"]]
          rownames(m) <- paste(ta_name, ta_list[[ta_name]]$labels, sep = "::")
          m <- m[which(ta_list[[ta_name]]$exclude == FALSE), ]
          dt <- data.table(m)
          dt[, "label" := rownames(m)]
          y <- melt.data.table(data = dt, id.vars = "label")
          setnames(y, old = "variable", new = "feature")
          y
        }
      )
      message("... creating aggregated DT")
      DT <- rbindlist(DTs)
      message("... crosstabulation")
      DT_crosstab <- dcast.data.table(DT, feature ~ label, fun.aggregate = sum)
      features <- as.vector(DT_crosstab[["feature"]])
      DT_crosstab[, feature := NULL]
      merged <- as.matrix(DT_crosstab)
      rownames(merged) <- features
      message("... calculating cosine similarity")
      similarity <- lsa::cosine(merged)
      similarity
    },
    
    find_topics = function(x, n = 100, word2vec = NULL){
      if (is.null(self$terms)){
        self$terms <- terms(self$topicmodel, n)
      }
      if (!is.null(word2vec)){
        vocab <- unlist(lapply(x, function(x) as.vector(word2vec$similarity(x, 50)[,"word"])))
      } else {
        vocab <- x
      }
      y <- apply(
        self$terms, 2,
        function(column) sum(unlist(lapply(vocab, function(t) which(t == rev(column)))))
      )
      y <- y[order(y, decreasing = TRUE)]
      y <- round(y / (length(vocab) * n), 3L)
      df <- data.frame(
        topic = as.integer(gsub("^.*?\\s+(\\d+)$", "\\1", names(y))),
        score = unname(y)
      )
      df <- subset(df, score > 0)
      for (i in 1L:10L){
        df[[paste("word", i, sep = "_")]] <- self$terms[i, df$topic]
      }
      df
    }

  )
)
