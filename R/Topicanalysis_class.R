#' Topicanalysis
#' 
#' Analyse topicmodels.
#' 
#' @section Methods:
#' \describe{
#' \item{\code{cooccurrences(k = 3, regex = NULL, docs = NULL, renumber = NULL, progress = TRUE, exclude = TRUE)}}{
#' Get cooccurrences of topics. Params are documented with the S4 cooccurrences-method for TopicModel-objects.
#' }
#' }

#' @examples
#' \dontrun{
#' library(polmineR.topics)
#' setwd("/Users/blaette/Lab/repos/arenen/analysis/policy_events_paper")
#' options(polmineR$meta=c("text_name", "text_party", "text_date")))
#' speeches <- readRDS("bundle/speeches.RData")
#' lda <- readRDS("lda/lda_bt_migration.RData")
#' TA <- Topicanalysis$new(topicmodel=lda, bundle=speeches)
#' TA$labels # see the result
#' saveRDS(TA$labels, file="lda_bt_migration_50_labels.RData")
#' TA$labels <- readRDS("lda_bt_migration_50_labels.RData")
#' 
#' TA$as.zoo(period="year")
#' TA$cooccurring()
#' }
#' @export Topicanalysis
#' @import R6
Topicanalysis <- R6Class(
  
  "Topicanalysis",
  
  public=list(

    topicmodel = NULL,
    bundle = NULL,
    labels = c(),
    name = c(),
    categories = c(),
    grouping = list(),
    exclude = c(),
    type = NULL,
    
    initialize = function (topicmodel, name = NULL, bundle=NULL, labels=NULL, categories=as.character(c()), exclude=NULL, type=NULL){
      self$topicmodel <- topicmodel
      self$bundle <- bundle
      self$type <- type
      self$name <- name
      self$categories <- categories
      if (is.null(labels)){
        self$labels <- as.character(c(1:topicmodel@k))
      } else {
        self$labels <- labels
      }
      if (is.null(labels)){
        self$exclude <- as.logical(rep(FALSE, times=topicmodel@k))  
      } else {
        self$exclude <- exclude 
      }
      
    },
    
    relabel = function(n, new){
      stopifnot(is.character(new))
      if (as.character(n) %in% names(self$labels)){
        self$labels[which(names(self$labels) == as.character(n))] <- new  
      } else {
        self$labels[n] <- new
      }
    },

    addCategory = function(new){
      stopifnot(is.character(new))
      if (new != ""){
        newCategories <- c(new, self$categories)
        self$categories <- newCategories[order(newCategories)]
      }
    },

    ignorance = function(n, new){
      stopifnot(is.numeric(n), is.logical(new))
      self$exclude[n] <- new
    },

    cooccurrences = function(k = 3, regex = NULL, docs = NULL, renumber = NULL, progress = TRUE, exclude = TRUE){
      if(is.null(renumber)){
        cooc <- cooccurrences(
          self$topicmodel, k = k, regex = regex, docs = docs,
          progress = progress
          )
        cooc[, x_label := self$labels[cooc[["x"]] ] ]
        cooc[, y_label := self$labels[cooc[["y"]] ] ]
        if (exclude == TRUE){
          cooc <- cooc[!cooc$x %in% which(self$exclude == TRUE),]
          cooc <- cooc[!cooc$y %in% which(self$exclude == TRUE),]
        }
      } else {
        stopifnot(is.integer(renumber))
        cooc <- cooccurrences(
          self$topicmodel, k = k, regex = regex, docs = docs, renumber = unname(renumber),
          progress = progress
          )
        if (exclude == TRUE){
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
      }
      
      cooc
    },

    wordcloud = function(x=1, n=50){
      tokens <- posterior(self$topicmodel)[["terms"]][x, ]
      tokens <- tokens[order(tokens, decreasing=TRUE)][c(1:n)]
      wordcloud::wordcloud(words=names(tokens), freq=unname(tokens), scale=c(4,.5))
    }
    ,

    docs = function(x, y=NULL, n=3, sAttributes=NULL){
      topicDf <- as.data.frame(topics(self$topicmodel, k=n))
      if (is.character(x)) x <- which(self$labels == x)
      xPresence <- sapply(topicDf, function(top) any(x %in% top))
      xPresent <- names(xPresence[unname(xPresence)])
      if (!is.null(y)){
        if (is.character(y)) y <- which(self$labels == y)
        yPresence <- sapply(topicDf, function(top) y %in% top)
        yPresent <- names(yPresence[unname(yPresence)])
        docs <- xPresent[xPresent %in% yPresent]
      } else {
        docs <- xPresent
      }
      docs
    },

    read = function(x, n=3, noToken=100){
      read(self$topicmodel, as(self$bundle[[x]], self$type), noTopics=n, noToken=100)
    },

    as.zoo = function(x = NULL, y = NULL, k = 3, exclude = TRUE, aggregation = "year"){
      if (is.null(y)){
        retval <- as.zoo(self$topicmodel, k = k, aggregation = aggregation)
        colnames(retval) <- self$labels[as.integer(colnames(retval))]
        
        if (!is.null(x)) retval <- retval[,x]
        if (exclude == TRUE){
          retval <- retval[, -which(self$exclude == TRUE)]
        }
        return(retval)
      } else {
        if (is.character(y)){
          labelPosition <- sapply(y, function(x) grep(x, self$labels))
          y <- as.integer(names(self$labels)[labelPosition])
        }
        zooObject <- as.zoo(self$topicmodel, k=k, select=y, aggregation=aggregation)
        colnamesSplit <- strsplit(colnames(zooObject), "-")
        colnames(zooObject) <- sapply(
          colnamesSplit,
          function(x) paste(self$labels[x[1]], self$labels[x[2]], sep=" <-> ")
        )

        return(zooObject)
      }
    },

    compare = function(x, ...){
      # missing here: check that names exist and are unique
      TA_list <- list(x, ...)
      names(TA_list) <- sapply(TA_list, function(x) x$name)
      DTs <- lapply(
        names(TA_list),
        function(taName){
          message("... generating DT for ", taName)
          m <- topicmodels::posterior(TA_list[[taName]]$topicmodel)[["terms"]]
          rownames(m) <- TA_list[[taName]]$labels
          m <- m[which(TA_list[[taName]]$exclude == FALSE), ]
          rownames(m) <- paste(taName, rownames(m), sep="::")
          mExt <- melt(m)
          colnames(mExt) <- c("label", "feature", "value")
          data.table(mExt)
        }
      )
      message("... creating aggregated DT")
      DT <- rbindlist(DTs)
      message("... crosstabulation")
      DT_crosstab <- dcast.data.table(DT, feature~label, fun.aggregate = sum)
      features <- as.vector(DT_crosstab[["feature"]])
      DT_crosstab[, feature := NULL]
      merged <- as.matrix(DT_crosstab)
      rownames(merged) <- features
      message("... calculating cosine similarity")
      similarity <- lsa::cosine(merged)
      similarity
    }

  )
)
