#' TopicanalysisBundle
#' 
#' @importFrom reshape2 melt
#' @importFrom lsa cosine
#' @import data.table
#' @examples 
#' \dontrun{
#' B <- TopicanalysisBundle$new(topicmodels = TAs)
#' B$makeMatrix(big = T)
#' saveRDS(B$termTopicMatrix, file="/Users/blaette/Lab/repos/diffusion/matrix.RData")
#' B$termTopicMatrix <- readRDS("/Users/blaette/Lab/repos/diffusion/matrix.RData")
#' B$similarity <- readRDS("/Users/blaette/Lab/repos/diffusion/simil.RData")
#' B$doClustering()
#' B$getClusters(h=1)
#' }
#' @export TopicanalysisBundle
TopicanalysisBundle <- R6Class(
  
  "TopicanalysisBundle",
  
  public=list(
    
    topicmodels = list(),
    
    filenames = NULL,
    
    termTopicMatrix = NULL,
    
    simil = NULL,
    
    hclust = NULL,
    
    clusters = NULL,
    
    length = function() length(self$topicmodels),
    
    names = function() unname(sapply(self$topicmodels, function(x) x$name)),
    
    makeTermTopicMatrix = function(output = "matrix", minimize = FALSE, verbose = TRUE, ...){
      stopifnot(output %in% c("matrix", "simple_triplet_matrix", "big.matrix"))
      names(self$topicmodels) <- sapply(self$topicmodels, function(x) x$name)
      if (output == "matrix"){
        DTs <- lapply(
          names(self$topicmodels),
          function(id){
            if (verbose) message("... generating DT for ", id)
            M <- topicmodels::posterior(self$topicmodels[[id]]$topicmodel)[["terms"]]
            rownames(M) <- self$topicmodels[[id]]$labels
            M <- M[which(self$topicmodels[[id]]$exclude == FALSE), ]
            rownames(M) <- paste(id, rownames(M), sep="::")
            M2 <- reshape2::melt(M)
            colnames(M2) <- c("label", "feature", "value")
            data.table(M2)
          }
        )
        if (verbose) message("... creating aggregated DT")
        DT <- rbindlist(DTs)
        rm(DTs)
        gc()
        if (verbose) message("... crosstabulation")
        DT_crosstab <- dcast.data.table(DT, feature~label, fun.aggregate = sum)
        features <- as.vector(DT_crosstab[["feature"]])
        DT_crosstab[, feature := NULL]
        self$termTopicMatrix <- as.matrix(DT_crosstab)
        rownames(self$termTopicMatrix) <- features
        
      } else if (output %in% c("simple_triplet_matrix", "big.matrix")){
        
        if (verbose) message("... generate index and overall information")
        pb <- txtProgressBar(max = length(self$filenames), style = 3, width = getOption("width") - 8)
        info <- lapply(
          c(1:length(self$filenames)),
          function(i){
            setTxtProgressBar(pb, value=i)
            M <- readRDS(self$filenames[i])
            list(terms=M@terms, k=M@k)
          }
        )
        close(pb)
        names(info) <- names(self$filenames)
        
        if (verbose) message("... generating indices")
        uniqueTerms <- unique(unlist(lapply(info, function(x) x$terms)))
        termIndex <- setNames(1:length(uniqueTerms), uniqueTerms)
        
        labelList <- lapply(setNames(names(info), names(info)), function(id) paste(id, c(1:info[[id]]$k), sep="::"))
        labelVect <- unlist(labelList)
        labelIndex <- setNames(c(1:length(labelVect)), labelVect)
        
        
        if (output == "simple_triplet_matrix"){
          dfList <- lapply(
            names(self$filenames),
            function(id){
              message("... topicmodel: ", id)
              topicmodel <- readRDS(self$filenames[[id]])
              termTopicMatrix <- t(topicmodels::posterior(topicmodel)[["terms"]])
              rm(topicmodel)
              pb <- txtProgressBar(max = ncol(termTopicMatrix), style=3, width = getOption("width") - 10)
              dfList <- lapply(
                  c(1:ncol(termTopicMatrix)),
                  function(i){
                    setTxtProgressBar(pb, value = i)
                    column <- termTopicMatrix[,i]
                    if (minimize) column <- column[-which(column == min(column))]
                    data.frame(
                      i = unname(termIndex[names(column)]),
                      j = unname(labelIndex[paste(id, i, sep="::")]),
                      v = unname(column)
                      )
                  }
                )
              
              close(pb)
              do.call(rbind, dfList)
            }
            )
          self$termTopicMatrix <- slam::simple_triplet_matrix(
            i = unlist(lapply(dfList, function(x) x$i)),
            j = unlist(lapply(dfList, function(x) x$j)),
            v = unlist(lapply(dfList, function(x) x$v)),
            dimnames=list(names(termIndex), names(labelIndex))
          )
          
        } else if (output == "big.matrix"){
          self$termTopicMatrix <- bigmemory::big.matrix(
            nrow = length(uniqueTerms), ncol = length(labelVect),
            dimnames = list(uniqueTerms, labelVect)
          )
          dummy <- lapply(
            names(self$filenames),
            function(id){
              if (verbose) message("... topicmodel: ", id)
              topicmodel <- readRDS(self$filenames[[id]])
              termTopicMatrix <- t(posterior(topicmodel)[["terms"]])
              rm(topicmodel)
              gc()
              pb <- txtProgressBar(max = ncol(termTopicMatrix), style = 3, width = getOption("width") - 10)
              dummy <- lapply(
                c(1:ncol(termTopicMatrix)),
                function(i){
                  setTxtProgressBar(pb, value = i)
                  rowToAssign <- termTopicMatrix[,i]
                  self$termTopicMatrix[unname(termIndex[names(rowToAssign)]), unname(labelList[[id]][i])] <- rowToAssign
                  return(NULL)
                }
              )
            })
          
        }
        invisible(return(NULL))
      }
    },
    
    similarity = function(...){
      message("... calculating cosine similarity")
      self$simil <- polmineR.misc::similarity(self$termTopicMatrix...)
      invisible(self$simil)
    },
    
    doClustering = function(){
      self$hclust <- hclust(as.dist(proxy::pr_simil2dist(self$similarity)), method="ward.D")
    },
    
    plot = function(...){
      plot(as.dendrogram(self$hclust), ...)
    },
    
    getClusters = function(h, k=25){
      groups <- cutree(self$hclust, h=h)
      groupList <- split(x = names(groups), f = unname(groups))
      .getTerms <- function(topics, ...){
        M <- self$termTopicMatrix[,topics]
        apply(M, 2, function(column) rownames(M)[order(column, decreasing = TRUE)][1:k])
      }
      self$clusters <- blapply(x = groupList, f = .getTerms, progress=TRUE, mc=FALSE)
    },
    
    label = function(min = 0, max = 12){
      clusterLength <- sapply(self$clusters, ncol)
      toGet <- intersect(which(clusterLength >= min), which(clusterLength <= max))
      for (i in toGet){
        View(self$clusters[[i]])
        labelToAssign <- readline(prompt = ">>> ")
        if (labelToAssign %in% names(self$clusters)) message("label already exists")
        if (labelToAssign != "") names(self$clusters)[i] <- labelToAssign
      }
    },
    
    assignLabels = function(){
      labels <- names(self$clusters)
      labels <- labels[!is.na(labels)]
      labels <- labels[which(labels != "")]
      dummy <- lapply(
        labels,
        function(label){
          lapply(
            strsplit(colnames(self$clusters[[label]]), "::"),
            function(id) self$topicmodels[[id[1]]]$labels[as.integer(id[2])] <- label
          )
        })
    }
  )
)
