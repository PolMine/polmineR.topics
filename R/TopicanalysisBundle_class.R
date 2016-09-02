#' TopicanalysisBundle
#' 
#' @importFrom reshape2 melt
#' @importFrom lsa cosine
#' @import data.table
#' @field foo something
#' @examples 
#' \dontrun{
#' source("/Users/blaette/Lab/repos/diffusion/code/01_global_variables.R")
#' B <- TopicanalysisBundle$new()
#' B$filenames <- filenames
#' B$getTerms()
#' B$dist <- readRDS("/Users/blaette/Lab/repos/diffusion/data/n_topics_250/dist_binary_take2.RData")
#' colnames(B$dist) <- colnames(B$terms)
#' rownames(B$dist) <- colnames(B$terms)
#' B$dist <- as.dist(B$dist)
#' B$hclust <- hclust(B$dist, method="ward.D")
#' B$hclust$height <- B$hclust$height[order(B$hclust$height)]
#' png("/Users/blaette/Lab/tmp/dendrogram.png", width = 30000, height = 30000)
#' plot(as.dendrogram(B$hclust), horiz = T, cex = 0.01)
#' dev.off()
#' 
#' B$getClusters(h = 1.2)
#' names(B$clusters)[as.integer(names(B$query("Integration")[1]))] <- "Integration"
#' 
#' B$restore()
#' B$assignLabels()
#' 
#' }
#' @export TopicanalysisBundle
TopicanalysisBundle <- R6Class(
  
  "TopicanalysisBundle",
  
  public=list(
    
    topicmodels = list(),
    
    filenames = NULL,
    
    terms = NULL,
    
    termTopicMatrix = NULL,
    
    simil = NULL,
    
    dist = NULL,
    
    hclust = NULL,
    
    clusters = NULL,
    
    length = function() length(self$topicmodels),
    
    names = function() unname(sapply(self$topicmodels, function(x) x$name)),
    
    restore = function(x=NULL, verbose = TRUE){
      "restore it"
      if (is.null(x)) x <- names(self$filenames)
      B$topicmodels <- lapply(
        setNames(x, x),
        function(what){
          if (file.exists(self$filenames[[what]]["lda"])){
            if (verbose) message("... restoring: ", what)
            TA <- Topicanalysis$new(
              topicmodel = readRDS(self$filenames[[what]]["lda"])
            )
          } else {
            if (verbose) warning("restoring ", what, " failed - topicmodel file not available")
          }
          if (file.exists(self$filenames[[what]]["labels"])){
            TA$labels = readRDS(self$filenames[[what]]["labels"])
            if (verbose) message("[labels: OK]")
          } else {
            if (verbose) message("[labels: NOT AVAILABLE]")
          }
          if (file.exists(self$filenames[[what]]["exclude"])){
            TA$exclude = readRDS(self$filenames[[what]]["exclude"])
            if (verbose) message("[exclude: OK]")
          } else {
            if (verbose) message("[exclude: NOT AVAILABLE]")
          }
          TA
        }
      )
    },
    
    store = function(verbose = TRUE){
      lapply(
        names(self$topicmodels),
        function(topicmodel_name){
          if ("labels" %in% names(self$filenames[[topicmodel_name]])){
            saveRDS(self$topicmodels[[topicmodel_name]]$labels, file=self$filenames[[topicmodel_name]]["labels"])
          } else {
            message("labels not saved for, ", topicmodel_name, " (no filename available)")
          }
          if ("exclude" %in% names(self$filenames[[topicmodel_name]])){
            saveRDS(self$topicmodels[[topicmodel_name]]$exclude, file=self$filenames[[topicmodel_name]]["exclude"])
          } else {
            message("exclude vector not saved for, ", topicmodel_name, " (no filename available)")
          }
        }
      )
    },
    
    getTerms = function(k = 100, verbose = TRUE){
      Ms <- lapply(
        names(self$filenames),
        function(id){
          if (verbose) message("... loading and processing topicmodel: ", id)
          topicmodel <- readRDS(file = self$filenames[[id]]["lda"])
          M <- topicmodels::terms(topicmodel, k = k)
          colnames(M) <- paste(id, c(1:ncol(M)), sep="::")
          M
        }
      )
      if (verbose) message("... putting together simple_triplet_matrix")
      M <- do.call(cbind, Ms)
      topicsVector <- unlist(lapply(Ms, colnames))
      rm(Ms)
      topicsIndex <- setNames(c(1:length(topicsVector)), topicsVector)
      uniqueTerms <- unique(as.vector(M))
      termIndex <- setNames(c(1:length(uniqueTerms)), uniqueTerms)
      self$terms <- slam::simple_triplet_matrix(
        i = unname(termIndex[as.vector(M)]),
        j = rep(1:ncol(M), each = nrow(M)),
        v = as.vector(apply(M, 2, function(x) termIndex[x])),
        ncol = length(topicsIndex),
        nrow = length(termIndex),
        dimnames=list(names(termIndex), names(topicsIndex))
      )
      invisible(self$terms)
    },
    
    
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
            M <- readRDS(self$filenames[[i]])
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
      self$simil <- polmineR.misc::similarity(self$termTopicMatrix, ...)
      invisible(self$simil)
    },
    
    distance = function(chunks = 1, method="binary", mc = FALSE, progress = TRUE, verbose = TRUE){
      if (is.null(self$terms) || class(self$terms) != "simple_triplet_matrix"){
        stop("object of class 'simple_triplet_matrix' needs to be available in field terms")
      }
      if (chunks == 1){
        self$dist <- proxy::dist(as.matrix(self$terms), method = method, by_rows = FALSE)
      } else {
        self$dist <- blapply(as.matrix(self$terms), f = proxy::dist, method = method, chunks = chunks, by_rows = F, mc = mc, progess = progress, verbose = FALSE)
      }
      invisible(self$dist)
    },
    
    doClustering = function(){
      self$hclust <- hclust(as.dist(proxy::pr_simil2dist(self$simil)), method="ward.D")
    },
    
    plot = function(...){
      plot(as.dendrogram(self$hclust), ...)
    },
    
    getClusters = function(h, k=25){
      groups <- cutree(self$hclust, h=h)
      clusterList <- split(x = names(groups), f = unname(groups))
      M <- lapply(split(self$terms$v, self$terms$j), function(x) self$terms$dimnames[[1]][x])
      M <- do.call(cbind, M)
      colnames(M) <- colnames(self$terms)
      # .getTerms <- function(topics, ...){
      #   select <- self$terms[, topics]
      #   cols <- lapply(split(select$v, select$j), function(x) self$terms$dimnames[[1]][x])
      #   do.call(cbind, cols)
      # }
      self$clusters <- lapply(clusterList, function(x) M[,x])
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
    },
    
    query = function(query){
      noHits <- sapply(
        setNames(self$clusters, c(1:length(self$clusters))),
        function(x) sum(table(as.vector(x))[query], na.rm = TRUE)
      )
      if (length(which(noHits == 0))) noHits <- noHits[-which(noHits == 0)]
      noHits[order(noHits, decreasing = TRUE)]
      
    },
    
    as.zoo = function(x, y=NULL, k = 3, aggregation = "year"){
      ts_list <- lapply(
        setNames(names(self$topicmodels), names(self$topicmodels)),
        function(id) {
          if (x %in% self$topicmodels[[id]]$labels){
            message("... getting time series data for: ", id)
            count <- self$topicmodels[[id]]$as.zoo(x = label, y = y, aggregation = aggregation)  
            DT <- data.table(date = index(count), as.data.frame(count))
            DT[, "id" := id, with = TRUE]
            return(DT)
          } else {
            return(NULL)
          }
          
        }
      )
      ts <- rbindlist(ts_list)
      tab <- dcast.data.table(ts, date~id, value.var = "count", fun.aggregate = sum)
      dates <- tab[["date"]]
      tab[, "date" := NULL, with = TRUE]
      zooObject <- zoo(x=tab, order.by = dates)
    }
  )
)