#' TopicanalysisBundle
#' 
#' @importFrom lsa cosine
#' @import data.table
#' @field foo something
#' @export TopicanalysisBundle
TopicanalysisBundle <- R6Class(
  
  "TopicanalysisBundle",
  
  public = list(
    
    # fields 
    
    topicmodels = list(),
    filenames = NULL,
    terms = NULL,
    termTopicMatrix = NULL,
    simil = NULL,
    dist = NULL,
    distMatrix = NULL,
    hclust = NULL,
    topicClusters = NULL,
    categories = NULL, # a data.frame
    
    
    # methods
    
    length = function() length(self$topicmodels),
    
    names = function() unname(sapply(self$topicmodels, function(x) x$name)),
    
    restore = function(x = NULL, verbose = TRUE){
      
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
          topicmodel <- readRDS(file = self$filenames[[id]])
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
    
    
    makeTermTopicMatrix = function(output = "matrix", minimize = FALSE, verbose = TRUE, progress = TRUE, mc = FALSE, ...){
      stopifnot(output %in% c("matrix", "simple_triplet_matrix", "big.matrix"))
      names(self$topicmodels) <- sapply(self$topicmodels, function(x) x$name)
      if (output == "matrix"){
        DTs <- lapply(
          names(self$topicmodels),
          function(id){
            if (verbose) message("... generating DT for ", ta_name)
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
        info <- blapply(
          1L:length(self$filenames),
          function(i, filenames, ...){
            M <- readRDS(filenames[[i]])
            list(terms=M@terms, k=M@k)
          },
          filenames = self$filenames,
          mc = mc, verbose = FALSE, progress = progress
        )
        names(info) <- names(self$filenames)
        
        if (verbose) message("... generating indices")
        uniqueTerms <- unique(unlist(lapply(info, function(x) x$terms)))
        termIndex <- setNames(1:length(uniqueTerms), uniqueTerms)
        
        labelList <- lapply(setNames(names(info), names(info)), function(id) paste(id, c(1:info[[id]]$k), sep="::"))
        labelVect <- unlist(labelList)
        labelIndex <- setNames(c(1:length(labelVect)), labelVect)
        
        
        if (output == "simple_triplet_matrix"){
          
          .getTermTopics <- function(id, filenames, ...){
            if (mc) progress <- FALSE
            message("... topicmodel: ", id)
            topicmodel <- readRDS(filenames[[id]])
            termTopicMatrix <- t(topicmodels::posterior(topicmodel)[["terms"]])
            rm(topicmodel)
            if (progress) pb <- txtProgressBar(max = ncol(termTopicMatrix), style=3, width = getOption("width") - 10)
            dfList <- lapply(
              1L:ncol(termTopicMatrix),
              function(i){
                if (progress) setTxtProgressBar(pb, value = i)
                column <- termTopicMatrix[,i]
                if (minimize) column <- column[-which(column == min(column))]
                data.frame(
                  i = unname(termIndex[names(column)]),
                  j = unname(labelIndex[paste(id, i, sep="::")]),
                  v = unname(column)
                )
              }
            )
            
            if (progress) close(pb)
            do.call(rbind, dfList)
          }
          dfList <- blapply(
            names(self$filenames),
            f = .getTermTopics,
            filenames = self$filenames,
            mc = mc, progress = progress, verbose = FALSE
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
    
    getTopicSimilarity = function(...){
      message("... calculating cosine similarity")
      self$simil <- polmineR.misc::similarity(self$termTopicMatrix, ...)
      invisible(self$simil)
    },
    
    getTopicDistance = function(chunks = 1, method = "binary", mc = FALSE, progress = TRUE, verbose = TRUE){
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
    
    getTopicClusters = function(h, k = 25){
      
      "Get clusters of topics that are similar because they share terms. The method
      will fill the field 'clusters' of the class with a list of matrices (rows are
      the top terms indicating a topic."
      
      stopifnot(is(self$hclust) == "hclust")
      if (is.null(self$terms)) stop("slot terms is missing")
      groups <- cutree(self$hclust, h = h)
      clusterList <- split(x = names(groups), f = unname(groups))
      M <- lapply(split(self$terms$v, self$terms$j), function(x) self$terms$dimnames[[1]][x])
      M <- do.call(cbind, M)
      colnames(M) <- colnames(self$terms)
      self$topicClusters <- lapply(clusterList, function(x) M[,x])
      names(self$topicClusters) <- paste("(", names(self$topicClusters), ")", sep = "")
    },
    
    makeOverviewTopicClusters = function(n = 10, terms = TRUE){
      
      "Based on the topic clusters in the 'topicClusters'-field, the method will return
      a data.frame with the topic number, the topic label and the n most frequent words
      in the matrix."
      
      DF <- data.frame(
        cluster_no = 1:length(self$topicClusters),
        no_topics = sapply(self$topicClusters, ncol),
        label = names(self$topicClusters)
      )
      if (terms){
        topTermsList <- lapply(
          self$topicClusters,
          function(x){
            y <- table(as.vector(x))
            y <- setNames(as.vector(y), names(y))
            y <- y[order(y, decreasing = TRUE)]
            names(y)[1:n]
          })
        topTermsDF <- as.data.frame(do.call(rbind, topTermsList))
        colnames(topTermsDF) <- paste("rank", c(1:ncol(topTermsDF)), sep = "_")
        DF <- data.frame(DF, topTermsDF)
      }
      rownames(DF) <- as.character(c(1:nrow(DF)))
      return(DF)
      
    },
    
    plotClusterDendrogram = function(topicNo, verbose = TRUE){
      topics <- colnames(self$topicClusters[[topicNo]])
      distMatrix <- as.matrix(self$dist)
      distMatrixSubset <- distMatrix[topics, topics]
      distObjectSubset <- as.dist(distMatrixSubset)
      H <- hclust(distObjectSubset, method = "ward.D")
      D <- as.dendrogram(H)
      plot(D, horiz = T, cex = 0.01)
    },
    
    plotHeatmap = function(i){
      topicIds <- colnames(self$topicClusters[[i]])
      distMatrixSubset <- distMatrix[topicIds, topicIds]
      distObjectSubset <- as.dist(distMatrixSubset)
      H <- hclust(distObjectSubset, method = "ward.D")
      D <- as.dendrogram(H)
      heatmap.2(distMatrixSubset, Rowv = D, Colv = "Rowv")
    },
    
    labelTopicClusters = function(min = 0, max = 25, heatmap = FALSE){
      
      "Skip through topicClusters and assign labels to the clusters."
      
      clusterLength <- sapply(self$topicClusters, ncol)
      toGet <- intersect(which(clusterLength >= min), which(clusterLength <= max))
      i <- 1
      while (TRUE){
        if (i > length(self$topicClusters)) break
        # generate output: cluster no and nrow of the matrix
        cat(
          "Showing terms for cluster ", i, "/", length(self$topicClusters),
          " (", ncol(self$topicClusters[[i]]), " topics)\n",
          sep=""
        )
        cat("label assigned: ", names(self$topicClusters)[i], "\n")
        View(self$topicClusters[[i]], title = names(self$topicClusters)[i])
        
        if (heatmap == TRUE) self$plotHeatmap(i = i)
        
        if (is.data.frame(self$categories)){
          toAssign <- polmineR.misc::selectLabel(x = self$categories)
        } else {
          toAssign <- readline(prompt = ">> ")
        }
        
        if (toAssign == ""){
          whatToDo <- select.list(choices = c("retry", "next", "previous", "overview", "help", "continue", "ambigious", "freehand", "exit"))
          if (whatToDo == "retry"){
            dummy <- 1
          } else if (whatToDo == "next"){
            i <- i + 1
          } else if (whatToDo == "previous"){
            i <- i - 1
          } else if (whatToDo == "exit"){
            break
          } else if (whatToDo == "overview"){ 
            View(
              x = self$makeOverviewTopicClusters(),
              title = "topic overview"
            )
            readline(prompt = "Hit a key to continue ...")
          } else if (whatToDo == "continue"){
            labelled <- which(grepl("^\\d+$", names(self$topicClusters)) == FALSE)
            if (length(labelled > 0)) i <- max(labelled) + 1
          } else if (whatToDo == "freehand"){
            names(self$topicClusters)[i] <- readline(prompt = "freehand >>> ")
            i <- i + 1
          } else if (whatToDo == "ambigious"){
            names(self$topicClusters)[i] <- "()"
            i <- i + 1
          }
        } else {
          if (toAssign %in% names(self$topicClusters)) {
            message("label already exists, assign anyway?")
            whatToDo <- select.list(choices = c("yes", "no"))
            if (whatToDo == "yes"){
              message("assigning label: ", toAssign)
              names(self$topicClusters)[i] <- toAssign
              i <- i + 1
            }
          } else {
            message("assigning label: ", toAssign)
            names(self$topicClusters)[i] <- toAssign
            i <- i + 1
          }
          
        }
      }
    },
    
    assignLabels = function(){
      labels <- names(self$topicClusters)
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