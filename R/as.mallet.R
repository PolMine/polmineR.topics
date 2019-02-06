#' Interface to mallet topicmodelling.
#' 
#' Turn partitionBundle into a mallet object (instance list). The typical 
#' workflow will be as follows (see example):
#' (1) Turn partitionBundle-object into mallet instance list, (2a) store the
#' resulting jobjRef-object locally using store-method, create mallet command
#' with malletCmd, and run mallet from the command line; alternativelly (2b) run
#' mallet topic modelling from within R.
#' 
#' @param .Object \code{partition}, \code{partition_bundle}, or \code{rJava} object
#' @param termsToDrop stopwords
#' @param ... further parameters
#' @param pAttribute the pAttribute to use, typically "word" or "lemma"
#' @param mc whether to use multicore
#' @param verbose logical, whether to be verbose
#' @param filename where to store the Java-object
#' @exportMethod as.mallet
#' @importFrom utils read.csv read.table
#' @importFrom stats setNames
#' @importFrom slam simple_triplet_matrix
#' @importFrom parallel mclapply
#' @importFrom polmineR get_token_stream
#' @examples 
#' \dontrun{
#' use("polmineR.sampleCorpus")
#' bt2009 <- partition("PLPRBTTXT", text_year="2009")
#' bt2009bundle <- partitionBundle(bt2009, sAttribute= "text_protocol_no", pAttribute = NULL)
#' instanceList <- as.mallet(bt2009bundle)
#' topic.model <- MalletLDA(num.topics = 20)
#' topic.model$loadDocuments(instanceList)
#' topic.model$setAlphaOptimization(20, 50)
#' topic.model$train(2000)
#' 
#' fname <- store(instanceList, filename = NULL) # output to a tempfile
#' system(malletCmd(fname))
#' }
#' @rdname mallet
setGeneric("as.mallet", function(.Object, ...) standardGeneric("as.mallet"))

#' @rdname mallet
#' @importFrom polmineR getTokenStream
setMethod("as.mallet", "partition_bundle", function(.Object, pAttribute = "word", termsToDrop = tm::stopwords("de"), mc = TRUE, verbose = TRUE){
  if (!requireNamespace(package = "rJava", quietly = TRUE)){
    stop("rJava package not available")
  }
  
  if (!mc){
    if (verbose == TRUE) message("... reconstructing token stream (mc=FALSE)")
    tokenStream <- lapply(
      .Object@objects,
      function(x) get_token_stream(x, p_attribute = pAttribute, collapse = "\n")
      )
  } else if (mc){
    if (verbose) message("... reconstructing token stream (mc = TRUE)")
    tokenStream <- mclapply(
      .Object@objects,
      function(x) get_token_stream(x, p_attribute = pAttribute, collapse = "\n")
      )
  }
  tokenStreamVector <- unlist(tokenStream)
  
  tmpDir <- tempdir()
  stoplistFile <- file.path(tmpDir, "stoplists.txt")
  cat(paste(termsToDrop, collapse = "\n"), file = stoplistFile)
  
  if (verbose) message("... make mallet object")
  malletObject <- mallet::mallet.import(
    id.array = names(.Object), text.array = tokenStreamVector,
    stoplist.file = stoplistFile, preserve.case = TRUE
  )
  return(malletObject)
})

#' @param sourcefile ...
#' @param targetDir ...
#' @param topwords ...
#' @param topics ...
#' @param iterations ...
#' @param threads ...
#' @param dir ...
#' @export malletCmd
#' @rdname mallet
malletCmd <- function(sourcefile, targetDir = "/Users/blaette/Lab/tmp/mallet_result", topwords = 50, topics = 50, iterations = 2000, threads = 1){
  stopifnot(
    is.numeric(topics), topics > 2,
    is.numeric(iterations), iterations > 1,
    is.numeric(topwords), topwords > 2,
    is.numeric(threads), threads > 0
  )
  
  # check whether targetDir exists
  if (!file.exists(targetDir)){
    dir.create(targetDir)
    message("... creating targetDir")
  } else {
    if (file.info(targetDir)[,"isdir"] == FALSE){
      stop("targetDir is not a directore, but an existing file")
    }
  }

  cmd <- c(
    "/opt/mallet/bin/mallet", "train-topics",
    "--input", sourcefile,
    "--num-topics", topics,
    "--num-iterations", iterations,
    "--output-state", file.path(targetDir, "state.gz"),
    "--output-doc-topics", file.path(targetDir, "doc-topics.csv"),
    "--output-topic-keys", file.path(targetDir, "topics-keys.csv"),
    "--num-top-words", topwords,
    "--num-threads", threads,
    "--xml-topic-report", file.path(targetDir, "xmlTopicReport.xml"),
    "--topic-word-weights-file", file.path(targetDir, "wordWeights.csv"),
    "--xml-topic-phrase-report", file.path(targetDir, "topicPhraseReport.xml")
    )
  paste(cmd, collapse=" ")
}

#' @import xml2
#' @rdname mallet
malletImport <- function(dir = "/Users/blaette/Lab/tmp/mallet_result"){
  
  retval <- list()
  
  message("... importing doc-topics.csv")
  docsTopics <- read.csv(file = file.path(dir, "doc-topics.csv"), sep = "\t", header = FALSE)
  rownames(docsTopics) <- as.character(c(1:nrow(docsTopics)))
  docsTopicsMatrix <- as.matrix(docsTopics[, c(3:ncol(docsTopics))])
  colnames(docsTopicsMatrix) <- as.character(c(1:ncol(docsTopicsMatrix)))
  retval[["docsTopicsMatrix"]] <- docsTopicsMatrix
  
  # gzConnection <- gzfile(file.path(dir, "state.gz"))
  # foo <- read.csv(gzConnection)
  
  message("... importing topics-keys.csv")
  topicKeysRaw <- read.csv(
    file = file.path(dir, "topics-keys.csv"), sep="\t", header=FALSE,
    stringsAsFactors=FALSE
    )
  topicKeysMatrix <- as.matrix(data.frame(strsplit(topicKeysRaw[[3]], "\\s")))
  colnames(topicKeysMatrix) <- NULL
  retval[["topicKeysMatrix"]] <- topicKeysMatrix
  
  
  message("... importing word-weights.csv")
  
  wordWeightsRaw <- read.table(file.path(dir, "wordWeights.csv"), sep = "\t")
  wordsUnique <- unique(wordWeightsRaw[,2])
  wordIndex <- setNames(1:length(wordsUnique), wordsUnique)
  wordWeightsRaw[,2] <- wordIndex[wordWeightsRaw[,2]]
  wordWeightsRaw <- subset(wordWeightsRaw, wordWeightsRaw[["V3"]] > min(wordWeightsRaw[,3]))
  wordWeightsSlam <- simple_triplet_matrix(
    i = wordWeightsRaw[,2],
    j = wordWeightsRaw[,1] + 1,
    v = wordWeightsRaw[,3],
    dimnames = list(
      names(wordIndex),
      as.character(1:(max(wordWeightsRaw[,1]) + 1))
      )
  )
  retval[["wordWeights"]] <- wordWeightsSlam
  
  
  message("... importing topicPhraseReport.xml")
  topicPhraseXmlDoc <- read_xml(file.path(dir, "topicPhraseReport.xml"))
  topicNodes <- xml_find_all(topicPhraseXmlDoc, "/topics/topic")
  topicPhraseReportDf <- do.call(rbind, lapply(
    topicNodes,
    function(x){
      do.call(rbind, lapply(
        c("word", "phrase"),
        function(what){
          childrenToGet <- xml_find_all(x, paste("./", what, sep=""))
          cbind(
            topic=rep(xml_attrs(x)["id"], times=length(childrenToGet)),
            what=rep(what, times=length(childrenToGet)),
            token=sapply(childrenToGet, xml_text),
            do.call(rbind, lapply(childrenToGet, xml_attrs))
          )
        }
      ))
    }
  ))
  topicPhraseReportDT <- as.data.table(topicPhraseReportDf)
  
  retval[["topicPhraseReportDT"]] <- topicPhraseReportDT
  retval
  
}


setOldClass(Classes = "jobjRef")

#' @rdname mallet
#' @importMethodsFrom polmineR store
setMethod("store", "jobjRef", function(.Object, filename = tempfile()){
  if (!requireNamespace(package = "rJava", quietly = TRUE)){
    stop("rJava package not available")
  }
  fileOutputStream <- new(rJava::J("java/io/FileOutputStream"), filename)
  objectStream <- new(rJava::J("java/io/ObjectOutputStream"), fileOutputStream)
  objectStream$writeObject(.Object)
  objectStream$close()
  filename
})

