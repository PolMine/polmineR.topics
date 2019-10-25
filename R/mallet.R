#' Interface to mallet topicmodelling.
#' 
#' The envisaged workflow will be as follows (see example):
#' (1) Turn \code{partition_bundle}-object into mallet instance list, (2a) store the
#' resulting \code{jobjRef}-object locally using \code{java_object_store}, create mallet command
#' with \code{mallet_cmd}, and run mallet from the command line; alternativelly (2b) run
#' mallet topic modelling from within R.
#' 
#' @param terms_to_drop stopwords
#' @param ... further parameters
#' @param p_attribute The p_attribute to use, typically "word" or "lemma".
#' @param mc A \code{logical} value, whether to use multicore.
#' @param verbose A \code{logical} value, whether to be verbose.
#' @param filename Where to store the Java-object.
#' @importFrom utils read.csv read.table
#' @importFrom stats setNames
#' @importFrom slam simple_triplet_matrix
#' @importFrom parallel mclapply
#' @importFrom polmineR get_token_stream
#' @examples  
#' polmineR::use("polmineR")
#' speeches <- polmineR::as.speeches("GERMAPARLMINI", s_attribute_name = "speaker")
#' 
#' if (requireNamespace("rJava")){
#'   library(rJava)
#'   instance_list <- mallet_make_instance_list(speeches)
#' }
#' 
#' # Workflow 1: Run mallet from R using mallet package
#' 
#' if (requireNamespace("mallet")){
#'   engine <- mallet::MalletLDA(num.topics = 20)
#'   engine$loadDocuments(instance_list)
#'   engine$setAlphaOptimization(20, 50)
#'   engine$train(2000)
#' }
#' 
#' # Save instance list to disk and run mallet binary from command line
#' 
#' instancefile <- mallet_instance_list_store(instance_list, filename = tempfile()) # output to a tempfile
#' destfile <- tempfile()
#' cmd <- mallet_cmd(
#'   mallet_bin_dir = "/opt/mallet-2.0.8/bin",
#'   sourcefile = instancefile,
#'   threads = 1,
#'   destfile = destfile
#' )
#' system(cmd)
#' mallet_lda <- mallet_load_topicmodel(destfile)
#' topicmodels_lda <- as(mallet_lda, "LDA_Gibbs")
#' @rdname mallet
#' @importFrom polmineR get_token_stream
#' @export mallet_make_instance_list
mallet_make_instance_list <- function(x, p_attribute = "word", terms_to_drop = tm::stopwords("de"), mc = TRUE, verbose = TRUE){
  if (!requireNamespace(package = "rJava", quietly = TRUE)) stop("rJava package not available")
  if (!requireNamespace(package = "mallet", quietly = TRUE)) stop("mallet package not available")
  
  .fn <- function(subcorpus) get_token_stream(subcorpus, p_attribute = p_attribute, collapse = "\n")
  token_stream_list <- if (!mc) lapply(x@objects, .fn) else mclapply(x@objects, .fn)
  token_stream_vec <- unlist(token_stream_list)
  rm(token_stream_list)
  
  tmp_dir <- tempdir()
  stoplist_file <- file.path(tmp_dir, "stoplists.txt")
  cat(paste(terms_to_drop, collapse = "\n"), file = stoplist_file)
  
  if (verbose) message("... preparing mallet object")
  mallet::mallet.import(
    id.array = names(x), text.array = token_stream_vec,
    stoplist.file = stoplist_file, preserve.case = TRUE
  )
}

#' @details The \code{mallet_cmd} function will return a command to be executed
#'   in a terminal that will prepare a topic model with the input parameters.
#' @param x An object to process (HaHaHa).
#' @param model A topic model prepared by mallet.
#' @param mallet_bin_dir The directory where the executable 'mallet' resides.
#' @param sourcefile File to which an instance list has been saved.
#' @param destfile Directory where results will be saved.
#' @param topwords ...
#' @param topics The number of topics the topic model shall have.
#' @param iterations Number of iterations to perform.
#' @param threads Number of threads to use.
#' @export mallet_cmd
#' @rdname mallet
mallet_cmd <- function(mallet_bin_dir, sourcefile, destfile, topwords = 50, topics = 50, iterations = 2000, threads = 1){
  stopifnot(
    is.numeric(topics), topics > 2,
    is.numeric(iterations), iterations > 1,
    is.numeric(topwords), topwords > 2,
    is.numeric(threads), threads > 0
  )
  
  cmd <- c(
    file.path(mallet_bin_dir, "mallet"), "train-topics",
    "--input", sourcefile,
    "--num-topics", topics,
    "--num-iterations", iterations,
    "--output-model", destfile,
     "--num-threads", threads
    )
  paste(cmd, collapse = " ")
}


# setOldClass("jobjRef")

#' @export
setAs(from = "jobjRef", to = "LDA_Gibbs", function(from){
  new(
    "LDA_Gibbs",
    Dim = c(
      from$data$size(), # Number of documents
      from$getAlphabet()$size() # Number of terms
    ),
    k = from$getNumTopics(),
    terms = strsplit(from$getAlphabet()$toString(), "\n")[[1]],
    documents = sapply(0L:(from$data$size() - 1L), function(i) from$data$get(i)$instance$getName()), # Vector containing the document names
    beta = t(as.matrix(mallet_get_word_weights(from))), # A matrix; logarithmized parameters of the word distribution for each topic
    gamma = do.call(rbind, lapply(0L:(from$data$size() - 1L), function(i) from$getTopicProbabilities(i))), # matrix, parameters of the posterior topic distribution for each document
    iter = from$numIterations
  )
})



#' @export mallet_get_word_weights
#' @rdname mallet
mallet_get_word_weights <- function(model){
  if (!requireNamespace("rJava", quietly = TRUE)){
    stop("Package 'rJava' required, but not available.")
  }
  destfile <- tempfile()
  # file <- new(rJava::J("java/io/File"), destfile)
  file <- rJava::.jnew("java/io/File", destfile)
  # file_writer <- new(rJava::J("java/io/FileWriter"), file)
  file_writer <- rJava::.jnew("java/io/FileWriter", file)
  # print_writer <- new(rJava::J("java/io/PrintWriter"), file_writer)
  print_writer <- rJava::new(rJava::J("java/io/PrintWriter"), file_writer)
  model$printTopicWordWeights(print_writer)
  print_writer$close()
  mallet_load_word_weights(destfile)
}


#' @export mallet_load_word_weights
#' @importFrom slam simple_triplet_matrix
#' @rdname mallet
mallet_load_word_weights <- function(filename){
  word_weights_raw <- read.table(filename, sep = "\t")
  # word_weights_raw <- subset(word_weights_raw, word_weights_raw[["V3"]] > min(word_weights_raw[,3]))
  simple_triplet_matrix(
    i = as.integer(word_weights_raw[,2]),
    j = word_weights_raw[,1] + 1L,
    v = word_weights_raw[,3],
    dimnames = list(
      levels(word_weights_raw[,2]),
      as.character(1L:(max(word_weights_raw[,1]) + 1L))
    )
  )
}



#' @rdname mallet
#' @export mallet_instance_list_store
mallet_instance_list_store <- function(x, filename = tempfile()){
  # This snippet is inspired an unexported function save.mallet.instances in 
  # v1.2.0 of the R mallet package which has not yet been released at CRAN.
  # See: https://github.com/mimno/RMallet/blob/master/mallet/R/mallet.R
  x$save(rJava::.jnew("java/io/File", filename))
  filename
}

#' @rdname mallet
#' @export mallet_load_topicmodel
mallet_load_topicmodel <- function(filename){
  rJava::J("cc/mallet/topics/ParallelTopicModel")$read(rJava::.jnew("java/io/File", filename))
}
