#' Interface to mallet topicmodelling.
#' 
#' Turn \code{partition_bundle} into a mallet object (instance list). The typical 
#' workflow will be as follows (see example):
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
#' destdir <- tempdir()
#' cmd <- mallet_cmd(
#'   mallet_bin_dir = "/opt/mallet-2.0.8/bin",
#'   sourcefile = instancefile,
#'   threads = 1,
#'   target_dir = destdir
#' )
#' system(cmd)
#' mallet_lda <- mallet_topicmodel_load(file.path(destdir, "model.bin"))
#' y <- mallet_as_LDA_Gibbs(mallet_lda, modeldir = destdir)
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
#' @param modeldir The director where a model computed by mallet is stored.
#' @param mallet_bin_dir The directory where the executable 'mallet' resides.
#' @param sourcefile File to which an instance list has been saved.
#' @param target_dir Directory where results will be saved.
#' @param topwords ...
#' @param topics The number of topics the topic model shall have.
#' @param iterations Number of iterations to perform.
#' @param threads Number of threads to use.
#' @export mallet_cmd
#' @rdname mallet
mallet_cmd <- function(mallet_bin_dir, sourcefile, target_dir, topwords = 50, topics = 50, iterations = 2000, threads = 1){
  stopifnot(
    is.numeric(topics), topics > 2,
    is.numeric(iterations), iterations > 1,
    is.numeric(topwords), topwords > 2,
    is.numeric(threads), threads > 0
  )
  
  # check whether target_dir exists
  if (!file.exists(target_dir)){
    dir.create(target_dir)
    message("... creating target_dir")
  } else {
    if (file.info(target_dir)[,"isdir"] == FALSE){
      stop("target_dir is not a directory!")
    }
  }

  cmd <- c(
    file.path(mallet_bin_dir, "mallet"), "train-topics",
    "--input", sourcefile,
    "--num-topics", topics,
    "--num-iterations", iterations,
    "--output-model", file.path(target_dir, "model.bin"),
    "--output-state", file.path(target_dir, "state.gz"),
    "--output-doc-topics", file.path(target_dir, "doc-topics.csv"),
    "--output-topic-keys", file.path(target_dir, "topics-keys.csv"),
    "--num-top-words", topwords,
    "--num-threads", threads,
    "--xml-topic-report", file.path(target_dir, "xmlTopicReport.xml"),
    "--topic-word-weights-file", file.path(target_dir, "wordWeights.csv"),
    "--xml-topic-phrase-report", file.path(target_dir, "topicPhraseReport.xml")
    )
  paste(cmd, collapse = " ")
}


#' @rdname mallet
#' @export mallet_as_LDA_Gibbs
mallet_as_LDA_Gibbs <- function(x, modeldir){
  new(
    "LDA_Gibbs",
    Dim = c(
      x$data$size(), # number of documents
      x$getAlphabet()$size() # number of terms
    ),
    # control = new("TopicModelcontrol"),
    k = x$getNumTopics(),
    terms = strsplit(x$getAlphabet()$toString(), "\n")[[1]],
    documents = sapply(0L:(x$data$size() - 1L), function(i) x$data$get(i)$instance$getName()), # Vector containing the document names
    beta = t(as.matrix(mallet_load_word_weights(file.path(modeldir, "wordWeights.csv")))), # matrix; logarithmized parameters of the word distribution for each topic
    gamma = do.call(rbind, lapply(0L:(x$data$size() - 1L), function(i) x$getTopicProbabilities(i))), # matrix, parameters of the posterior topic distribution for each document
    iter = x$numIterations
  )
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


#' @import xml2
#' @rdname mallet
#' 
mallet_load_results <- function(modeldir){
  
  retval <- list()
  
  message("... importing doc-topics.csv")
  docs_topics <- read.csv(file = file.path(modeldir, "doc-topics.csv"), sep = "\t", header = FALSE)
  rownames(docs_topics) <- as.character(1L:nrow(docs_topics))
  docs_topics_matrix <- as.matrix(docs_topics[, 3L:ncol(docs_topics)])
  colnames(docs_topics_matrix) <- as.character(1L:ncol(docs_topics_matrix))
  retval[["docs_topics_matrix"]] <- docs_topics_matrix
  
  message("... importing topics-keys.csv")
  topic_keys_raw <- read.csv(
    file = file.path(modeldir, "topics-keys.csv"), sep = "\t", header = FALSE,
    stringsAsFactors = FALSE
    )
  topic_keys_matrix <- as.matrix(data.frame(strsplit(topic_keys_raw[[3]], "\\s")))
  colnames(topic_keys_matrix) <- NULL
  retval[["topic_keys_matrix"]] <- topic_keys_matrix
  
  message("... importing word-weights.csv")
  retval[["word_weights"]] <- mallet_load_word_weights(file.path(modeldir, "wordWeights.csv"))

  message("... importing topicPhraseReport.xml")
  topic_phrase_xml_file <- read_xml(file.path(modeldir, "topicPhraseReport.xml"))
  topic_nodes <- xml_find_all(topic_phrase_xml_file, "/topics/topic")
  topic_phrase_report_df <- do.call(rbind, lapply(
    topic_nodes,
    function(x){
      do.call(rbind, lapply(
        c("word", "phrase"),
        function(what){
          children <- xml_find_all(x, paste("./", what, sep = ""))
          cbind(
            topic = rep(xml_attrs(x)["id"], times=length(children)),
            what = rep(what, times = length(children)),
            token = sapply(children, xml_text),
            do.call(rbind, lapply(children, xml_attrs))
          )
        }
      ))
    }
  ))
  retval[["topicPhraseReportDT"]] <- as.data.table(topic_phrase_report_df)
  
  retval
}


# setOldClass(Classes = "jobjRef")

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
#' @export mallet_topicmodel_load
mallet_topicmodel_load <- function(filename){
  parallel_topic_model <- rJava::J("cc/mallet/topics/ParallelTopicModel")$read(rJava::.jnew("java/io/File", filename))
  # Map in the RTopicModel class which can be evaluated using mallet package
  # y <- .jcast(parallel_topic_model, new.class = "cc/mallet/topics/RTopicModel", check = TRUE)
  parallel_topic_model
}
