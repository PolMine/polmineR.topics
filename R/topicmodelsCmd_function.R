#' generate command to run lds topicmodelling from command line
#' 
#' @param method either ctm or lda
#' @param infile input file
#' @param outfile output file (full path)
#' @param k number of topics
#' @export topicmodelsCmd
topicmodelsCmd <- function(method="lda", infile, outfile, k){
  what <- switch(
    method,
    ctm = "topicmodels_ctm.R",
    lda = "topicmodels_lda.R"
  )
  cmd <- paste(
    "Rscript",
    system.file("Rscript", what, package="polmineR.topics"),
    "-f", infile,
    "-o", outfile,
    "-k", k,
    sep=" "
  )
  paste(cmd, sep=" ")
}

