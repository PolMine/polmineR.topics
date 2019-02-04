#' calculate and compare a set of topicmodels 
#' 
#' @param ks numeric vector, number of topics
#' @param method method (defaults to "Gibbs")
#' @param burnin foo
#' @param dtm dtm
#' @param iter number of iterations
#' @param keep keep
#' @param verbose verbose 
#' @param x x
#' @param mc number of cores
#' @exportClass ldaBundle
#' @rdname ldaBundle
setClass(
  "ldaBundle",
  representation(
    objects="list",
    ks="numeric",
    burnin="numeric",
    keep="numeric",
    iter="numeric"
    )
)

#' @export ldaBundle
#' @importFrom topicmodels LDA
#' @importFrom parallel mclapply
#' @rdname ldaBundle
ldaBundle <- function(dtm, ks, method="Gibbs", burnin=1000, iter=1000, keep=50, mc=2, verbose=TRUE){
  bundle <- new("ldaBundle", ks=ks, burnin=burnin, iter=iter, keep=keep)
  bundle@objects <- mclapply(
    ks,
    function(k) {
      LDA(dtm, k, method = "Gibbs", control = list(burnin = burnin, iter = iter, keep = keep, verbose=verbose))
    },
    mc.cores=mc)
  bundle
}

#' @exportMethod mean
#' @importFrom Rmpfr mpfr
#' @importFrom stats median
#' @rdname ldaBundle
setMethod("mean", "ldaBundle", function(x){
  .harmonicMean <- function(logLikelihoods, precision = 2000L) {
    llMed <- median(logLikelihoods)
    as.double(
      llMed - log(mean(exp(-Rmpfr::mpfr(logLikelihoods, prec = precision) + llMed)))
    )
  }
  logLiks <- lapply(x, function(L)  L@logLiks[-c(1:(x@burnin/x@keep))])
  hm <- sapply(logLiks, function(h) .harmonicMean(h))
  hm
})

#' @exportMethod plot
#' @rdname ldaBundle
setMethod("plot", "ldaBundle", function(x, ks, burnin, keep){
  hm <- mean(x)
  plot(x@ks, hm, type = "l")  
})

#' @exportMethod max
#' @rdname ldaBundle
setMethod("max", "ldaBundle", function(x){
  hm <- mean(x)
  x[which.max(hm)][[1]]
})

