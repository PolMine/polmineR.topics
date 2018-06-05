#' start shiny application for topic analysis
#' 
#' @param dev logical, if TRUE, start shiny app in dev version of the package
#' @export shiny
#' @importFrom shiny runApp
shiny <- function(dev=TRUE){
  if (dev == TRUE){
    runApp("/Users/blaette/Lab/repos/polmineR.topics/inst/shiny")
  } else {
    runApp(system.file("shiny", package="polmineR.topics"))
  }
}