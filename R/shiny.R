#' Run Shiny App for Topic Analysis.
#' 
#' @export topicanalysis
#' @importFrom shiny runApp
#' @import methods
#' @rdname topicanalysis_app
topicanalysis <- function() runApp(system.file("shiny", package = "topicanalysis"))
