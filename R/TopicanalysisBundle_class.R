#' Annotations
#' 
#' @import R6
#' @field foo some explanation
#' @field foo2 anotther explanation
#' @section Methods:
#' \describe{
#' \item{\code{anyMethod(x)}}{do something}
#' }
#' @examples 
#' \dontrun{
#' }
#' @export Annotations
Annotations <- R6Class(
  
  "Annotations",
  
  public = list(
    
    # fields 
    
    topicmodels = list(),
    filenames = NULL,
    
    # methods
    
    length = function() length(self$topicmodels),
    
    names = function() unname(sapply(self$topicmodels, function(x) x$name))
  )
)