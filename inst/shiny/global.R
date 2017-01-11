library(topicmodels)
library(DT)
library(shinythemes)
library(data.table)
# library(shinyBS)
# library(polmineR)

topicanalysisObjects <- polmineR::getObjects('Topicanalysis', envir = .GlobalEnv)
shinyThemeToUse <- shinytheme("cerulean") # alternatives: flatly, cerulean
toPassOver <- c()

makeOverviewDf <- function(ta_object){
  data.frame(
    topic = as.character(1:get(ta_object)$topicmodel@k),
    label = get(ta_object)$labels,
    exclude = as.character(get(ta_object)$exclude)
  )
}

getTopicIndex <- function(x) as.integer(gsub("^(\\d+).*?$", "\\1", x))

makeTopicChoices <- function(labels){
  paste(
    as.character(c(1:length(labels))),
    sapply(labels, function(x) ifelse(grepl("^\\d+$", x), "", paste(' / "', x, '"', sep=""))),
    sep=""
  )
}
