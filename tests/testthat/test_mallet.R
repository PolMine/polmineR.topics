library(testthat)
library(data.table)
library(polmineR)
library(topicanalysis)
library(rJava)

testthat::context("mallet")

test_that(
  "creating, saving and loading mallet instance list",
  {
    use("polmineR")
    speeches <- polmineR::as.speeches("GERMAPARLMINI", s_attribute_name = "speaker", progress = FALSE)
    instance_list <- topicanalysis::mallet_make_instance_list(speeches, verbose = FALSE)
    instancefile <- mallet_instance_list_store(instance_list)
    instance_list_reloaded <- mallet_instance_list_load(instancefile)
    expect_identical(instance_list$size(), instance_list_reloaded$size())
  }
)

