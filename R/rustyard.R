# plot = function(x, dtm, targetDir, verbose=TRUE){
#   if (verbose == TRUE) message("... getProbs")
#   dat <- LDAtools::getProbs(
#     word.id = self$topicmodel@wordassignments$j,
#     doc.id = self$topicmodel@wordassignments$i,
#     topic.id = self$topicmodel@wordassignments$v,
#     vocab = self$topicmodel@terms,
#     K = self$topicmodel@k,
#     sort.topics = "None"
#   )
#   json <- LDAvis::createJSON(
#     phi = dat$phi.hat, 
#     theta = dat$theta.hat, 
#     doc.length = slam::row_sums(dtm),
#     vocab = dimnames(dtm)[["Terms"]],
#     term.frequency = slam::col_sums(dtm),
#     reorder.topics=FALSE
#   )
#   LDAvis::serVis(json, out.dir=targetDir, open.browser = TRUE)  
# }
# 
# ,
# 
# label = function(noTerms=25, cloud=FALSE){
#   termDf <- terms(self$topicmodel, k=noTerms)
#   topicVector <- topics(self$topicmodel, 1)
#   docsGroupedByTopic <- split(names(topicVector), f=unname(topicVector))
#   i <- 1
#   while (i != 0 && i <= self$topicmodel@k){
#     message("... proceeding to topic ", i)
#     termsOfTopic <- termDf[,paste("Topic", i, sep=" ")]
#     if (cloud == TRUE) self$wordcloud(x=i)
#     print(termsOfTopic)
#     docNames <- sample(docsGroupedByTopic[[i]])
#     j <- 1
#     while (j <= length(docNames)){
#       if (as.character(i) %in% names(self$labels)){
#         message("There is already a label for the topic: ", self$labels[i])
#         choice <- menu(choices=c("keep label and move on", "assign a new label"))
#         if (choice == 1){
#           i <- i + 1
#           break
#         } else if (choice == 2){
#           message("... ok, you want to reconsider the label")
#         }
#       }
#       newLabel <- readline(prompt=">>> ")
#       print(newLabel)
#       if (newLabel == ""){
#         message("... moving on without assigning a label")
#         i <- i + 1
#         break
#       } else if (newLabel == "options"){
#         theMenu <- c(
#           "no entry + return to read speeches",
#           "'exit' to exit",
#           "'+' to move to next topic (without assigning a topic)",
#           "'-' to go to the previous topic",
#           "an integer to jump to the topic indicated",
#           "'labels' to show labels assigned so far",
#           "'options' to display these options",
#           "'missing' to show (numbers) of topics that have not yet been labelled",
#           "any other text: assign label and proceed to next topic"
#         )
#         cat(paste(theMenu, collapse="\n")) 
#       } else if (newLabel == "exit"){
#         i <- 0
#         break
#       } else if (newLabel == "+"){
#         i <- i + 1
#         break
#       } else if (newLabel == "labels"){
#         print(self$labels)
#       } else if (newLabel == "missing"){
#         allTopicNos <- as.character(c(1:self$topicmodel@k))
#         print(allTopicNos[!allTopicNos %in% names(self$labels)])
#       } else if (newLabel == "-"){
#         i <- i - 1
#         break
#       } else if (grepl("^[0-9]*$", newLabel)){
#         i <- as.integer(newLabel)
#         break
#       } else if (newLabel == "r"){
#         print(i)
#         read(self$topicmodel, partitionObject=as(self$bundle[[docNames[j]]], "plprPartition"), noTopics=3, noToken=20)
#         j <- j + 1
#       } else if (newLabel != "") {
#         if (newLabel %in% self$labels){
#           message("... label already exists: ", grep(newLabel, self$labels))
#         } else {
#           self$labels <- c(self$labels, setNames(newLabel, as.character(i)))
#           i <- i + 1
#           break
#         }
#       }
#     }
#   }
# }