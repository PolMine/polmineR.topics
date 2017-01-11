shinyServer(function(input, output, session) {

  ## overview page 
  
  observeEvent(
    input$ta_object,
    {
      updateSelectInput(
        session, "ts_topic",
        choices = get(input$ta_object)$labels
      )
      updateSelectInput(
        session, "label_topic_no",
        choices = makeTopicChoices(get(input$ta_object)$labels)
      )
      updateSelectInput(
        session, "label_category",
        choices = c("", get(input$ta_object)$categories)
      )
      updateSelectInput(session, "docs_topics_1", choices = get(input$ta_object)$labels)
      updateSelectInput(session, "docs_topics_2", choices = c("---", get(input$ta_object)$labels))
    }
  )

  values <- reactiveValues()
  
  observe({
    input$ta_object
    input$overview_terms
    isolate({
      termTab <- posterior(get(input$ta_object)$topicmodel)[["terms"]]
      termMatrix <- terms(get(input$ta_object)$topicmodel, k=input$overview_terms)
      termDf <- as.data.frame(t(termMatrix), stringsAsFactors = F)
      colnames(termDf) <- as.character(c(1:ncol(termDf)))
    })
    values$termDf <- termDf
    values$termTab <- termTab
    })
  
  output$overview_df <- DT::renderDataTable({
    makeOverviewDf(input$ta_object) # in global.R
  }, options=list(pageLength=25))
  
  observeEvent(
    input$overview_ignore,
    {
      if (!is.null(input$overview_df_rows_selected)){
        for (x in input$overview_df_rows_selected) get(input$ta_object)$ignorance(x, TRUE)
      }
      output$overview_df <- DT::renderDataTable({
        makeOverviewDf(input$ta_object) # in global.R
      },  options = list(pageLength=25)
      )
    }
  )
  
  observeEvent(
    input$overview_dontignore,
    {
      if (!is.null(input$overview_df_rows_selected)){
        for (x in input$overview_df_rows_selected) get(input$ta_object)$ignorance(x, FALSE)
      }
      output$overview_df <- DT::renderDataTable({
        makeOverviewDf(input) # in global.R
      }, options = list(pageLength = 25, displayStart = 25))
    }
  )
  
  
  
  # label
  
  observeEvent(
    input$label_category,
    {
      if (input$label_category != ""){
        updateTextInput(session, "label_label", value = input$label_category)  
      }
    }
  )
  
  observeEvent(
    input$label_forward,
    {
      currentIndex <- as.integer(gsub("^(\\d+)\\s.*?$", "\\1", input$label_topic_no))
      if (!input$label_label %in% get(input$ta_object)$labels[-currentIndex]) {
        if (currentIndex < get(input$ta_object)$topicmodel@k){
          newIndex <- currentIndex + 1
          if (input$label_label != get(input$ta_object)$labels[currentIndex]){
            get(input$ta_object)$relabel(getTopicIndex(input$label_topic_no), input$label_label)
            if (!input$label_label %in% get(input$ta_object)$categories){
              get(input$ta_object)$addCategory(input$label_label)
              updateSelectInput(session, "label_category", choices = c("", get(input$ta_object)$categories), selected=NULL)
            }
          }
          updateSelectInput(session, "label_category", selected="")
          updateSelectInput(session, "ts_topic", choices = get(input$ta_object)$labels)
          updateSelectInput(session, "docs_topics_1", choices = get(input$ta_object)$labels)
          updateSelectInput(session, "docs_topics_2", choices = c("NONE", get(input$ta_object)$labels))
          topicChoices <- makeTopicChoices(get(input$ta_object)$labels)
          updateSelectInput(session, "label_topic_no", choices = topicChoices, selected=topicChoices[newIndex])
          updateTextInput(session, "label_label", value = get(input$ta_object)$labels[newIndex])
        }
      } else {
        session$sendCustomMessage(type='jsCode', list(value = 'alert("Label already exists, labels must be unique");'))
      }
    }
  )
  
  observeEvent(
    input$label_backward,
    {
      currentIndex <- getTopicIndex(input$label_topic_no)
      if (!input$label_label %in% get(input$ta_object)$labels[-currentIndex]) {
        if (currentIndex > 1){
          newIndex <- currentIndex - 1
          if (input$label_label != get(input$ta_object)$labels[currentIndex]){
            get(input$ta_object)$relabel(getTopicIndex(input$label_topic_no), input$label_label)   
            if (!input$label_label %in% get(input$ta_object)$categories){
              get(input$ta_object)$addCategory(input$label_label)
              updateSelectInput(session, "label_category", choices = c("", get(input$ta_object)$categories))
            }
          }
          updateSelectInput(session, "label_category", selected="")
          updateSelectInput(session, "docs_topics_1", choices = get(input$ta_object)$labels)
          updateSelectInput(session, "docs_topics_2", choices = c("NONE", get(input$ta_object)$labels))
          topicChoices <- makeTopicChoices(get(input$ta_object)$labels)
          updateSelectInput(session, "label_topic_no", choices = topicChoices, selected=topicChoices[newIndex])
          updateTextInput(session, "label_label", value = get(input$ta_object)$labels[newIndex])
        }
      } else {
        session$sendCustomMessage(type='jsCode', list(value = 'alert("Label already exists, labels must be unique");'))
      }
    }
  )
  
  observeEvent(
    input$label_topic_no,
    {
      topicIndex <- getTopicIndex(input$label_topic_no)
      # updateTextInput(session, "label_label", value = get(input$ta_object)$labels[topicIndex])
    }
  )
  
  output$label_terms_table <- DT::renderDataTable({
    input$overview_terms
    input$label_topic_no
    isolate({
      termVector <- values$termTab[getTopicIndex(input$label_topic_no), ]
      values$termVector <- termVector[order(termVector, decreasing = T)][c(1:as.integer(input$overview_terms))]
    })
    data.frame(term=names(values$termVector), posterior=round(unname(values$termVector), 5))
    }, options=list(pageLength=as.integer(input$overview_terms), lengthMenu=-1, paging=FALSE))
  
  observeEvent(
    input$label_assign,
    {
      if (!input$label_label %in% get(input$ta_object)$labels) {
        get(input$ta_object)$relabel(as.integer(input$label_topic_no), input$label_label)
        if (!input$label_label %in% get(input$ta_object)$categories){
          get(input$ta_object)$addCategory(input$label_label)
          updateSelectInput(session, "label_category", choices = c("", get(input$ta_object)$categories))
        }
        updateSelectInput(session, "ts_topic", choices = get(input$ta_object)$labels)
        updateSelectInput(session, "docs_topics_1", choices = get(input$ta_object)$labels)
        updateSelectInput(session, "docs_topics_2", choices = c("NONE", get(input$ta_object)$labels))
      } else {
        session$sendCustomMessage(type='jsCode', list(value = 'alert("Label already exists, labels must be unique");'))
      }
      
    }
  )
  
  output$docs_table <- DT::renderDataTable({
    input$docs_topics_1
    input$docs_topics_2
    input$docs_go_button
    input$docs_how
    input$docs_meta
    isolate({
      topicMatrix <- topics(x=get(input$ta_object)$topicmodel, k=as.integer(input$docs_k))
      if (input$docs_topics_2 == "---"){
        docs <- get(input$ta_object)$docs(x=input$docs_topics_1, n=input$docs_k)
      } else if (input$docs_topics_2 != "---"){
        docs <- get(input$ta_object)$docs(x=input$docs_topics_1, y=input$docs_topics_2, n=input$docs_k)
      }
      if (length(input$docs_meta) == 0){
        values$documents_df <- data.frame(docs=docs, stringsAsFactors=FALSE)  
      } else if (length(input$docs_meta) > 0){
        dfSupplement <- lapply(
          setNames(input$docs_meta, input$docs_meta),
          function(x){
            sapply(docs, function(y) sAttributes(get(input$ta_object)$bundle@objects[[y]], x))
          })
        values$documents_df <<- data.frame(
          docs=docs, dfSupplement,
          as.data.frame(t(topicMatrix[, docs])),
          stringsAsFactors=FALSE, row.names=NULL
        )
        if (input$docs_how == "meta"){
          for (x in input$docs_meta){
            if (length(input[[paste("docs_meta", x, sep="_")]]) > 0){
              rowsToGet <- which(values$documents_df[[x]] == input[[paste("docs_meta", x, sep="_")]])
              values$documents_df <<- values$documents_df[rowsToGet,]
            }
          }
        }
      }
    })
    DT::datatable(values$documents_df, selection="single", rownames=FALSE)
  })
  

  observeEvent(
    input$docs_table_rows_selected,
    {
      updateNavbarPage(session, inputId="topicanalysis", selected = "fulltext")
      id_selected <- values$documents_df[input$docs_table_rows_selected, "docs"]
      print(id_selected)
      updateSelectInput(
        session, "fulltext_id",
        choices=values$documents_df[,"docs"], selected=id_selected
        )
      }
  )
  
  output$docs_sAttr_select <- renderUI({
    tagList(lapply(
      input$docs_meta,
      function(x) textInput(paste("docs_meta", x, sep="_"), x)
    ))
  })
  
  observeEvent(
    input$docs_go_button,
    {
      for (x in input$docs_meta){
        print(input[[paste("docs_meta", x, sep="_")]])
      }
    }
  )
  
  output$label_document_table <- DT::renderDataTable({
    input$label_topic_no
    docsToGet <- get(input$ta_object)$docs(as.integer(input$label_topic_no), n=3)
    docDf <<- data.frame(
      docs = docsToGet,
#      size = sapply(docsToGet, function(x) size(get(input$ta_object)$bundle[[x]])),
#      date = sapply(docsToGet, function(x) sAttributes(get(input$ta_object)$bundle[[x]], "text_date")),
      stringsAsFactors = FALSE
      )
    DT::datatable(docDf, selection="single", rownames=FALSE)
  })
  
  observeEvent(
    input$label_document_table_rows_selected,
    {
      if (length(input$label_document_table_rows_selected) > 0){
        toHighlight <- unlist(unname(
          values$termDf[as.integer(input$label_topic_no), c(1:as.integer(input$overview_terms))]
          ))
        fulltext <- html(
          as(
            get(input$ta_object)$bundle[[docDf[input$label_document_table_rows_selected, "docs"]]],
            "plprPartition"
          ),
          input$label_document_table_rows_selected,
          meta=c("text_name", "text_date")
          )
        fulltext <- polmineR:::highlight(fulltext, highlight=list(yellow=toHighlight))
        browse(fulltext)
      }
    })

  
  output$wordcloud <- renderPlot({
    input$label_topic_no
    input$label_terms_displayed
    termVector <- values$termTab[as.integer(input$label_topic_no), ]
    termVector <- termVector[order(termVector, decreasing = T)][c(1:as.integer(input$label_terms_displayed))]
    wordcloud::wordcloud(words=names(termVector), freq=unname(termVector), scale=c(4,.5))
  })
  
  output$cooccurrences_table <- DT::renderDataTable({
    tab <- polmineR.topics::cooccurrences(get(input$ta_object)$topicmodel, k=input$cooccurrences_k)
    tab[, x_label := get(input$ta_object)$labels[tab[["x"]] ]]
    tab[, y_label := get(input$ta_object)$labels[tab[["y"]] ]]
    tab
  })
  
  output$ts_table <- DT::renderDataTable({
    input$ts_topic
    input$ts_aggregation
    zooObject <- get(input$ta_object)$as.zoo(
      x = which(get(input$ta_object)$labels == input$ts_topic),
      k = as.integer(input$ts_k),
      aggregation = input$ts_aggregation
    )
    values$zoo <- zooObject
    as.data.frame(zooObject)
    # plot(zooObject)
  })
  
  output$ts_chart <- renderPlot({plot(values$zoo)})
  
  # observeEvent(
  #   values$zoo,
  #   {
  #     output$ts_chart <- renderPlot({plot(values$zoo)})
  #   }
  #   
  # )

  output$fulltext_fulltext <- renderUI({
    input$fulltext_id
    noTopics=input$fulltext_topics
    input$fulltext_terms
    partitionObject <- as(get(input$ta_object)$bundle[[input$fulltext_id]], "plprPartition")
    toHighlight <- .makeHighlightList(
      .Object=get(input$ta_object)$topicmodel,
      partitionObject=partitionObject,
      noTopics=input$fulltext_topics,
      noToken=input$fulltext_terms
      )
    everything <- polmineR::html(partitionObject, meta="text_year")
    everything <- polmineR:::highlight(everything, highlight=toHighlight)
    body <- gsub("^.*?<body>(.*?)</body>.*?$", "\\1", everything)
    shiny::HTML(body)
    })
  
  observeEvent(
    input$reading,
    {print(input$reading)}
  )
  
})
