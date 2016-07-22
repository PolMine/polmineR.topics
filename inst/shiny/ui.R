shinyUI(
  
  navbarPage(
    
    theme = shinyThemeToUse,
    title = "topicanalysis",
    id = "topicanalysis",
    
    tags$head(tags$script(src="customn_functions.js")),
    
    tabPanel(
      "overview",
      sidebarLayout(
        sidebarPanel(
          selectInput("ta_object", "TA-Object", choices=topicanalysisObjects),
          sliderInput("overview_terms", "terms (max)", min=10, value=100, max=500, step=10),
          actionButton("overview_dontignore", "consider", icon=icon("eye-open", lib="glyphicon")),
          actionButton("overview_ignore", "ignore", icon=icon("eye-close", lib="glyphicon"))
        ),
        mainPanel(
            DT::dataTableOutput("overview_df")  
        )
        
      )
    ),
    
    tabPanel(
      "topics",
      sidebarLayout(
        sidebarPanel(
          selectInput("label_topic_no", "topic", choices=makeTopicChoices(get(topicanalysisObjects[1])$labels)),
          selectInput(
            "label_category", "category",
            choices=c("", get(topicanalysisObjects[1])$categories)
            ),
          textInput("label_label", "label", value=get(topicanalysisObjects[1])$labels[1]),
          actionButton("label_backward", label="", icon=icon("backward", lib="font-awesome")),
          actionButton("label_assign", "assign"),
          actionButton("label_forward", label="", icon=icon("forward", lib="font-awesome"))
        ),
        
        mainPanel(
        tabsetPanel(
          tabPanel(
            "terms",
            DT::dataTableOutput("label_terms_table")
          ),
          tabPanel(
            "documents",
            DT::dataTableOutput("label_document_table")
          ),
          tabPanel(
            "wordcloud",
            plotOutput("wordcloud")
          )
        )
          )
      )
    ),
    
    
    tabPanel(
      "documents",
      sidebarLayout(
        sidebarPanel(
          radioButtons("docs_how", "select by", choices=c("topic", "meta"), inline = TRUE),
          sliderInput("docs_k", "topics evaluated", min=1, max=10, value=3, step = 1, ticks=F),
          selectInput("docs_topics_1", "topic", choices=get(topicanalysisObjects[1])$labels),
          selectInput("docs_topics_2", "topic (2)", choices=c("---", get(topicanalysisObjects[1])$labels)),
          selectInput(
            "docs_meta", "sAttributes",
            choices=ifelse(
              !is.null(get(topicanalysisObjects[1])$bundle),
              sAttributes(get(topicanalysisObjects[1])$bundle[[1]]),
              ""
              ),
            multiple=TRUE
            ),
          conditionalPanel(
            "input.docs_how == 'meta'",
            uiOutput("docs_sAttr_select"),
            actionButton("docs_go_button", "subset")
          )
        ),
        mainPanel(
          DT::dataTableOutput("docs_table")
        )
      )
    ),
    
    tabPanel(
      title="fulltext",
      id="fulltext",
      sidebarLayout(
       sidebarPanel(
         selectInput("fulltext_id", "id", choices=get(topicanalysisObjects[1])$topicmodel@documents[1:100]),
         sliderInput("fulltext_topics", "topics", min=1, max=10, value=1, step=1, ticks=FALSE),
         sliderInput("fulltext_terms", "terms", min=5, max=500, value=100, step=5),
         actionButton("fulltext_backward", "<<"),
         actionButton("fulltext_forward", ">>")
         
       ),
       mainPanel(
        uiOutput("fulltext_fulltext")
        )
      )
    ),
    
    tabPanel(
      "time series",
      sidebarLayout(
        sidebarPanel(
            selectInput("ts_topic", "topic", choices=get(topicanalysisObjects[1])$labels[1], multiple = TRUE),
            selectInput("ts_k", "k", choices=as.character(c(1:10))),
            selectInput("ts_aggregation", "aggregation", choice=c("none", "month", "quarter", "year"), selected="year")
          ),
        
        mainPanel(
          tabsetPanel(
            tabPanel(
              "table",
              DT::dataTableOutput("ts_table")
            ),
            tabPanel(
              "chart",
              plotOutput("ts_chart")
            )
          )
          
          
        )
      )
    ),
    
    tabPanel(
      "cooccurrences",
      sidebarLayout(
        sidebarPanel(
          selectInput("cooccurrences", "threshold", choices=c("10.83", "3.84", "0"), selected="0"),
          selectInput("cooccurrences_k", "k", choices=as.character(c(1:20)), selected="3")
        ),
        
        mainPanel(
          tabsetPanel(
            tabPanel(
              "table",
              DT::dataTableOutput("cooccurrences_table")
            ),
            tabPanel(
              "docs"
            ),
            tabPanel(
              "time series"
            )
            
          )
          
        )
      )
    )
    
  )
)
