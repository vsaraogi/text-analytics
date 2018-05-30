#TA Group Assignment 2
#Srikanth Yarlagadda | Surabhi Kumar | Varun Saraogi

#UDPipe NLP Workflow using ShinyApp

#Import the Packages
suppressPackageStartupMessages({
  if (!require(shiny)) {
    install.packages("shiny")
  }
  library(shiny)
  if (!require(udpipe)) {
    install.packages("udpiper")
  }
  library(udpipe)
  if (!require(dplyr)) {
    install.packages("dplyr")
  }
  library(dplyr)
  if (!require(ggplot2)) {
    install.packages("ggplot2")
  }
  library(ggplot2)
  if (!require(stringr)) {
    install.packages("stringr")
  }
  library(stringr)
  if (!require(wordcloud)) {
    install.packages("wodcloud")
  }
  library(wordcloud)
  if (!require(ggraph)) {
    install.packages("ggraph")
  }
  library(ggraph)
  if (!require(igraph)) {
    install.packages("igraph")
  }
  library(igraph)
  if (!require(shinythemes)) {
    install.packages("shinythemes")
  }
  library(shinythemes)
  if (!require(RColorBrewer)) {
    install.packages("RColorBrewer")
  }
  library(RColorBrewer)
})

#Increasing the file upload limit to 30MB
#options(shiny.maxRequestSize = 60 * 1024 ^ 2)

# Define UI function
ui <- shinyUI(fluidPage(
  theme = shinytheme("spacelab"),
  
  titlePanel("NLP Worklflow using UDPipe"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput(
        "file",
        "Choose CSV File",
        multiple = FALSE,
        accept = c("text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv")
      ),
      
      #Horizontal line
      tags$hr(),
      
      #Select the upos
      checkboxGroupInput(
        "upos",
        "Select the Parts of Speech:",
        choices = c(
          "Adjective" = "ADJ",
          "Noun" = "NOUN",
          "Proper Noun" = "PROPN",
          "Adverb" = "ADV",
          "Verb" = "VERB"
        ),
        selected = c("ADJ", "NOUN", "VERB")
      ),
      
      #Input: Select number of rows to display
      radioButtons(
        "disp",
        "Display Data:",
        choices = c(Head = "head",
                    Top100 = "top100"),
        selected = "head"
      )
      
    ),
    
    mainPanel(
      tabsetPanel(
        type = "tabs",
        
        tabPanel(
          "Overview",
          br(),
          h4(p("Features")),
          p(
            "This app uses UDPipe to create Annotated document from the uploaded file."
          ),
          p("The app also creates Wordcloud for the Noun and Verb tokens."),
          p("The app supports coocurrence plot for top 30 most occuring tokesn."),
          br(),
          h4(p("Data input")),
          p(
            "This app supports only comma separated values (.csv) data file. CSV data file should have headers with two columns.",
            align = "justify"
          ),
          p(
            "The first column contains date with format as YYYYMMDD, the second column contains text."
          ),
          br(),
          h4('How to use this App'),
          p(
            'To use this app, click on',
            span(strong("Upload data (csv file with header)")),
            'and upload the csv data file.'
          ),
          p(
            "You can also change the Parts of Speech to modify the Cooccurences Plot."
          ),
          p(
            "You can select the records to display, head displays top 6 records and the other option top 100 records."
          )
        ),
        
        tabPanel("Data",
                 tableOutput('contents')),
        
        tabPanel(
          "Annotated  Documents",
          br(),
          downloadButton("downloadData", "Download"),
          br(),
          br(),
          dataTableOutput('ann_doc')
        ),
        
        tabPanel(
          "Wodcloud Plot",
          plotOutput('wordcloud_noun'),
          plotOutput('wordcloud_verb')
        ),
        
        tabPanel("Cooccurence Plot",
                 plotOutput('cog'))
      )
    )
  )
))


# Define Server function
server <- shinyServer(function(input, output) {
  data <- reactive({
    req(input$file)
    
    news <-
      read.csv(input$file$datapath,
               header = T,
               stringsAsFactors = F)
    #Split the date column into Year, Month and Days
    news <- news %>% mutate(
      year = str_sub(publish_date, 1, 4),
      month = str_sub(publish_date, 5, 6),
      date = str_sub(publish_date, 7, 8)
    )
    
    #Import English model from udpipe library
    #model <- udpipe_download_model(language = "english")
    udmodel_english <-
      udpipe_load_model(file = 'english-ud-2.0-170801.udpipe')
    
    #Annotating the corpus
    news <- udpipe_annotate(udmodel_english, news$headline_text)
    #Converting the corpus to dataframe
    news <- data.frame(news)
    
    #Dataframe to be used across the app
    news <- subset(news, upos %in% input$upos)
    
    return (news)
    
  })
  
  output$contents <- renderTable({
    # input$file will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file)
    
    df <- read.csv(input$file$datapath)
    
    if (input$disp == "head") {
      return(head(df))
    }
    else {
      return(head(df, 100))
    }
    
  })
  
  output$ann_doc <- renderDataTable({
    news <- data()
    
    news_wo_sent <- subset(news, select = -c(sentence, sentence_id))
    
    #Displaying only 100 tokens
    news_100 <- head(news_wo_sent, 100)
    
  })
  
  output$wordcloud_noun <- renderPlot({
    news <- data()
    
    #Wordcloud for all the Noun
    stats.noun <- subset(news, upos %in% c("NOUN"))
    
    pal = brewer.pal(8, "Dark2")
    wordcloud(
      words = stats.noun$token,
      min.freq = 1,
      max_freq = 200,
      scale = c(3.5, 0.5),
      rot.per = 0.35,
      random.order = F,
      random.color = T,
      colors = pal
    )
    
  })
  
  output$wordcloud_verb <- renderPlot({
    news <- data()
    
    #Wordcloud for all the Verb
    stats.verb <- subset(news, upos %in% c("VERB"))
    
    
    pal = brewer.pal(8, "Dark2")
    wordcloud(
      words = stats.verb$token,
      min.freq = 1,
      max_freq = 200,
      scale = c(3.5, 0.5),
      rot.per = 0.35,
      random.order = F,
      random.color = T,
      colors = pal
    )
  })
  
  #Downloadable csv of selected dataset
  output$downloadData <- downloadHandler(
    filename = function() {
      "annotated_data.csv"
    },
    
    content = function(file) {
      write.csv(data()[, -4], file, row.names = F)
      
    }
  )
  
  
  output$cog <- renderPlot({
    news <-
      cooccurrence(
        x = subset(data(), upos %in% input$upos),
        term = "lemma",
        group = c("doc_id", "paragraph_id", "sentence_id")
      )
    
    wordnetwork <- head(news, 30)
    
    wordnetwork <- igraph::graph_from_data_frame(wordnetwork)
    
    ggraph(wordnetwork, layout = "fr") +
      geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour =
                       "orange") +
      geom_node_text(aes(label = name), colour = "darkgreen", size = 10) +
      
      theme_graph(base_family = "Arial Narrow") +
      theme(legend.position = "none") +
      labs(title = "Cooccurrences Plot", subtitle = "Nouns & Adjective")
    
  })
  
})

shinyApp(ui = ui, server = server)
