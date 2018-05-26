#Displaying UDPipe NLP Workflow using ShinyApp

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
})

#Increasing the file upload limit to 30MB
#options(shiny.maxRequestSize = 30*1024^2)

# Define UI function
ui <- shinyUI(fluidPage(
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
        selected = c("ADJ", "NOUN", "PROPN")
      ),
      
      #Input: Select number of rows to display
      radioButtons(
        "disp",
        "Display Data:",
        choices = c(Head = "head",
                    All = "all"),
        selected = "head"
      )
      
    ),
    
    mainPanel(
      tabsetPanel(
        type = "tabs",
        
        tabPanel(
          "Overview",
          h4(p("Data input")),
          p(
            "This app supports only comma separated values (.csv) data file. CSV data file should have headers.",
            align = "justify"
          ),
          p("Please refer to the link below for sample csv file."),
          br(),
          h4('How to use this App'),
          p(
            'To use this app, click on',
            span(strong("Upload data (csv file with header)")),
            'and upload the csv data file. You can also change the Parts of Speech to modify the Cooccurences Plot'
          )
        ),
        
        tabPanel("Data",
                 tableOutput('contents')),
        
        tabPanel(
          "Annotated  Documents",
          dataTableOutput('ann_doc'),
          downloadButton("downloadData", "Download")
          #,tableOutput("ann_doc_file")
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
    model <- udpipe_download_model(language = "english")
    udmodel_english <-
      udpipe_load_model(file = 'english-ud-2.0-170801.udpipe')
    
    #Annotating the corpus
    news <- udpipe_annotate(udmodel_english, news$headline_text)
    #Converting the corpus to dataframe
    news <- data.frame(news)
    
    #Dataframe to be used across the app
    news <- subset(news, upos %in% input$upos)
    
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
    wordcloud(
      stats.noun$token,
      scale = c(4, 0.5),
      title = 'Noun',
      #min.freq = 0, max.words=100,
      colors = brewer.pal(8, "Dark2")
    )
    
  })
  
  output$wordcloud_verb <- renderPlot({
    news <- data()
    
    #Wordcloud for all the Verb
    stats.verb <- subset(news, upos %in% c("VERB"))
    wordcloud(
      stats.verb$token,
      scale = c(4, 0.5),
      title = 'Verb',
      #min.freq = 0, max.words=100,
      colors = brewer.pal(8, "Dark2")
    )
    
  })
  
  #Downloadable csv of selected dataset
  output$downloadData <- downloadHandler(
    news <- data(),
    
    filename = function() {
      paste("ann_doc", ".csv", sep = "")
    },
    
    content = function(file) {
      write.csv(news, file, row.names = FALSE)
    }
  )
  
  
  output$cog <- renderPlot({
    x <- data()
    
    x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")
    #Convert Parts Of Speech Tags To One-Letter Tags Which Can Be Used To Identify Phrases Based On Regular Expressions
    stats <-
      keywords_phrases(
        x = x$phrase_tag,
        term = tolower(x$token),
        pattern = "(A|N)*N(P+D*(A|N)*N)*",
        is_regex = TRUE,
        detailed = FALSE
      ) #Identifying Noun Phrase with the Regular Expression given
    stats <- subset(stats, ngram > 1 & freq > 3)
    stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
    a <-
      barchart(
        key ~ freq,
        data = head(stats, 20),
        col = "magenta",
        main = "Keywords - simple noun phrases",
        xlab = "Frequency"
      )
    a
  })
  
  
})

shinyApp(ui = ui, server = server)
