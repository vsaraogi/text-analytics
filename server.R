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
    
    pal = brewer.pal(8,"Dark2")
    wordcloud(words = stats.noun$token, 
              min.freq=1,max_freq=200,
              scale = c(3.5, 0.5),
              rot.per=0.35,
              random.order = F,
              random.color = T,
              colors = pal)
    
  })
  
  output$wordcloud_verb <- renderPlot({
    news <- data()
    
    #Wordcloud for all the Verb
    stats.verb <- subset(news, upos %in% c("VERB"))
    
    
    pal = brewer.pal(8,"Dark2")
    wordcloud(words = stats.verb$token, 
              min.freq=1,max_freq=200,
              scale = c(3.5, 0.5),
              rot.per=0.35,
              random.order = F,
              random.color = T,
              colors = pal)
  })
  
  #Downloadable csv of selected dataset
  output$downloadData <- downloadHandler(
    news <- data(),
    
    filename = function() {
      paste("ann_doc", ".csv",sep="")
    },
    
    content = function(file) {
      write.csv(news, file)
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