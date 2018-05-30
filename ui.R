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