#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinythemes)
library(textreadr)
library(tm)
library(pdftools)
library(wordcloud)
library(wordcloud2)
library(NLP)
library(dplyr)
library(stringr)
library(tidytext)
library(tidyverse)
library(tibble)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(igraph)
library(ggraph)
library(reshape2)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cerulean"),
                
                # Application title
                titlePanel("Valentine's Gift Survey Analysis"),
                
                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                    sidebarPanel(
                        tags$h3("Conditions for filtering:"),
                        checkboxGroupInput("questions",
                                           "Target data source:",
                                           choices = c(
                                               "Question 1: What is your ideal Valentines day, can you elaborate it to us?" = "Q1",
                                               "Question 2: What is the most romantic things you did for your lover in previous valentines day?" = "Q2",
                                               "Question 3: Under lockdown, do you any plan  for the valentines day this year? " = "Q3",
                                               "Question 4: Except of traditional gift, like flower, chocolates, etc, do you consider some out of box gift ideas?" = "Q4",
                                               "Question 5: In digital ages, all the sweet memories are stored in our multiple device, do you consider to make a cool gift through digital method, if you do , elaborate your ideas?" = "Q5"
                                           ),
                                           selected = c("Q1","Q2","Q3","Q4","Q5")
                        ),
                        checkboxGroupInput("answer",
                                           "Do you consider PIRATE DIGITAL LOVE CARD as a gift to  your lover?",
                                           choices =c(
                                               "Yes" = 'YES',
                                               "No" = "NO"
                                           ),
                                           selected = c("YES","NO")
                        ),
                        sliderInput("num", "Maximum number of words for wordcloud", min=1, max=200, value = 100),
                        #colorInput("col", "Background color", value = "white"),
                        selectInput("sentiment", "Choose lexicon for sentiment analysis", choices=c("afinn", "nrc", "bing"), selected="afinn"),
                        sliderInput("n_rows", "The number of ranks shown in plot", min=1, max=100, value=20)
                    ),
                    
                    # Show a plot of the generated distribution
                    mainPanel(
                        tabsetPanel(
                            tabPanel(
                                title="Original data",
                                DT::dataTableOutput("od")
                            ),
                            tabPanel(
                                title="Raw Data after filtering and tokenization",
                                plotlyOutput("p_table"),
                                DT::dataTableOutput("table")
                            ),
                            tabPanel(
                                title="Raw data for Sentimental Analysis",
                                DT::dataTableOutput("table_sentiment")
                            ),
                            tabPanel(
                                title="Sentimental Analysis",
                                plotOutput("sentiment")
                            ),
                            tabPanel(
                                title="Wordcloud",
                                wordcloud2Output("p_wordcloud")
                            ),
                            tabPanel(
                                title="TF-IDF",
                                plotlyOutput("p_tfidf_bigram"),
                                DT::dataTableOutput("p_tfidf")
                            )
                        )
                    )
                )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #Importing all .txt files from one directory # a txt works like a csv file with multiple rows
    setwd("/Users/tetsuyamano/Documents/R/TextAnalytics_group/Transcripts_cleaned")
    nm <- list.files(path="/Users/tetsuyamano/Documents/R/TextAnalytics_group/Transcripts_cleaned")
    
    #make empty datafram
    df_data <- data.frame(person=character(), answer=character(), question=character(), file=character(), text=character())
    
    #import data   
    for (i in 1:length(nm)){
        wk_filename <- strsplit(nm[i], "_")
        df_wk <- data.frame(matrix(unlist(wk_filename), nrow=length(wk_filename), byrow=TRUE))
        names(df_wk) <- c("person", "answer", "question", "file")
        df_wk$text <- read_document(file=nm[i])
        df_data <- rbind(df_data, df_wk)
    }
    
    #import stop_words
    data(stop_words)
    
    #delete redundunt column
    df_data <- df_data[, colnames(df_data) != "file"]
    
    #Creates original lexicon of stop words that I want to remove
    custom_lex <- data.frame(word=c('yeah'), lexicon=rep('custom', each=1))
    
    #combining both lexicons the stop_words and the custom_lex
    binded_stopwords <- rbind(stop_words, custom_lex)
    
    create_wordcloud <- function(data, num_words = 100, background = "white") {
        
        # If text is provided, convert it to a dataframe of word frequencies
        if (is.character(data)) {
            corpus <- Corpus(VectorSource(data))
            corpus <- tm_map(corpus, tolower)
            corpus <- tm_map(corpus, removePunctuation)
            corpus <- tm_map(corpus, removeNumbers)
            corpus <- tm_map(corpus, removeWords, stopwords("english"))
            tdm <- as.matrix(TermDocumentMatrix(corpus))
            data <- sort(rowSums(tdm), decreasing = TRUE)
            data <- data.frame(word = names(data), freq = as.numeric(data))
        }
        
        # Make sure a proper num_words is provided
        if (!is.numeric(num_words) || num_words < 3) {
            num_words <- 3
        }  
        
        # Grab the top n most common words
        data <- head(data, n = num_words)
        if (nrow(data) == 0) {
            return(NULL)
        }
        
        wordcloud2(data, backgroundColor = background)
    }
    
    data_original <- reactive({
        df_data %>%
            filter(question %in% input$questions) %>%
            filter(answer %in% input$answer)
    })
    
    data_source <- reactive({
        data_original() %>%
            unnest_tokens(word, text) %>%
            anti_join(binded_stopwords)
    })
    
    data_sentiment <- reactive({
        data_source() %>%
            count(word,sort=TRUE) %>%
            inner_join(get_sentiments(input$sentiment), by = "word") 
    })
    
    output$od <- DT::renderDataTable({
        df_data
    })
    
    output$table <- DT::renderDataTable({
        data_source()
    })
    
    output$p_table <- renderPlotly({
        ggplotly({
            data_source() %>%
                count(word, sort=TRUE) %>%
                mutate(word = reorder(word, n)) %>%
                top_n(input$n_rows, wt=n) %>%
                ggplot(aes(word, n, fill=n)) + 
                geom_bar(stat = "identity") + ylab("Word frequencies") + 
                coord_flip()
        })
    })
    
    output$table_sentiment <- DT::renderDataTable({
        data_sentiment()
    })
    
    output$sentiment <- renderPlot({
        if (input$sentiment %in% c("nrc")){
            data_sentiment() %>%
                acast(word ~ sentiment, value.var="n", fill=0) %>%
                comparison.cloud(colors  = c("grey20", "grey80"), max.words=input$num, scale = c(1,0.7))
            
        }else if(input$sentiment %in% c("bing")){
            data_sentiment() %>%
                arrange(desc(n)) %>%
                top_n(input$n_rows, wt=n) %>%
                ggplot(aes(x=reorder(word, n), n, fill=sentiment)) + 
                geom_bar(stat = "identity") +
                xlab("Word") +
                ylab("Contribution to sentiment") + 
                coord_flip()
            
        }else if(input$sentiment %in% c("afinn")){
            data_sentiment() %>%
                mutate(n_new=as.integer(n*value)) %>%
                arrange(desc(n_new)) %>%
                top_n(input$n_rows, wt=n_new) %>%
                ggplot(aes(x=reorder(word, n_new), n_new, fill=value)) + 
                geom_bar(stat = "identity") + 
                xlab("Word") + 
                ylab("Strength of sentiment") + 
                coord_flip()
            
        }
    })
    
    output$p_wordcloud <- renderWordcloud2({
        data_wc <- data_source() %>% 
            count(word,sort=TRUE)
        create_wordcloud(data_wc, num_words = input$num)
    })
    
    output$p_tfidf <- DT::renderDataTable({
        data_bigrams <- data_original() %>%
            group_by(person) %>%
            unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
            separate(bigram, c("word1", "word2"), sep = " ") %>%
            filter(!word1 %in% stop_words$word) %>%
            filter(!word2 %in% stop_words$word) %>%
            count(word1, word2, sort = TRUE) %>%
            unite(bigram, word1, word2, sep=" ") %>%
            count(person, bigram) %>%
            bind_tf_idf(bigram, person, n) %>%
            arrange(desc(tf_idf))
    })
    
    output$p_tfidf_bigram <- renderPlotly({
        ggplotly({
            data_original() %>%
                group_by(person) %>%
                unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
                separate(bigram, c("word1", "word2"), sep = " ") %>%
                filter(!word1 %in% stop_words$word) %>%
                filter(!word2 %in% stop_words$word) %>%
                count(word1, word2, sort = TRUE) %>%
                unite(bigram, word1, word2, sep=" ") %>%
                count(person, bigram) %>%
                bind_tf_idf(bigram, person, n) %>%
                ungroup() %>%
                arrange(desc(tf_idf)) %>%
                top_n(input$n_rows, wt=tf_idf) %>%
                ggplot(aes(x=reorder(bigram, tf_idf), tf_idf, fill=tf_idf)) +
                geom_bar(stat = "identity") +
                xlab("Word") + 
                ylab("TF-IDF Value") + 
                coord_flip()
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
