library(shiny)
library(tidytext)
library(tidyverse)
library(wordcloud)
library(ggplot2)
library(shinythemes)
library(RColorBrewer)
# library(rsconnect)

# books
books <- list("A Mid Summer Night's Dream" = "summer",
              "The Merchant of Venice" = "merchant",
              "Romeo and Juliet" = "romeo")

# Task 4
getFreq <- function(book, stopwords = TRUE) {
  # check that only one of three books is selected
  if (!(book %in% books))
    stop("Unknown book")
  
  text <-  tibble(text = readLines(sprintf("./data/%s.txt", book), encoding="UTF-8"))
  
  # could also pass column of text/character instead
  text <- text %>%
    unnest_tokens(word, text) %>%
    count(word, sort = TRUE) 
  
  if(stopwords){
    text <- text %>%
      anti_join(stop_words)
  }
  
  return(text)
}

# Task 6

ui <- fluidPage(
  theme = shinytheme("cyborg"),
  titlePanel("Shakespeare's Plays Word Frequencies"), # Application title
  # task1: add in the sidebarLayout with sidebarPanel and mainPanel
  sidebarLayout(
    position = "left",
    sidebarPanel(
      # task2: add in the inputs in the sidebarPanel
      selectInput("book","Choose a Book",books),
      checkboxInput("stopwords", "stopwords" ,value = TRUE),
      actionButton("run", "Rerun"),
      h3("Word Cloud Settings"),
      sliderInput("maxwords", "Max # of Words", min = 10, max = 200, value = 100, step = 10),
      sliderInput("large_word", "Size of largest words:", min = 1, max = 8, value = 4),
      sliderInput("small_word", "Size of smallest words:", min = .1, max = 4, value = .5),
      hr(),
      h3("Word Count Settings"),
      sliderInput("min_word_count", "Minimum words for Counts Chart", min = 10, max = 100, value = 25),
      sliderInput("word_size", "Word size for Counts Chart", min = 8, max = 30, value = 14)
    ),
    mainPanel(
      tabsetPanel(
        # Task 1
        type = "tabs",
        tabPanel("Word Cloud", plotOutput("cloud"), height = "600px"),
        tabPanel("Word Counts", plotOutput("freq", height = "600px"))
      )
    )
  )
  

)

server <- function(input, output) {
  
  # Task 5
  freq = eventReactive(
    input$run,
    {
      withProgress({
        setProgress(message = "Processing corpus...")
        getFreq(input$book, input$stopwords)})
    }
  )
  
  output$cloud = renderPlot({
    v <- freq()
    pal <- brewer.pal(8,"Dark2")
    v %>% 
      with(
        wordcloud(
          word, 
          n, 
          scale = c(input$large_word, input$small_word),
          random.order = FALSE, 
          max.words = input$maxwords, 
          colors=pal))
  })
  
  output$freq = renderPlot({
    v <- freq()
    v %>%
      filter(n > input$min_word_count) %>%
      ggplot(aes(x = reorder(word, n), y = n)) +
      geom_col() +
      coord_flip() +
      theme(text = element_text(size=input$word_size)) +
      labs(x = '', y = '')
    
  })
  
}

shinyApp(ui = ui, server = server)