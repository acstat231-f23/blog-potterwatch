####################
# import libraries #
####################
library(tidyverse)
library(countrycode)
library(dplyr)
library(sf)
library(wordcloud)
library(tidytext)
library(ggplot2)
library(plotly)

###############
# import data #
###############

#SENTIMENT LEXICON
sentiment_data <- get_sentiments("afinn")

#MOVIES
movie1 <- read_csv("data/hp1.csv")
movie2 <- read_csv("data/hp2.csv")
movie3 <- read_csv("data/hp3.csv")
movie4 <- read_csv("data/hp4.csv")
movie5 <- read_csv("data/hp5.csv")
movie6 <- read_csv("data/hp6.csv")
movie7 <- read_csv("data/hp7.csv")
movie8 <- read_csv("data/hp8.csv")

movies_list <- list(movie1, movie2, movie3, movie4, movie5, movie6, movie7, movie8)

movies <- movies_list |>
  reduce(full_join)

movies_all_words <- movies |>
  unnest_tokens(output = word, input = dialog) |>
  mutate(word = tolower(word)) |>
  select(-character)

rated_words <- movies_all_words |> 
  inner_join(sentiment_data) |>
  group_by(movie, chapter) |>
  mutate(average_sentiment = mean(value)) |>
  select(-word, -value) |>
  distinct() |>
  group_by(movie) |>
  mutate(chapt_num = row_number())

movie_name_choices <- unique(rated_words$movie)

############
#    ui    #
############

ui <- fluidPage(
  title = "Harry Potter Sentiment Analysis",
  
  tabPanel(
    "Line Graph",
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "hp_choice",
                     label = "Choose the movies you would like displayed:",
                     choices = movie_name_choices,
                     selected = "Harry Potter and the Philosopher's Stone",
                     multiple = TRUE)
      ),
      mainPanel( 
        plotlyOutput(outputId = "linegraph")
      )
    )
  )
)

############
#  server  #
############
server <- function(input, output) {
  
  # Creates filtered data set based on user input
  output$linegraph <- renderPlotly({
    selected_movies <- rated_words %>%
      filter(movie %in% input$hp_choice)
    
    # Creates graphic
    p <- ggplot(selected_movies, aes(x = chapt_num, y = average_sentiment, color = movie,
                                     text = paste0('Movie: ', movie, '\nChapter ', chapt_num , ': "', chapter, '"\n Average Sentiment: ', round(average_sentiment, 3)), group = 1)) +
      geom_line(aes(color = movie)) + 
      geom_point() + 
      scale_color_manual(values = c("#AE0001", "#2A623D", "#00165e", "#e3a000", "#808080", "black", "#9e711e", "orange")) + #NEED TO CHANGE COLORS
      labs(x = "Chapter", y = "Average Sentiment", title = "Sentiment Analysis of the Harry Potter Film Series", color = "Movie")
    
    p <- ggplotly(tooltip = c("text"))
    
    return(p)
  })
}

####################
# call to shinyApp #
####################

shinyApp(ui = ui, server = server)
