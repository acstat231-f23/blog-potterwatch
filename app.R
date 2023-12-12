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
rated_words <- readRDS("shiny_rated_data.rds") #Reads in wrangled data set

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
      filter(movie %in% input$hp_choice)  #Creates smaller data set based on users choices
    
    movie_colors <- c(
      "Harry Potter and the Philosopher's Stone" = "#AE0001",
      "Harry Potter and the Chamber of Secrets" = "#2A623D",
      "Harry Potter and the Prisoner of Azkaban" = "#00165e",
      "Harry Potter and the Goblet of Fire" = "#e3a000",
      "Harry Potter and the Order of the Phoenix" = "#808080",
      "Harry Potter and the Half-Blood Prince" = "black",
      "Harry Potter and the Deathly Hallows Part 1" = "#9e711e",
      "Harry Potter and the Deathly Hallows Part 2" = "royalblue3"
    )
    
    # Creates graphic
    p <- ggplot(selected_movies, aes(x = chapt_num, y = average_sentiment, color = movie,
                                     text = paste0('Movie: ', movie, '\nChapter ', chapt_num , ': "', chapter, '"\n Average Sentiment: ', round(average_sentiment, 3)), group = 1)) +
      geom_line(aes(color = movie)) + 
      geom_point() +
      scale_color_manual(values = movie_colors) + #uses harry potter themed color palette
      labs(x = "Chapter", y = "Average Sentiment", title = "Sentiment Analysis of the Harry Potter Film Series", color = "Movie")
    
    p <- ggplotly(tooltip = c("text")) #adds hover tool to shiny app based on text in ggplot function
    
    return(p)
  })
}

####################
# call to shinyApp #
####################

shinyApp(ui = ui, server = server)
