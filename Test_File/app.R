#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(bslib)

# Select only columns from data file that we need
movie_data <- read.csv("data/themoviedb-api-data.csv")
tv_data <- read.csv("data/themoviedb-tv-data.csv")

glimpse(movie_data)
glimpse(tv_data)
timerange <- movie_data$runtime[order(movie_data$runtime)]
range1 <- timerange[0:10]
timerange <- tv_data$runtime[order(tv_data$runtime)]
range2 <- timerange[0:10]

# Define UI for application
ui <- fluidPage(title = "Recommendations",
  theme = bslib::bs_theme(bootswatch = "solar"),

  # Application title
  titlePanel("Watch Recommendations"),

  # Sidebar with different input button options
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        textOutput("welcome"), class = "btn-lg" # Welcome message (instructions?)
      ),
      inputPanel(
        
        # Options to look for movies, series, or both
        checkboxGroupInput("movieSeries", "Movies and/or Series?",
                           choices = c("Movies", "Series")),
        
        # Option to look for an actor/actress
        # Might need to remove this option (current API doesn't have this info)
        textInput("actor", "Actor in Movie/Series (Optional)",
                  placeholder = "Enter actor"),
        
        # Can be multiplied when processing
        # Time in data is in minutes (double)
        sliderInput("time", "Time available (in hours)",
                    min = 0, max = 120, value = c(0, 120)), # maybe drop-down list, or
                                                    # slider?
        
        checkboxGroupInput("ratings", "Age Rating",
                            choices = c("G", "PG", "PG-13", "R")),
        
        # Rating can be done as a slider that allows for range
        sliderInput("popular", "Movie/Series Rating",
                    min = 0.0, max = 10.0, value = c(0.0, 10.0))
      ),
      submitButton("Search!") # Need to center button (?)
      # actionButton() can be used to reset values?
    ),
      
    mainPanel(
      textOutput("test"),
      textOutput("person"),
      textOutput("time"),
      textOutput("rating"),
      textOutput("popular")
    )
  )
)

# This function will take whichever radio option is used and
# based on that go through either or both data files to get
# the titles of the movies and/or series
data_search <- function(movie_or_series) {
  file = NA
  if (length(movie_or_series) == 1) {
    if ("movies" %in% movie_or_series){
      renderText("Looking for movies.")
      file = movie_data
    }
    if ("series" %in% movie_or_series) {
      renderText("Looking for series.")
      file = tv_data
    } 
  } else {
    # Process both files for series and movies
  }
  
  if ((file$vote_average %in% ratings) && (file$runtime %in% time*60)) {
    renderText(paste(file$title), sep = "\n")
  }
  
}

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$welcome <- renderText("Welcome to our Movie/Series Recommendation System!
                     Please fill out the information below so that we can
                     give you recommendations based on your choices. Some
                     of the choices are optional, such as the Actor Name,
                     and your time available which will give you movies
                     or series of any duration length.")
  
  #activateApp <- eventReactive()
  
  # Depending on choice, process data frames (movies or series, or both)
  # Call data processing function
  output$test <- data_search("movies")
  # Might need to make ui elements as global to be able to use them
  # (if it is possible)
  
  output$time <- renderText(if (input$time == 0){
    paste(range1, "\n", range2)
  })
  
  output$rating <- renderText(input$ratings) # Vector of choices
                              #columnName %in% input$ratings -> do something
  
  #output$person <- renderText(paste("Looking for: ",input$actor))
  
  #output$popular <- renderText(if (input$popular == '1'){
  #  paste(data$popularity)
  #})

}

# Run the application 
shinyApp(ui = ui, server = server)