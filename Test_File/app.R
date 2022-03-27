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

#glimpse(movie_data)
#glimpse(tv_data)
timerange <- movie_data$runtime[order(movie_data$runtime)]
range1 <- timerange[0:10]
timerange <- tv_data$runtime[order(tv_data$episode_run_time3)]
range2 <- timerange[0:10]

# Define UI for application
ui <- fluidPage(title = "Recommendations",
  theme = bslib::bs_theme(version = 5, bootswatch = "quartz"),

  # Application title
  titlePanel("Now Showing"),

  # Sidebar with different input button options
  sidebarLayout(position = "left",
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
        # textInput("actor", "Actor in Movie/Series (Optional)",
        #           placeholder = "Enter actor"),
        
        # Can be multiplied when processing
        # Time in data is in minutes (double)
        sliderInput("time", "Time available (in minutes)",
                    min = 15, max = 180, value = c(15, 180),
                    step = 15),
        
        # checkboxGroupInput("ratings", "Age Rating",
        #                     choices = c("G", "PG", "PG-13", "R")),
        
        # Rating can be done as a slider that allows for range
        sliderInput("popular", "Movie/Series Rating",
                    min = 0.0, max = 10.0, value = c(0.0, 10.0))
      ),
      submitButton("Search!") # Need to center button (?)
      # actionButton() can be used to reset values?
    ),
      
    mainPanel(
      textOutput("movieSeries"),
      htmlOutput("tester")
    )
  )
)

url_start <- "https://image.tmdb.org/t/p/w500"

# This function will take whichever radio option is used and
# based on that go through either or both data files to get
# the titles of the movies and/or series
# data_search <- function(movie_or_series) {
#   file = NA
#   if (length(movie_or_series) == 1) {
#     if ("movies" %in% movie_or_series){
#       renderText("Looking for movies.")
#       file = movie_data
#     }
#     if ("series" %in% movie_or_series) {
#       renderText("Looking for series.")
#       file = tv_data
#     } 
#   } else {
#     # Process both files for series and movies
#   }
  
  # Need to check how to pass app elements (vectors) as parameters in functions
  # if ((file$vote_average %in% ratings) && (file$runtime %in% time*60)) {
  #   renderText(paste(file$title), sep = "\n")
  # }
  
#}

# Define server logic required to draw a histogram
server <- function(input, output) {

  file = reactiveVal(NA)
  end_url = reactiveVal()
  
  output$welcome <- renderText("Welcome to our Movie/Series Recommendation System!
                     Please fill out the information below so that we can
                     give you recommendations based on your choices. Runtime for
                     series is given by episode runtime, so you might want a smaller
                     lower range for series!")
  
  # observeEvent(input$movieSeries,{
  #   if (length(input$movieSeries == 1)){
  #     if (input$movieSeries == "Movies"){
  #       file(movie_data)
  #       id_only <- movie_data %>% 
  #         select(id,poster_path) %>% 
  #         group_by(id)
  #       path1 = id_only$poster_path[2]
  #       post_url = paste0("https://image.tmdb.org/t/p/w500",path1)
  #       end_url(post_url)
  #     }
  #     else if (input$movieSeries == "Series") {
  #       file(tv_data)
  #       id_only <- tv_data %>% 
  #         select(id,poster_path) %>% 
  #         group_by(id)
  #       path1 = id_only$poster_path[2]
  #       post_url = paste0("https://image.tmdb.org/t/p/w500",path1)
  #       end_url(post_url)
  #     } else {
  #       paste("sad day")
  #       # Process both files for series and movies
  #     }
  #   }
  #   output$movieSeries <- renderText(paste(end_url()))
  # 
  #   output$tester <- renderText({c('<img src="',end_url(),'">')
  #   })
  # })

  # Depending on choice, process data frames (movies or series, or both)
  # Need a way to filter out adult movies. Some are not marked as such so we
  # need to find a pattern in a column, maybe genres being NA can be used
  # to filter those out
  output$tester <- renderText(if (input$movieSeries == "Series"){
    
    tv_data %>%
      filter(episode_run_time <= input$time[2] & episode_run_time > input$time[1]
             & vote_average <= input$popular[2] & vote_average > input$popular[1]
             & adult == FALSE) %>%
      arrange(episode_run_time) %>%
      mutate(name = paste0('<img src="https://image.tmdb.org/t/p/w500',
                           poster_path,'" width=100> ',name," -",episode_run_time,
                           " minutes, Popularity: ", vote_average,"<br>")) %>%
      pull(name)
    
  } else if (input$movieSeries == "Movies") {
    
    movie_data %>%
      filter(runtime <= input$time[2] & runtime > input$time[1]
             & vote_average <= input$popular[2] & vote_average > input$popular[1]
             & adult == FALSE) %>%
      arrange(runtime) %>%
      mutate(title = paste0('<img src="https://image.tmdb.org/t/p/w500',
                            poster_path,'" width=100> ',title," -",runtime,
                            " minutes, Popularity: ", vote_average,"<br>")) %>%
      pull(title)
    
  } else { # This could be changed, having no options will still run something
    # Might put these as separate functions to avoid repeating code (if possible)
    tv_data %>%
      filter(episode_run_time <= input$time[2] & episode_run_time > input$time[1]
             & vote_average <= input$popular[2] & vote_average > input$popular[1]
             & adult == FALSE) %>%
      arrange(episode_run_time) %>%
      mutate(name = paste0('<img src="https://image.tmdb.org/t/p/w500',
                           poster_path,'" width=100> ',name," -",episode_run_time,
                           " minutes, Popularity: ", vote_average,"<br>")) %>%
      pull(name)
    
    movie_data %>%
      filter(runtime <= input$time[2] & runtime > input$time[1]
             & vote_average <= input$popular[2] & vote_average > input$popular[1]
             & adult == FALSE) %>%
      arrange(runtime) %>%
      mutate(title = paste0('<img src="https://image.tmdb.org/t/p/w500',
                            poster_path,'" width=100> ',title," -",runtime,
                            " minutes, Popularity: ", vote_average,"<br>")) %>%
      pull(title)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)