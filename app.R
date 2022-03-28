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

#glimpse(movie_data$poster_path)
#glimpse(tv_data)
timerange <- movie_data$runtime[order(movie_data$runtime)]
range1 <- timerange[0:10]
timerange <- tv_data$runtime[order(tv_data$episode_run_time3)]
range2 <- timerange


# Define UI for application
ui <- fluidPage(
  title = "Recommendations",
  theme = bslib::bs_theme(version = 5,bootswatch = "quartz"),
  # Application title
  titlePanel("Watch Recommendations"),

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
        textInput("actor", "Actor in Movie/Series (Optional)",
                  placeholder = "Enter actor"),
        
        # Can be multiplied when processing
        # Time in data is in minutes (double)
        sliderInput("time", "Time available (in hours)",
                    min = 1, max = 10, value = c(1, 10),step = .5), # maybe drop-down list, or
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
      textOutput ("movieSeries"),
      htmlOutput("tester"),
      textOutput("person"),
      htmlOutput("time"),
      textOutput("rating"),
      textOutput("popular"),
      textOutput("path")
    )
  )
)

# This function will take whichever radio option is used and
# based on that go through either or both data files to get
# the titles of the movies and/or series
url_start <- "https://image.tmdb.org/t/p/w500"


  # Need to check how to pass app elements (vectors) as parameters in functions
  #if ((file$vote_average %in% ratings) && (file$runtime %in% time*60)) {
    #renderText(paste(file$title), sep = "\n")
 # }




# Define server logic required to draw a histogram
server <- function(input, output) {
  

  file = reactiveVal(NA)
  end_url = reactiveVal()
  time = reactiveVal(0)

  
  output$path <- renderText(if(movie_data$id == 1){
    paste(movie_data$id)
  })
  
 
    
  

  output$welcome <- renderText("Welcome to our Movie/Series Recommendation System!
                     Please fill out the information below so that we can
                     give you recommendations based on your choices. Some
                     of the choices are optional, such as the Actor Name,
                     and your time available which will give you movies
                     or series of any duration length.")
  
  #activateApp <- eventReactive()
  
  # Depending on choice, process data frames (movies or series, or both)
  # Call data processing function
  
  observeEvent(input$movieSeries,{
    if (length(input$movieSeries == 1)){
      if (input$movieSeries == "Movies"){
        file(movie_data)
        id_only <- movie_data %>% 
          select(id,poster_path) %>% 
          group_by(id)
        path1 = id_only$poster_path[2]
        post_url = paste0("https://image.tmdb.org/t/p/w500",path1)
        end_url(post_url)
         
        
      }
      else if (input$movieSeries == "Series") {
        file(tv_data)
      id_only <- tv_data %>% 
        select(id,poster_path) %>% 
        group_by(id)
      path1 = id_only$poster_path[2]
      post_url = paste0("https://image.tmdb.org/t/p/w500",path1)
      end_url(post_url)
      
      
      
    } else {
      paste("sad day")
      # Process both files for series and movies
    }
    }
  
  output$movieSeries <- renderText(paste(end_url()))
  # Might need to make ui elements as global to be able to use them
  # (if it is possible)
 
  
  
  
  output$tester <- renderText({c('<img src="',end_url(),'">')
   })
  

  
  
  
  output$time <- renderText(
    if (input$time == 0){
    paste(range1, "\n", range2)}
      else {
        tv_data %>%
          filter(episode_run_time <= input$time[2] & episode_run_time < input$time[1]) %>% 
          arrange(episode_run_time) %>%
          mutate(name = paste(name,"-",episode_run_time,"<br>")) %>%
          pull(name)
      })
  
  output$rating <- renderText(input$ratings) # Vector of choices
                              #columnName %in% input$ratings -> do something
  
  #output$person <- renderText(paste("Looking for: ",input$actor))
  
  output$popular <- renderText(if (input$popular == '1'){
    paste(movie_data$popularity)
  })
  }
# Run the application 
shinyApp(ui = ui, server = server)