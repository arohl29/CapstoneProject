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
# glimpse(tv_data)
timerange <- movie_data$runtime[order(movie_data$runtime)]
range1 <- timerange[0:10]
timerange <- tv_data$runtime[order(tv_data$episode_run_time3)]
range2 <- timerange[0:10]

# Define UI for application
ui <- fluidPage(title = "Recommendations",
  theme = bslib::bs_theme(version = 5, bootswatch = "quartz"),

  # Application title
  titlePanel(title= span(img(src="camerareel.png",height = 100,width =100),"Now Showing",align="center")),

  # Sidebar with different input button options
  sidebarLayout(position = "left",
    sidebarPanel(
      wellPanel(h1("Welcome!",align="center"),p("Please make your selection below:"),checkboxGroupInput("movieSeries", "Movies and/or Series?",
                           choices = c("Movies", "Series")),class = "btn-lg" # Welcome message (instructions?)
      ),
        # Options to look for movies, series, or both
        
        
        # Rating can be done as a slider that allows for range
       fluidRow(
       
        column(11,align="left",
      conditionalPanel(
      condition = "input.movieSeries == 'Movies'",
      checkboxGroupInput("MGenres", "Movies Genre Selection",
                           choices = c("Comedy", "Drama", "Science Fiction", "Action","Animation", "Thriller","Horror","Mystery","Adventure")),
      
      sliderInput("time", "Time available (in minutes)",
                  min = 15, max = 180, value = c(15, 105),
                  step = 15),
        
      sliderInput("popular", "Movie Popularity",
                  min = 0.0, max = 10.0, value = c(0.0, 10.0)),
      
      selectInput("MLanguage", "Spoken Language",
                         choices = c("en", "es", "fr", "ja"),selected = "en")
      
    ),
    conditionalPanel(
      condition = "input.movieSeries == 'Series'",
      checkboxGroupInput("SGenres", "Series Genre Selection",
                         choices = c("Comedy", "Drama", "Sci-Fi & Fantasy", "Action & Adventure", "Crime", "Mystery", "Kids")),
      sliderInput("time", "Time available (in minutes)",
                  min = 15, max = 180, value = c(15, 105),
                  step = 15),
      sliderInput("popular", "Series Popularity",
                  min = 0.0, max = 10.0, value = c(0.0, 10.0)),
      
      selectInput("SLanguage", "Spoken Language",
                         choices = c("en", "es", "fr", "ja"),selected = "en"))
    ),
      submitButton("Search", width= 215) # Need to center button (?)
      # actionButton() can be used to reset values?
    )), column(5, align="left",offset = 1,
    mainPanel(
      htmlOutput("tester")
    )
  )
)
)


# Define server logic required to draw a histogram
server <- function(input, output) {

  file = reactiveVal(NA)
  end_url = reactiveVal()
  output$welcome <- renderText("Welcome to our Movie/Series Recommendation System!
                     Please fill out the information below so that we can
                     give you recommendations based on your choices. Runtime for
                     series is given by episode runtime, so you might want a smaller
                     lower range for series!")
  

  # Depending on choice, process data frames (movies or series, or both)
  # Need a way to filter out adult movies. Some are not marked as such so we
  # need to find a pattern in a column, maybe genres being NA can be used
  # to filter those out
  output$tester <- renderText(
    if (length(input$movieSeries >= 0)) {
      
      if (input$movieSeries == "Series"){
      tv_data %>%
        filter(episode_run_time <= input$time[2] & episode_run_time > input$time[1]
               & vote_average <= input$popular[2] & vote_average > input$popular[1]
               ,adult == FALSE, genres.name %in% input$SGenres, original_language %in% input$SLanguage) %>% 
        arrange(episode_run_time) %>%
        mutate(name = paste0('<br> <img src="https://image.tmdb.org/t/p/w500',
                             poster_path,'" width=470> ',"<h2>",name,"</h2><br><h5>",episode_run_time,
                             " minutes","<br> Popularity: ",vote_average," Points <br>")) %>% 
        pull(name)
      
      
     }else if (input$movieSeries == "Movies") {
    
    movie_data %>%
      filter(runtime <= input$time[2] & runtime > input$time[1]
             & vote_average <= input$popular[2] & vote_average > input$popular[1]
             & adult == FALSE,genres.name %in% input$MGenres, original_language %in% input$MLanguage) %>%
      arrange(runtime) %>%
      mutate(title = paste0('<br><img src="https://image.tmdb.org/t/p/w500',
                            poster_path,'" width=470>',"<br>","<h2>",title,"</h2>","<br><h5>",runtime,
                            " minutes","<br> Popularity: ",vote_average," Points <br>")) %>%
      pull(title)
    } else { # This could be changed, having no options will still run something
    # Might put these as separate functions to avoid repeating code (if possible)
      tv_data %>%
      filter(episode_run_time <= input$time[2] & episode_run_time > input$time[1]
             & vote_average <= input$popular[2] & vote_average > input$popular[1]
             & adult == FALSE) %>%
      arrange(episode_run_time) %>%
      mutate(name = paste0('<br><img src="https://image.tmdb.org/t/p/w500',
                           poster_path,'" width=470> ',"<br><h2>",name,"</h2><br><h5>",episode_run_time,
                           " minutes","<br> Popularity: ",vote_average, " Points <br>")) %>%
      pull(name)

    movie_data %>%
      filter(runtime <= input$time[2] & runtime > input$time[1]
             & vote_average <= input$popular[2] & vote_average > input$popular[1]
             & adult == FALSE) %>%
      arrange(runtime) %>%
      mutate(title = paste0('<br><img src="https://image.tmdb.org/t/p/w500',
                            poster_path,'" width=470> <br><h2>',title,"</h2>","<br><h5>",runtime,
                            " minutes","<br> Popularity: ",vote_average," Points <br>")) %>%
      pull(title)
}   })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)