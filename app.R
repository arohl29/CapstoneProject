#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#loaded in libraries
library(shiny)
library(tidyverse)
library(bslib)

# Our Datasets for both tv and movies
movie_data <- read.csv("data/themoviedb-api-data.csv")
tv_data <- read.csv("data/themoviedb-tv-data.csv")

# Define UI for application
ui <- fluidPage(title = "Recommendations",
  theme = bslib::bs_theme(version = 5, bootswatch = "quartz"),

  # Application title
  titlePanel(title= span(img(src="camerareel.png",height = 100,width =100),
                         "Now Showing",align="center")),

  # Sidebar with different input button options
  sidebarLayout(position = "left",
    sidebarPanel( 
        # Options to look for movies, series, or both
      fluidRow(
        column(width = 12,align="center",
        wellPanel(h1("What Will You Watch Next?",align="center"),
                  h4("Please make your selection below:",style = "padding-top: -50px;"),
                  
                  # Radio Buttons for movie or series
                  radioButtons("movieSeries", h6("Movies or Series?",
                                                 style = "padding-top: -50px;"),
                           choices = c("Movies", "Series"),selected = NA),
                  # button to start search and produce results
                  submitButton("Search", width= 160), style = "display: block; 
                                                        text-align: center;")),
        column(11,align="left",
      conditionalPanel(condition = "input.movieSeries == 'Movies'",
  # Select genres preferred
      checkboxGroupInput("MGenres", "Movies Genre Selection",
                        choices = c("Comedy", "Drama", "Science Fiction","Action",
                          "Animation", "Thriller","Horror","Mystery","Adventure")),
  # Select how much time available, or preferred, to watch
  # This is a range
      sliderInput("time", "Time available (in minutes)",
                  min = 15, max = 180, value = c(15, 105),
                  step = 15),
  # Movie popularity rating
      sliderInput("popular", "Movie Popularity",
                  min = 0.0, max = 10.0, value = c(0.0, 10.0)),
  # Preferred language
      selectInput("MLanguage", "Language Selection",
                         choices = c("en", "es", "fr", "ja"),selected = "en")
      
    ),
    conditionalPanel(
      condition = "input.movieSeries == 'Series'",
  # Genres preferred for series
      checkboxGroupInput("SGenres", "Series Genre Selection",
                         choices = c("Comedy", "Drama", "Sci-Fi & Fantasy", 
                          "Action & Adventure", "Crime", "Mystery", "Kids")),
  # Time range available, or preferred, for watching
      sliderInput("time", "Time available (in minutes)",
                  min = 15, max = 180, value = c(15, 105),step = 15),
  # Series' popularity rating
      sliderInput("popular", "Series Popularity",
                  min = 0.0, max = 10.0, value = c(0.0, 10.0)),
  # Preferred language
      selectInput("SLanguage", "Language Selection",
                         choices = c("en", "es", "fr", "ja"),selected = "en"))
    )
    )), column(8,align="left",
    mainPanel(
      htmlOutput("tester")
    )
  )
)
)

# Sever logic to show results of movies or series based on selected input
# options, filtering data frames with those and creating a display
server <- function(input, output) {

  file = reactiveVal(NA)
  end_url = reactiveVal()

# Depending on choice, process data frames (movies or series, or both)
  output$tester <- renderText(
    if (length(input$movieSeries >= 0)) {
      
      if (input$movieSeries == "Series"){
      # This block runs the "Series" option is selected; what this does is use
      # the data frame tv_data and filter it so that it shows series based on the
      # time range, popularity range, and genres selected. It will then show the
      # results of that filter with the poster (if available) and their duration
      # popularity and a link button which will redirect to a webpage that gives
      # information of where to watch that series, if it exists in that database
      tv_data %>%
        filter(episode_run_time > input$time[1] & episode_run_time <= input$time[2]
               & vote_average > input$popular[1] & vote_average <= input$popular[2], 
               genres.name %in% input$SGenres, original_language %in% input$SLanguage) %>% 
          arrange(episode_run_time) %>%
          mutate(poster_path = ifelse(is.na(poster_path), '<img src="unavailable-image.jpg" width=470>',
                                      paste0('<img src="https://image.tmdb.org/t/p/w500',
                                             poster_path,'" width=470> ')),
                 ext_link = paste0('https://www.justwatch.com/us/tv-show/',
                                   gsub('\\s','\\-',tolower(name)))) %>%
          mutate(name = paste0('<table><tr><td align=center><h2>',name,
                '</h2></td></tr><tr><td style="padding:12px; margin: auto;">',
                poster_path,"</td><td width=120%><br><img src='timeclock.png' width=25> ",
                episode_run_time,'<br><img src="star.png" width=25> ',vote_average,
                " Points ",'<br><form action="',ext_link,
                '"Where to Watch" method="get" target="_blank">
                <button type="submit">Where to Watch</button></form></td></td></tr></table><br>')) %>%
        pull(name)
      
     }else if (input$movieSeries == "Movies") {
    # This block will run when the "Movies" option is selected, and will work in
    # in a similar manner as the series'
    movie_data %>%
      filter(runtime <= input$time[2] & runtime > input$time[1]
             & vote_average <= input$popular[2] & 
               vote_average > input$popular[1], genres.name1 %in% input$MGenres, 
             original_language %in% input$MLanguage, !is.na(backdrop_path)) %>% 
         arrange(runtime) %>%
         mutate(poster_path = ifelse(is.na(poster_path), '<img src="unavailable-image.jpg" width=470>',
                                     paste0('<img src="https://image.tmdb.org/t/p/w500',
                                            poster_path,'" width=470> ')),
                ext_link = paste0('https://www.justwatch.com/us/movie/',
                                  gsub('\\s','\\-',tolower(title)))) %>%
         mutate(title = paste0('<table><tr><td align=center><h2>',title,
                '</h2></td></tr><tr><td style="padding:12px; margin: auto;">',
                poster_path,"</td><td width=120%><img src='timeclock.png' width=25> ",
                runtime," Minutes",'<br><img src="star.png" width=25> ',vote_average,
                " Points ",'<br><form action="',ext_link,
                '"Where to Watch" method="get" target="_blank">
                <button type="submit">Where to Watch</button></form></td></tr></table><br>')) %>% 
      pull(title)
       }
      })
  
}

# Run the application 
shinyApp(ui = ui, server = server)