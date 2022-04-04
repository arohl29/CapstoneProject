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
        # Options to look for movies, series, or both
        
        
        # Rating can be done as a slider that allows for range
      fluidRow(
        column(width = 12,align="center",
        wellPanel(h1("What Will You Watch Next?",align="center"),
                  h4("Please make your selection below:",style = "padding-top: -50px;"),
                  radioButtons("movieSeries", h6("Movies or Series?",style = "padding-top: -50px;"),
                           choices = c("Movies", "Series"),selected = NA),submitButton("Search", width= 160), style = "display: block; text-align: center;")),
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
      
      selectInput("MLanguage", "Language Selection",
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
          mutate(poster_path = ifelse(is.na(poster_path), '<img src="unavailable-image.jpg" width=470>',
                                      paste0('<img src="https://image.tmdb.org/t/p/w500',
                                             poster_path,'" width=470> ')),
                 ext_link = paste0('https://www.justwatch.com/us/tv-show/',gsub('\\s','\\-',tolower(name)))) %>%
          mutate(name = paste0('<table><tr><td align=center><h2>',name,'</h2></td></tr><tr><td style="padding:12px; margin: auto;">',poster_path,"</td><td width=120%><br><img src='timeclock.png' width=25> ",episode_run_time,
                            '<br><img src="star.png" width=25> ',vote_average, " Points ",'<br><a href="',ext_link,'">Where to Watch</a></td></tr></table><br>')) %>%
        pull(name)
      
      
     }else if (input$movieSeries == "Movies") {
    
    movie_data %>%
      filter(runtime <= input$time[2] & runtime > input$time[1]
             & vote_average <= input$popular[2] & vote_average > input$popular[1]
             & adult == FALSE,genres.name %in% input$MGenres, original_language %in% input$MLanguage) %>%
      arrange(runtime) %>%
         mutate(poster_path = ifelse(is.na(poster_path), '<img src="unavailable-image.jpg" width=470>',
                                     paste0('<img src="https://image.tmdb.org/t/p/w500',
                                            poster_path,'" width=470> ')),
                ext_link = paste0('https://www.justwatch.com/us/movie/',gsub('\\s','\\-',tolower(title)))) %>%
         mutate(title = paste0('<table><tr><td align=center><h2>',title,'</h2></td></tr><tr><td style="padding:12px; margin: auto;">',poster_path,"</td><td width=120%><img src='timeclock.png' width=25> ",runtime,
                              " Minutes",'<br><img src="star.png" width=25> ',vote_average, " Points ",'<br><a href="',ext_link,'">Where to Watch</a></td></tr></table><br>')) %>% 
      pull(title)
    } else { # This could be changed, having no options will still run something
    # Might put these as separate functions to avoid repeating code (if possible)
      tv_data %>%
      filter(episode_run_time <= input$time[2] & episode_run_time > input$time[1]
             & vote_average <= input$popular[2] & vote_average > input$popular[1]
             & adult == FALSE) %>%
      arrange(episode_run_time) %>% 
        mutate(poster_path = ifelse(is.na(poster_path), '<img src="unavailable_image.jpg">',
                                    paste0('<img src="https://image.tmdb.org/t/p/w500',
                                      poster_path,'" width=470> ')),
        ext_link = paste0('https://www.justwatch.com/us/movie/',gsub('\\s','\\-',tolower(name)))) %>%
    mutate(name = paste0('<table><tr><td align=center><h2>',name,'</h2></td></tr><tr><td style="padding:12px; margin: auto;">',poster_path,"</td><td width=120%><img src='timeclock.png' width=25> ",episode_run_time,
                          " Minutes",'<br><img src="star.png" width=25> ',vote_average, " Points ",'<br><a href="',ext_link,'">Where to Watch</a></td></tr></table><br>')) %>%
      pull(name)

      movie_data %>%
        filter(runtime <= input$time[2] & runtime > input$time[1]
               & vote_average <= input$popular[2] & vote_average > input$popular[1]
               & adult == FALSE,genres.name %in% input$MGenres, original_language %in% input$MLanguage) %>%
        arrange(runtime) %>%
        mutate(poster_path = ifelse(is.na(poster_path), '<img src="unavailable-image.jpg" width=470>',
                                    paste0('<img src="https://image.tmdb.org/t/p/w500',
                                           poster_path,'" width=470> ')),
               ext_link = paste0('https://www.justwatch.com/us/movie/',gsub('\\s','\\-',tolower(title)))) %>%
        mutate(title = paste0('<table><tr><td align=center><h2>',title,'</h2></td></tr><tr><td style="padding:12px; margin: auto;">',poster_path,"</td><td width=120%><img src='timeclock.png' width=25> ",runtime,
                              " Minutes",'<br><img src="star.png" width=25> ',vote_average, " Points ",'<br><a href="',ext_link,'">Where to Watch</a></td></tr></table><br>')) %>% 
        pull(title)
}   })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)