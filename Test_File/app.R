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

# Define UI for application
ui <- fluidPage(title = "Recommendations",
  theme = bslib::bs_theme(bootswatch = "solar"),

  # Application title
  titlePanel("Watch Recommendations"),

  # Sidebar with different input button options
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        textOutput("welcome"), class = "btn-lg" # Welcome message (might be info?)
      ),
      inputPanel(
        #textOutput("movieSeries"),
        #checkboxInput("movies", "Movies?"),
        #checkboxInput("series", "Series?"),
        
        #Options to look for movies, series, or both
        radioButtons("movieSeries", "Movies and/or Series?",
                     c("Movies" = "movies", "Series" = "series",
                       "Both" = "both")),
        #Option to look for an actor/actress
        textInput("actor", "Actor in Movie/Series (Optional)",
                  placeholder = "Enter actor"),
        
        numericInput("time", "Time available (in hours)", 0,
                    min = 0, max = 10), # maybe drop-down list, or slider?
        
        checkboxGroupInput("ratings", "Movie Rating",
                            choices = c("G", "PG", "PG-13", "R"))
      ),
      submitButton("Search!")
      # actionButton() can be used to reset values?
    ),
      
    mainPanel(
      textOutput("test"),
      textOutput("person")
    )
  )
    
        # Show a plot of the generated distribution
        # mainPanel(
        #    plotOutput("distPlot")
        # )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$welcome <- renderText("Welcome to our Movie/Series Recommendation System!
                     Please fill out the information below so that we can
                     give you recommendations based on your choices. Some
                     of the choices are optional, such as the Actor Name,
                     and your time available which will give you movies
                     or series of any duration length.")
  
  #activateApp <- eventReactive()
  
  output$test <- renderText(switch (input$movieSeries,
                                  movies = "Looking for movies",
                                  series = "Looking for series",
                                  both = "Looking for both"))
  
  output$person <- renderText(paste("Looking for: ",input$actor))
  
    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
