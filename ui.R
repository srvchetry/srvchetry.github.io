## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)
library(dplyr)
library(tidyr)

source('functions/helpers.R')

genre.tab = tabItem(
  tabName = "genreTab",
  h2("Recommendations by Genre"),
  fluidRow(
    # box(width = 12, title = "Step 1: Select movies that you like for the chosen Genre", status = "info", solidHeader = TRUE, collapsible = TRUE,
    #     div(class = "rateitems",
    #         uiOutput('genre')
    box(
      width = 12,
      title = "Step 1: Select your favourite Genre:",
      status = "info",
      solidHeader = TRUE,
      collapsible = TRUE,
      selectInput(
        "selectedGenre",
        "Select any Genre:",
        choices = c(
          "Action",
          "Adventure",
          "Animation",
          "Children's",
          "Comedy",
          "Crime",
          "Documentary",
          "Drama",
          "Fantasy",
          "Film-Noir",
          "Horror",
          "Musical",
          "Mystery",
          "Romance",
          "Sci-Fi",
          "Thriller",
          "War",
          "Western"
        )
        
      )
    )
    
    
  ),
  fluidRow(
    useShinyjs(),
    box(
      width = 12,
      status = "info",
      solidHeader = TRUE,
      title = "Step 2: Discover movies you might like",
      br(),
      # withBusyIndicatorUI(
      #   actionButton("btn1", "Click here to get your recommendations", class = "btn-warning")
      # ),
      # br(),
      tableOutput("genreresults")
    )
  )
)

rating.tab = tabItem(
  tabName = "ratingsTab",
  h2("Recommendations by Ratings"),
  fluidRow(
    box(
      width = 12,
      title = "Step 1: Rate as many movies as possible",
      status = "info",
      solidHeader = TRUE,
      collapsible = TRUE,
      div(class = "rateitems",
          uiOutput('ratings'))
    )
  ),
  fluidRow(
    useShinyjs(),
    box(
      width = 12,
      status = "info",
      solidHeader = TRUE,
      title = "Step 2: Discover movies you might like",
      br(),
      withBusyIndicatorUI(
        actionButton("btn2", "Click here to get your recommendations", class = "btn-warning")
      ),
      br(),
      tableOutput("ratingresults")
    )
  )
)



shinyUI(dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Movie Recommendations", titleWidth = 280),
  
  dashboardSidebar(width = 280,
                   sidebarMenu(
                     menuItem("By User Ratings", tabName = "ratingsTab", icon = icon("film")),
                     menuItem("By Genre", tabName = "genreTab", icon = icon("star"))
                     
                   )),
  
  dashboardBody(
    includeCSS("css/movies.css"),
    tabItems(rating.tab,
             genre.tab),
  )
))


