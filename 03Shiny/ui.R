#ui.R 
# PROJECT 4

library(shiny)
require(shinydashboard)

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Project 6!"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    h3("CrossTab With Sets"),
    selectInput("inSet", "Filter Data By:",
                c("In Top Stores" = "top",
                  "Not In Stop Stores" = "not-top",
                  "All" = "all")),
    h3("Type of Store Sales Averages"),
    selectInput("year", "Filter Data Year:",
                c("2010" = "2010",
                  "2011" = "2011",
                  "2012" = "2012"))
    
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("crosstabPlot2"),
    plotOutput("tableCalc")
    
  )
))
