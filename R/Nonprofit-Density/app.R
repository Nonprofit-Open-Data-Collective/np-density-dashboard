#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library( shiny )
library( tidyverse )
library( plotly )
library( bslib )
library( shinydashboard )
library( sf )

ui <- dashboardPage(
  
  dashboardHeader(
    title = "Nonprofit Density"
  ),
  
  dashboardSidebar(),
  
  dashboardBody(
    
    fluidRow(
      box( plotOutput("plot1", height = 250 ) ),
      
      box(
        title = "Controls",
        sliderInput( "slider", "Number of observations:", 1, 100, 50 ) )
    )
  )
)


server <- function( input, output ) { 
  }

shinyApp( ui, server )

