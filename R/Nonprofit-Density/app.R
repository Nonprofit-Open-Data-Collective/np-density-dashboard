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
library( shinythemes )
library( shinyWidgets )


# Import counties landing page map
main <- "/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Geospatial Dashboard/"

setwd( paste0( main, "np-density-dashboard/Data-Rodeo" ) )


cnties <- readRDS( "Dashboard-County-Data/USA-Counties.rds" )
cnties.dorling <- readRDS( "Dashboard-County-Data/Dorling-Shapefiles/USA-Counties-Dorling.rds" )

# merge

lp.plot.chloro <- function( df ){
  ggplot( ) + geom_sf( df,
                             mapping = aes( fill = dens ),
                             color = NA, size = 0.5 )+
  theme_minimal( ) +
  theme( legend.position = 'none' )
}


lp.plot.dorling <- function( df ){
  ggplot(  )  +
  geom_sf( df, mapping = aes( fill = dens ),  color = NA ) +
  theme_minimal()
}

# USER INTERFACE
ui <- bootstrapPage(
  
  navbarPage(theme = shinytheme( "flatly" ), collapsible = TRUE,
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">U.S. Nonprofit Density</a>'), 
             id="nav",
             tabPanel("New Nonprofit Cumulative Density, 2014-2021 : U.S. Counties",
                      
                      sidebarLayout(
                        sidebarPanel(
                          pickerInput("yr_select", "Year:",   
                                      choices = c("Cumulative: 2014-2021"), 
                                      selected = c("Cumulative: 2014-2021"),
                                      multiple = FALSE) ) ,
                      
                      mainPanel(
                        tabsetPanel(
                          tabPanel("Chloropleth", plotOutput("lp.1")))
                        )
                      )
             )
  )
)

  

# SERVER
server <- function( input, output ) { 
  
  
  
  year.reactive.df <- reactive( if(input$yr_select=="Cumulative: 2014-2021") {
    cnties
  }
  
  
  )

  output$lp.1 <- renderPlot( lp.plot.chloro( year.reactive.df() ) )
  

}


shinyApp( ui, server )

