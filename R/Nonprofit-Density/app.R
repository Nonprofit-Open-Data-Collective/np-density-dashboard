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

## Cumulative county data
cnties <- readRDS( "Dashboard-County-Data/USA-Counties.rds" )

## Yearly County Data
setwd("Dashboard-County-Data/By-Year")

# read-in yearly data
for (i in 1:length( dir( ) ) ) {
  
  ct.yr <- paste0( 'cnties', 2014:2021 )
  
  assign( ct.yr[ i ], readRDS( dir()[i] ) )
  
  
}


setwd( paste0( main, "np-density-dashboard/Data-Rodeo" ) )
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
                                      choices = c("Cumulative: 2014-2021", "2014", "2015", "2016", "2017", "2018", 
                                                  "2019", "2020", "2021" ), 
                                      selected = c("Cumulative: 2014-2021" ),
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
  
  
  
  year.reactive.df <- reactive(
    {
    if(input$yr_select=="Cumulative: 2014-2021") {
    cnties
  }
      else if(input$yr_select=="2014") {
    cnties2014
  }
      else if(input$yr_select=="2015") {
      cnties2015
    }
      else if(input$yr_select=="2016") {
      cnties2016
    }
      else  if(input$yr_select=="2017") {
      cnties2017
    }
      else   if(input$yr_select=="2018") {
      cnties2018
    }
      else   if(input$yr_select=="2019") {
      cnties2019
    }
      else    if(input$yr_select=="2020") {
      cnties2020
    }
      else  if(input$yr_select=="2021") {
      cnties2021
    }
  }
  
  )

  output$lp.1 <- renderPlot( lp.plot.chloro( year.reactive.df() ) )
  

}


shinyApp( ui, server )

