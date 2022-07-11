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


# Import counties landing page map
main <- "/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Geospatial Dashboard/"

setwd( paste0( main, "np-density-dashboard/Data-Rodeo" ) )


cnties <- readRDS( "Dashboard-County-Data/USA-Counties.rds" )
cnties.dorling <- readRDS( "Dashboard-County-Data/Dorling-Shapefiles/USA-Counties-Dorling.rds" )

# merge

lp.1 <- ggplot( ) + geom_sf( cnties,
                     mapping = aes( fill = dens ),
                     color = NA, size = 0.5 )+
  theme_minimal( ) +
theme( legend.position = 'none' )

  

lp.2 <- ggplot(  )  +
  geom_sf( cnties.dorling, mapping = aes( fill = dens ),  color = NA ) +
  theme_minimal()
  
ui <- bootstrapPage(
  
    navbarPage(theme = shinytheme( "flatly" ), collapsible = TRUE,
               HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">U.S. Nonprofit Density</a>'), 
               id="nav",
               tabPanel("New Nonprofit Cumulative Density, 2014-2021 : U.S. Counties",
                        
                          
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Chloropleth", plotOutput("lp.1")),
                              tabPanel("Dorling", plotOutput("lp.2") )
                            )
                          )
               )
    )
)

                                         



server <- function( input, output ) { 
  
  output$lp.1 <- renderPlot( lp.1 )
  
  output$lp.2 <- renderPlot( lp.2 )
  
  
  }

shinyApp( ui, server )

