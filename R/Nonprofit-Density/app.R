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
library( RColorBrewer )
library( urbnthemes )
library( leaflet )

source('/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Geospatial Dashboard/np-density-dashboard/R/helpers.R')

# Render png to jpeg

# Import counties landing page map
main <- "/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Geospatial Dashboard/"

setwd( paste0( main, "np-density-dashboard/Data-Rodeo" ) )

## Cumulative county data
cnties <- readRDS( "Dashboard-County-Data/USA-Counties.rds" )

## Yearly County Data/shapefile
setwd("Dashboard-County-Data/By-Year")

## Read-in yearly data/shapefiles for county chloropleths
for (i in 1:length( dir( ) ) ) {
  
  ct.yr <- paste0( 'cnties', 2014:2021 )
  
  assign( ct.yr[ i ], readRDS( dir()[i] ) )
  
  
}

## Leaflet county data/shapefile
setwd( paste0( main,"np-density-dashboard/Data-Rodeo/Dashboard-County-Data/Leaflet-Shapefiles" ) )

cnty.leaf <- readRDS( "USA-Counties-Leaflet.rds" )

## Read-in yearly data for county Leaflets
setwd( "Leaflet-By-Year" )

for (i in 1:length( dir( ) ) ) {
  
  ct.yr.lf <- paste0( 'cnty.leaf.', 2014:2021 )
  
  assign( ct.yr.lf[ i ], readRDS( dir()[i] ) )
  
  
}


## Read in cumulative county Dorling Cartogram data/shapefile

setwd( paste0( main, "np-density-dashboard/Data-Rodeo" ) )
cnties.dorling <- readRDS( "Dashboard-County-Data/Dorling-Shapefiles/USA-Counties-Dorling.rds" )


### Plotting Functions ###

# landing page chloropleths
lp.plot.chloro <- function( df ){
  ggplot( ) + geom_sf( df,
                             mapping = aes( fill = dens.q ),
                             color = NA, size = 0.5 ) +
    scale_fill_brewer( "Quantile", palette = 1 ) +
    theme_minimal( ) +
    theme( text = element_text( family = "Helvetica Light" ) )
}

# landing page Dorling Cartograms
lp.plot.dorling <- function( df ){
  
  df$dens.q <- factor( quant.cut( var = 'dens', x = 7 ,df = df ) )
    
  ggplot(  )  +
    geom_sf( df, mapping = aes( fill = dens.q ),  color = NA ) +
    scale_fill_brewer( "Quantile", palette = 1 ) +
    theme_minimal( ) +
    theme( text = element_text( family = "Helvetica Light" ) )
}

# landing page interactiveLeaflet

lp.plot.leaf <- function( df ){
  
  leaflet(df) %>% 
    addTiles() %>%
    addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.5, popup = ~as.character(popup) ) %>%
    setView(-98.35, 39.50, zoom = 3.499)
  
}

# USER INTERFACE
ui <- bootstrapPage(
  navbarPage(theme = shinytheme( "flatly" ), collapsible = TRUE,
             
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">U.S. Nonprofit Density</a>'), 
             id="nav",
             tabPanel("New Nonprofit Density, 2014-2021: U.S. Counties",
                      div(class="outer",
                          tags$head(includeCSS("/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Geospatial Dashboard/np-density-dashboard/R/Nonprofit-Density/styles.css")),
                      
                      sidebarLayout(
                        sidebarPanel(
                          span( div( img(src="https://lh3.googleusercontent.com/k-BWWGuT_48sXsGlq0mwVDWbo0nFO7me3F4sVcAeJjB-mWsYJlhVm4HEO5iL1moxc5hKYwG6ZYCPxVw9FxLf0vuOSQN2rq3DGEgjdvJMMHaiWnZMqOouKaZiRd8qQHJn3vEn7LPQUL3ZziwRsboOcsGiRLevWAIULQ_4FtENb_e6gmBKAvciUz9-CR0RCISXWtRrbP3yRXwes64LmfYoPAZrrXb0pSReSLbkLAa5hAWmeuFONK25CticLz3Q3FzTSO7ovW342EuU3KwXkjTyMx-6NuZ2TBzLltA9Ott2X_RJUNUbvfXub58i_C6_MXb70RpFZJETHkiDxOdD2p7-cFFZA7owQJrMIOIr-Q_cDDqfbYgfxN4ahn6fU0MtCrcE9cz8xeKBkj4jRVgUdnuI8IpvxW-eQABhC9pE0yfSUMTimAeNYx860x4I51uYO_Fspn7VTn0hhV-FnLSz7YtjVEVyPGE4o89bWQ6IR4-Ls0VAAUhwZlm8-HjMuZQgXhNwjMwd-SKYKaM7F0MKLQ-kd74Sz01ZMy1b4ZYl3_88dPYQeAWFuFtsO3WE5deftuI-6qaqu7cKyvZRMJYav778gIcB6Pb4PUNa9C4EjHfF74aMreOowGdwnLjAIrognCeq_im_sjKCzA-TRIzrHZbgO8p3bJXuyiCizpC2dWWkv9RdcS1VYYmTsAMe7RVFG8v0EVofotjxCye84drStb2cTiZaEirD6h8-uKLN9-8cMHaLwRc620Jwa5ob0Il0mNq7psmGd5xcnZ30paL_snsi4SYEMLfKrTeT=s1000-no?authuser=0",
                                       height="45%", width="90%", align="center" ) ) ),
                          
                          pickerInput("yr_select", "Year:",   
                                      choices = c("Cumulative: 2014-2021", "2014", "2015", "2016", "2017", "2018", 
                                                  "2019", "2020", "2021" ), 
                                      selected = c("Cumulative: 2014-2021" ),
                                      multiple = FALSE) ) ,
                      
                      mainPanel(
                        tabsetPanel(
                          tabPanel("Chloropleth", plotOutput("lp.1") ),
                          tabPanel( "Dorling Cartogram", plotOutput( "lp.2" ) ),
                          tabPanel( "Interactive Leaflet", leafletOutput("lp.3"),
                                    p() ) )
                        )
                      )
             )
  )
)
)
  

# SERVER
server <- function( input, output ) { 
  
  
  
  year.reactive.df.chloro <- reactive(
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
  
  year.reactive.df.dorling <- reactive(
    {
      if(input$yr_select=="Cumulative: 2014-2021") {
        cnties.dorling
      }
      else if(input$yr_select=="2014") {
        cnties.dorling
      }
      else if(input$yr_select=="2015") {
        cnties.dorling
      }
      else if(input$yr_select=="2016") {
        cnties.dorling
      }
      else  if(input$yr_select=="2017") {
        cnties.dorling
      }
      else   if(input$yr_select=="2018") {
        cnties.dorling
      }
      else   if(input$yr_select=="2019") {
        cnties.dorling
      }
      else    if(input$yr_select=="2020") {
        cnties.dorling
      }
      else  if(input$yr_select=="2021") {
        cnties.dorling
      }
    }
  )
    
    year.reactive.df.leaflet <- reactive(
      {
        if(input$yr_select=="Cumulative: 2014-2021") {
          cnty.leaf
        }
        else if(input$yr_select=="2014") {
          cnty.leaf.2014
        }
        else if(input$yr_select=="2015") {
          cnty.leaf.2015
        }
        else if(input$yr_select=="2016") {
          cnty.leaf.2016
        }
        else  if(input$yr_select=="2017") {
          cnty.leaf.2017
        }
        else   if(input$yr_select=="2018") {
          cnty.leaf.2018
        }
        else   if(input$yr_select=="2019") {
          cnty.leaf.2019
        }
        else    if(input$yr_select=="2020") {
          cnty.leaf.2020
        }
        else  if(input$yr_select=="2021") {
          cnty.leaf.2021
        }
      }
  )
  

  output$lp.1 <- renderPlot( lp.plot.chloro( year.reactive.df.chloro() ) )
  output$lp.2 <- renderPlot( lp.plot.dorling( year.reactive.df.dorling() ) )
  output$lp.3 <- renderLeaflet( lp.plot.leaf( year.reactive.df.leaflet() ) )
}


shinyApp( ui, server )

