###---------------------------------------------------
###   SHINY APP--NONPROFIT DENSITY
###---------------------------------------------------

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
  
  ct.yr <- paste0( 'cnties.', 2014:2021 )
  
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

## Read-in yearly data for county Leaflets
setwd( "Dashboard-County-Data/Dorling-Shapefiles/Dorling-By-Year" )

for (i in 1:length( dir( ) ) ) {
  
  ct.yr.d <- paste0( 'cnties.dorling.', 2014:2021 )
  
  assign( ct.yr.d[ i ], readRDS( dir()[i] ) )
  
  
}


### Plotting Functions ###

# landing page chloropleths
lp.plot.chloro <- function( df, input ){
  
  # there are issues binning variables with high quantitites of zero values. Thus,
  # we will separate the zero and non-zero values of the metrics into separate datasets, bin them separately
  # and then row bind them back together for the final plot:
  df.zeros <- df[df$dens == 0, ] # metric = 0
  df.zeros$dens.q <- 0           # assign value of zero for the ordinal variable
  
  df.nonzeros <- df[df$dens != 0,] # metric != 0
  df.nonzeros$dens.q <- factor( quant.cut( var = 'dens', x = 6 , df = df.nonzeros ) ) # bin into 6 
  # ordinal categories
  
  df.out <- rbind( df.zeros, df.nonzeros)  # bind
  df.out$dens.q <- factor( df.out$dens.q ) # set to factor
  
  ggplot( ) + geom_sf( df.out,             # plot
                       mapping = aes_string( fill = input ),
                       color = NA, size = 0.5 ) +
    scale_fill_brewer( "Density Scale", palette = 1 ) +
    theme_minimal( ) +
    theme( text = element_text( family = "Avenir" ) )
  
}

# landing page Dorling Cartograms
lp.plot.dorling <- function( df, input ){
  
  # there are issues binning variables with high quantitites of zero values. Thus,
  # we will separate the zero and non-zero values of the metrics into separate datasets, bin them separately
  # and then row bind them back together for the final plot:
  df.zeros <- df[df$dens == 0, ] # metric = 0
  df.zeros$dens.q <- 0           # assign value of zero for the ordinal variable
  
  df.nonzeros <- df[df$dens != 0,] # metric != 0
  df.nonzeros$dens.q <- factor( quant.cut( var = 'dens', x = 6 , df = df.nonzeros ) ) # bin into 6 
  # ordinal categories
  
  df.out <- rbind( df.zeros, df.nonzeros)  # bind
  df.out$dens.q <- factor( df.out$dens.q ) # set to factor
  
  df$dens.q <- factor( quant.cut( var = 'dens', x = 7 ,df = df ) )
  
  ggplot(  )  +
    geom_sf( df, mapping = aes_string( fill = input ),  color = NA ) +
    scale_fill_brewer( "Density Scale", palette = 1 ) +
    theme_minimal( ) +
    theme( text = element_text( family = "Avenir" ) )
}

# landing page interactive Leaflet

lp.plot.leaf <- function( df ){
  
  leaflet(df) %>% 
    addTiles() %>%
    addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.5, popup = ~as.character(popup) ) %>%
    setView(-98.35, 39.50, zoom = 3.499)
  
}

# landing page histograms

lp.plot.hist <- function( df, input ) {
  
  ggplot( df, aes_string( x =  input ) )  +
    geom_histogram( color="darkblue", fill="lightblue", bins = 40 ) +
    theme_minimal()+
    ylab( "Frequency") +
    xlab( "# New Nonprofits per County")+
    ggtitle( "Distribution of County Nonprofits")+
    theme( text = element_text( family = "Avenir") )
  
}

# landing page summary stats
lp.tbl <- function( df, input ) {
  round( data.frame( Mean = mean( df[[ input ]], na.rm = T),
                     Median = median( df[[ input ]], na.rm = T),
                     Min = min( df[[ input ]], na.rm = T),
                     Max = max( df[[ input ]], na.rm = T),
                     Variance = var( df[[ input ]], na.rm = T),
                     SD = sd( df[[ input ]], na.rm = T) ), digits = 2 )
  
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
                              span( div( img(src="output-onlineimagetools(2).png",
                                             height="45%", width="90%", align="center" ) ) ),
                              
                              pickerInput( "yr_select", "Year:",   
                                           choices = c("Cumulative: 2014-2021", "2014", "2015", "2016", "2017", "2018", 
                                                       "2019", "2020", "2021" ), 
                                           selected = c("Cumulative: 2014-2021" ),
                                           multiple = FALSE),
                              
                              radioButtons( "ptype", "Plot Type:",
                                            c( "Chloropleth" = "chloro",
                                               "Dorling Cartogram" = "dorling") ) ,
                              
                              pickerInput( "metric", "Metric: ",   
                                           choices = c("Density" ), 
                                           selected = c("Density" ),
                                           multiple = FALSE) ),
                            
                            mainPanel( plotOutput( "ptype" , width = "90%", height = 400),
                                       plotOutput( "lp.h.1" ,width = "90%", height = 200),
                                       tableOutput( "lp.t.1" ) )
                            
                          )
                      )
             ),
             
             tabPanel("Interactive Leaflet Map",
                      div(class="outer",
                          tags$head(includeCSS("/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Geospatial Dashboard/np-density-dashboard/R/Nonprofit-Density/styles.css")),
                          
                          sidebarLayout(
                            sidebarPanel(
                              span( div( img(src="output-onlineimagetools(2).png",
                                             height="45%", width="90%", align="center" ) ) ) ),
                            
                            mainPanel( leafletOutput( "lp.3" ) )
                            
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
        cnties.2014
      }
      else if(input$yr_select=="2015") {
        cnties.2015
      }
      else if(input$yr_select=="2016") {
        cnties.2016
      }
      else  if(input$yr_select=="2017") {
        cnties.2017
      }
      else   if(input$yr_select=="2018") {
        cnties.2018
      }
      else   if(input$yr_select=="2019") {
        cnties.2019
      }
      else    if(input$yr_select=="2020") {
        cnties.2020
      }
      else  if(input$yr_select=="2021") {
        cnties.2021
      }
    }
    
  )
  
  year.reactive.df.dorling <- reactive(
    {
      if(input$yr_select=="Cumulative: 2014-2021") {
        cnties.dorling
      }
      else if(input$yr_select=="2014") {
        cnties.dorling.2014
      }
      else if(input$yr_select=="2015") {
        cnties.dorling.2015
      }
      else if(input$yr_select=="2016") {
        cnties.dorling.2016
      }
      else  if(input$yr_select=="2017") {
        cnties.dorling.2017
      }
      else   if(input$yr_select=="2018") {
        cnties.dorling.2018
      }
      else   if(input$yr_select=="2019") {
        cnties.dorling.2019
      }
      else    if(input$yr_select=="2020") {
        cnties.dorling.2020
      }
      else  if(input$yr_select=="2021") {
        cnties.dorling.2021
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
  
  
  
  # reactive for summary statistics variable selector
  metric.reactive <- reactive({
    if (input$metric =='Density'){
      "n"
    }
  })
  
  # reactive for chloropleth variable selector
  
  metric.reactive.b <- reactive({
    if (input$metric =='Density'){
      "dens.q"
    }
  })
  
  
  ### Rendering ###
  
  
  # Radio button for plot type
  output$ptype <- renderPlot( {
    switch(input$ptype,
           "chloro" = lp.plot.chloro( df = year.reactive.df.chloro(), input = metric.reactive.b() ),
           "dorling" = lp.plot.dorling( df = year.reactive.df.dorling(), input = metric.reactive.b() ) )
  }
  
  )
  
  # histograms
  output$lp.h.1 <- renderPlot( lp.plot.hist( df = year.reactive.df.chloro(), input = metric.reactive() ) )
  
  # tables
  output$lp.t.1 <- renderTable( lp.tbl( df= year.reactive.df.chloro(), input = metric.reactive() ) )
}

shinyApp( ui, server )