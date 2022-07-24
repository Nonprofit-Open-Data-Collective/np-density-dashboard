
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

setwd( paste0( main, "np-density-dashboard/Data-Rodeo/Dashboard-County-Data/" ) )

##  County Standard Projection Shapefile
cnties <- readRDS( "USA-Counties.rds" )


## Leaflet County data/shapefile

setwd( "Leaflet-Shapefiles" ) 

cnties.leaf <- readRDS( "USA-Counties-Leaflet.rds" )


## Dorling Cartogram Data/Shapefile

setwd( "../Dorling-Shapefiles/" ) 

cnties.dorling <- readRDS( "USA-Counties-Dorling.rds" )



### Plotting Functions ###

# landing page chloropleths
lp.plot.chloro <- function( df, input ){
  
  # there are issues binning variables with high quantitites of zero values. Thus,
  # we will separate the zero and non-zero values of the metrics into separate datasets, bin them separately
  # and then row bind them back together for the final plot:
  df.zeros <- df[df[[ input ]] == 0, ] # metric = 0
  df.zeros[[ paste0( input ) ]] <- 0           # assign value of zero for the ordinal variable
  
  df.nonzeros <- df[df[[ input ]] != 0,] # metric != 0
  df.nonzeros[[ paste0( input  ) ]] <- factor( quant.cut( var = input, x = 6 , df = df.nonzeros ) ) # bin into 6 
  # ordinal categories
  
  df.out <- rbind( df.zeros, df.nonzeros)  # bind
  df.out[[ paste0( input  ) ]] <- factor( df.out[[ paste0( input  ) ]] ) # set to factor
  
  ggplot( ) + geom_sf( df.out,             # plot
                       mapping = aes_string( fill = paste0( input ) ),
                       color = NA, size = 0.5 ) +
    scale_fill_brewer( "Density Scale", palette = 1 ) +
    theme_minimal( ) +
    theme( text = element_text( family = "Avenir" ) )
  
}

# landing page interactive Leaflet

lp.plot.leaf <- function( df ){
  
  leaflet( df ) %>% 
    addTiles() %>%
    addPolygons( color = "#444444", weight = 1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.5, popup = ~as.character( popup ) ) %>%
    setView( -98.35, 39.50, zoom = 3.499 )
  
}

# landing page histograms

lp.plot.hist <- function( df, input ) {
  
  ggplot( df, aes_string( x =  input ) )  +
    geom_histogram( color="darkblue", fill="lightblue", bins = 40 ) +
    theme_minimal()+
    ylab( "Frequency") +
    xlab( "NPO Density ( # New NPOs / 1,000 county residents )")+
    ggtitle( "Distribution of County Nonprofit (NPO) Density")+
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
                                           choices = c("Cumulative: 2014-2021" = "cum", "2014", "2015", "2016", "2017", "2018", 
                                                       "2019", "2020", "2021" ), 
                                           selected = c("Cumulative: 2014-2021" ),
                                           multiple = FALSE),
                              
                              radioButtons( "ptype", "Plot Type:",
                                            c( "Chloropleth" = "chloro",
                                               "Dorling Cartogram" = "dorling") ) ,
                              
                              pickerInput( "metric", "Metric: ",   
                                           choices = c( "Density" = "dens" ), 
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
                                             height="45%", width="90%", align="center" ) ) ) ,
                            
                            pickerInput( "yr_select", "Year:",   
                                         choices = c("Cumulative: 2014-2021" = "cum", "2014", "2015", "2016", "2017", "2018", 
                                                     "2019", "2020", "2021" ), 
                                         selected = c("Cumulative: 2014-2021" ),
                                         multiple = FALSE) ),
                            
                            mainPanel( leafletOutput( "lp.l.1" ) )
                            
                          )
                      )
             )
  )
)




# SERVER
server <- function( input, output ) { 
  

  # data filtering reactive (by year)
  data_reactive <- reactive(
    {
    
      if (input$ptype=="dorling") 
        { filter( cnties.dorling, year ==  input$yr_select ) }
      else if (input$ptype=="chloro") 
      { filter( cnties, year == input$yr_select ) }
      }
  )
  
  
  
  ### Rendering ###
  
  
  # Radio button for plot type
  output$ptype <- renderPlot( {
    switch(input$ptype,
           "chloro" = lp.plot.chloro( df = cnties %>% filter( year == input$yr_select), input = input$metric ),
           "dorling" = lp.plot.chloro( df = cnties.dorling %>% filter( year == input$yr_select), input = input$metric ) )
  }
  
  )
  
  # histograms
  output$lp.h.1 <- renderPlot( lp.plot.hist( df = data_reactive()  , input = input$metric ) )
  
  # tables
  output$lp.t.1 <- renderTable( lp.tbl( data_reactive(), input = input$metric ) )
  
  # leaflet
  
  output$lp.l.1 <- renderLeaflet( lp.plot.leaf( cnties.leaf %>% filter( year == input$yr_select ) ) )
  
}

shinyApp( ui, server )