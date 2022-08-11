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

# import helper functions
source('/Volumes/Chris-SSD/Urban Institute/Summer Projects/Geospatial Dashboard/np-density-dashboard/R/helpers.R')
source('/Volumes/Chris-SSD/Urban Institute/Summer Projects/Geospatial Dashboard/np-density-dashboard/R/Nonprofit-Density/plotting-fcts.R')

# Render png to jpeg

# Import counties landing page map
main <- "/Volumes/Chris-SSD/Urban Institute/Summer Projects/Geospatial Dashboard/"

lf <- "/Volumes/Chris-SSD/Urban Institute/Summer Projects/Geospatial Dashboard/Large-Files-Bank"


setwd( paste0( main, "np-density-dashboard/Data-Rodeo/Dashboard-County-Data/" ) )

##  County Standard Projection Shapefile
cnties <- readRDS( "USA-Counties.rds" )


## Leaflet County data/shapefile

setwd( "Leaflet-Shapefiles" ) 

cnties.leaf <- readRDS( "USA-Counties-Leaflet.rds" )


## Dorling Cartogram Data/Shapefile

setwd( "../Dorling-Shapefiles/" ) 

cnties.dorling <- readRDS( "USA-Counties-Dorling.rds" )


## MSA Standard Projection Shapefile

setwd( paste0( lf,"/Dashboard-MSA-Data/") )

msas <- readRDS( "USA-MSAs.rds" )

## MSA Dorling Shapefile

setwd( paste0( lf,"/Dashboard-MSA-Data/Dorling-Shapefiles") )

msas.dorling <- readRDS( "USA-MSAs-Dorling.rds" )


## Boardmember/NPO Data Shapefile

setwd( paste0( lf,"/10-Spatial-Grid-Data") )

bm.npo <- readRDS( "BM-NPO-Spatial-Grid.rds" )



# USER INTERFACE
ui <- bootstrapPage(
  navbarPage(theme = shinytheme( "flatly" ), collapsible = TRUE,
             
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">U.S. New Nonprofit Density, 2014-2021</a>'), 
             id="nav",
             
             ## Counties Tab
             tabPanel("U.S. Counties",
                      div(class="outer",
                          tags$head(includeCSS("/Volumes/Chris-SSD/Urban Institute/Summer Projects/Geospatial Dashboard/np-density-dashboard/R/Nonprofit-Density/styles.css")),
                          
                          sidebarLayout(
                            sidebarPanel(
                              span( div( img(src="output-onlineimagetools(2).png",
                                             height="45%", width="90%", align="center" ) ) ),
                              
                              selectInput( "yr_select", "Year:",   
                                           choices = c("Cumulative: 2014-2021" = "cum", "2014", "2015", "2016", "2017", "2018", 
                                                       "2019", "2020", "2021" ), 
                                           selected = c("Cumulative: 2014-2021" ),
                                           multiple = FALSE),
                              
                              radioButtons( "ptype", "Plot Type:",
                                            c( "Chloropleth" = "chloro",
                                               "Dorling Cartogram" = "dorling") ) ,
                              
                              selectInput( "metric", "Metric: ",   
                                           choices = c( "Density" = "dens" ), 
                                           selected = c("Density" ),
                                           multiple = FALSE) ),
                            
                            mainPanel( plotOutput( "ptype" , width = "90%", height = 400 ),
                                       plotOutput( "lp.h.1" ,width = "90%", height = 200 ),
                                       tableOutput( "lp.t.1" ) ) 
                            
                          )
                      )
             ),
             
             ## MSAs Tab
             tabPanel("Metropolitan Statistical Areas",
                      div(class="outer",
                          tags$head(includeCSS("/Volumes/Chris-SSD/Urban Institute/Summer Projects/Geospatial Dashboard/np-density-dashboard/R/Nonprofit-Density/styles.css")),
                          
                          sidebarLayout(
                            sidebarPanel(
                              span( div( img(src="output-onlineimagetools(2).png",
                                             height="45%", width="90%", align="center" ) ) ),
                              
                              selectInput( "yr_select_msa", "Year:",   
                                           choices = c("Cumulative: 2014-2021" = "cum", "2014", "2015", "2016", "2017", "2018", 
                                                       "2019", "2020", "2021" ), 
                                           selected = c("Cumulative: 2014-2021" ),
                                           multiple = FALSE),
                              
                              radioButtons( "ptype_msa", "Plot Type:",
                                            c( "Chloropleth" = "chloro",
                                               "Dorling Cartogram" = "dorling") ) ,
                              
                              selectInput( "metric_msa", "Metric: ",   
                                           choices = c( "Density" = "dens" ), 
                                           selected = c("Density" ),
                                           multiple = FALSE),
                              
                              selectInput( "msa", "MSA: ",   
                                           choices = c( "Atlanta-Sandy-Springs-Alpharetta", "Austin-Round-Rock-Georgetown", 
                                                        "Baltimore-Columbia-Towson", "Boston-Cambridge-Newton", 
                                                        "Charlotte-Concord-Gastonia", "Chicago-Naperville-Elgin", "Cincinnati", 
                                                        "Cleveland-Elyria", "Columbus", "Dallas-Fort-Worth-Arlington", "Denver-Aurora-Lakewood", 
                                                        "Detroit-Warren-Dearborn", "Houston-The-Woodlands-Sugar-Land", 
                                                        "Indianapolis-Carmel-Anderson", "Jacksonville", "Kansas-City", 
                                                        "Las-Vegas-Henderson-Paradise", "Los-Angeles-Long-Beach-Anaheim", 
                                                        "Miami-Fort-Lauderdale-Pompano-Beach", "Milwaukee-Waukesha", 
                                                        "Minneapolis-St-Paul-Bloomington", "Nashville-Davidson--Murfreesboro--Franklin", 
                                                        "New-York-Newark-Jersey-City", "Oklahoma-City", "Orlando-Kissimmee-Sanford", 
                                                        "Philadelphia-Camden-Wilmington", "Phoenix-Mesa-Chandler", "Pittsburgh", 
                                                        "Portland-Vancouver-Hillsboro", "Providence-Warwick", "Raleigh-Cary", 
                                                        "Riverside-San-Bernardino-Ontario", "Sacramento-Roseville-Folsom", 
                                                        "San-Antonio-New-Braunfels", "San-Diego-Chula-Vista-Carlsbad", 
                                                        "San-Francisco-Oakland-Berkeley", "San-Jose-Sunnyvale-Santa-Clara", 
                                                        "San-Juan-Bayamón-Caguas", "Seattle-Tacoma-Bellevue", "St-Louis", 
                                                        "Tampa-St-Petersburg-Clearwater", "Virginia-Beach-Norfolk-Newport-News", 
                                                        "Washington-Arlington-Alexandria" ), 
                                           selected = c( "Washington-Arlington-Alexandria" ),
                                           multiple = FALSE) ),
                            
                            mainPanel( plotOutput( "ptype.msa" , width = "90%", height = 360),
                                       plotOutput( "lp.h.1.msa" ,width = "90%", height = 170),
                                       fluidRow( tableOutput( "lp.t.1.msa" ) ) )
                            
                          )
                      )
             ),
             
             ## Boardmember/NPO Tab
             tabPanel("Boardmember Distances",
                      div(class="outer",
                          tags$head(includeCSS("/Volumes/Chris-SSD/Urban Institute/Summer Projects/Geospatial Dashboard/np-density-dashboard/R/Nonprofit-Density/styles.css")),
                          
                          sidebarLayout(
                            sidebarPanel(
                              span( div( img( src = "output-onlineimagetools(2).png",
                                              height="45%", width="90%", align="center" ) ) ),
                              
                              
                              
                              
                              selectInput( "msa_bm", "MSA: ",   
                                           choices = c( "Atlanta-Sandy-Springs-Alpharetta", "Austin-Round-Rock-Georgetown", 
                                                        "Baltimore-Columbia-Towson", "Boston-Cambridge-Newton", 
                                                        "Charlotte-Concord-Gastonia", "Chicago-Naperville-Elgin", "Cincinnati", 
                                                        "Cleveland-Elyria", "Columbus", "Dallas-Fort-Worth-Arlington", "Denver-Aurora-Lakewood", 
                                                        "Detroit-Warren-Dearborn", "Houston-The-Woodlands-Sugar-Land", 
                                                        "Indianapolis-Carmel-Anderson", "Jacksonville", "Kansas-City", 
                                                        "Las-Vegas-Henderson-Paradise", "Los-Angeles-Long-Beach-Anaheim", 
                                                        "Miami-Fort-Lauderdale-Pompano-Beach", "Milwaukee-Waukesha", 
                                                        "Minneapolis-St-Paul-Bloomington", "Nashville-Davidson--Murfreesboro--Franklin", 
                                                        "New-York-Newark-Jersey-City", "Oklahoma-City", "Orlando-Kissimmee-Sanford", 
                                                        "Philadelphia-Camden-Wilmington", "Phoenix-Mesa-Chandler", "Pittsburgh", 
                                                        "Portland-Vancouver-Hillsboro", "Providence-Warwick", "Raleigh-Cary", 
                                                        "Riverside-San-Bernardino-Ontario", "Sacramento-Roseville-Folsom", 
                                                        "San-Antonio-New-Braunfels", "San-Diego-Chula-Vista-Carlsbad", 
                                                        "San-Francisco-Oakland-Berkeley", "San-Jose-Sunnyvale-Santa-Clara", 
                                                        "San-Juan-Bayamón-Caguas", "Seattle-Tacoma-Bellevue", "St-Louis", 
                                                        "Tampa-St-Petersburg-Clearwater", "Virginia-Beach-Norfolk-Newport-News", 
                                                        "Washington-Arlington-Alexandria" ), 
                                           selected = c( "Washington-Arlington-Alexandria" ),
                                           multiple = FALSE),
                              
                              uiOutput("conditional_select") ),
                            
                            mainPanel( fluidRow( column( width = 6, align = "center",
                                          plotOutput( "sp.gr" ) ) )
                            )
                            
                          )
                      )
             ),
             
             ## Leaflet Tab
             tabPanel("Interactive Leaflet Map",
                      div(class="outer",
                          tags$head(includeCSS("/Volumes/Chris-SSD/Urban Institute/Summer Projects/Geospatial Dashboard/np-density-dashboard/R/Nonprofit-Density/styles.css")),
                          
                          sidebarLayout(
                            sidebarPanel(
                              span( div( img(src="output-onlineimagetools(2).png",
                                             height="45%", width="90%", align="center" ) ) ) ,
                              
                              selectInput( "yr_select", "Year:",   
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
  
  ## dependent inputs for boardmember/NPO spatial grids
  output$conditional_select <- renderUI({
    selectInput("npo_options", "Non-Profit Organization:", 
                choices = c(unique( as.character( data.frame( bm.npo )[ which( bm.npo$MSA == input$msa_bm ), "ORGNAME" ] ) ) ) )
  })
  

  

  
  ## data filtering reactive (by year)
  
  # counties dataset
  data_reactive <- reactive(
    {
      
      if ( input$ptype =="dorling" ) 
      { filter( cnties.dorling, year ==  input$yr_select ) }
      else if ( input$ptype=="chloro" ) 
      { filter( cnties, year == input$yr_select ) }
    }
  )
  
  # MSAs dataset
  data_reactive_msa <- reactive(
    {
      
      if (input$ptype_msa=="dorling") 
      { filter( msas.dorling, year ==  input$yr_select_msa & MSA == input$msa ) }
      else if (input$ptype_msa=="chloro") 
      { filter( msas, year == input$yr_select_msa & MSA == input$msa ) }
    }
  )
  

  ### Rendering ###
  
  ## County Maps
  
  # Radio button for plot type
  output$ptype <- renderPlot( {
    switch(input$ptype,
           "chloro" = lp.plot.chloro( df = cnties %>% filter( year == input$yr_select ), input = input$metric ), 
           "dorling" = lp.plot.chloro( df = cnties.dorling %>% filter( year == input$yr_select ), input = input$metric ) )
  }
  
  )
  
  # histograms
  output$lp.h.1 <- renderPlot( lp.plot.hist( df = data_reactive()  , input = input$metric ) )
  
  # tables
  output$lp.t.1 <- renderTable( lp.tbl( data_reactive(), input = input$metric ) )
  
  # leaflet
  
  output$lp.l.1 <- renderLeaflet( lp.plot.leaf( cnties.leaf %>% filter( year == input$yr_select ) ) )
  
  
  ## MSA Maps
  
  # Radio button for plot type
  output$ptype.msa <- renderPlot( {
    switch(input$ptype_msa,
           "chloro" = lp.plot.chloro( df = msas %>% filter( year == input$yr_select_msa & MSA == input$msa ), input = input$metric_msa  ),
           "dorling" = lp.plot.chloro( df = msas.dorling %>% filter( year == input$yr_select_msa  & MSA == input$msa ), input = input$metric_msa ) )
  }
  
  )
  
  # histograms
  output$lp.h.1.msa <- renderPlot( lp.plot.hist( df = data_reactive_msa()  , input = input$metric_msa ) )
  
  # tables
  output$lp.t.1.msa <- renderTable( lp.tbl( df=data_reactive_msa(), input = input$metric_msa ) )
  
  # spatial grids
  output$sp.gr <- renderPlot( spatial_grid( bm.npo %>% filter( MSA == input$msa_bm & ORGNAME == input$npo_options) ) )
}

# spatial grids


shinyApp( ui, server )