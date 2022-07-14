source( "global.R")

## USER INTERFACE
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
                            
                            mainPanel( leafletOutput( "lp.l.1" ) )
                            
                          )
                      )
             )
  )
)




## SERVER
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
        cnties.leaf
      }
      else if(input$yr_select=="2014") {
        cnties.leaf.2014
      }
      else if(input$yr_select=="2015") {
        cnties.leaf.2015
      }
      else if(input$yr_select=="2016") {
        cnties.leaf.2016
      }
      else  if(input$yr_select=="2017") {
        cnties.leaf.2017
      }
      else   if(input$yr_select=="2018") {
        cnties.leaf.2018
      }
      else   if(input$yr_select=="2019") {
        cnties.leaf.2019
      }
      else    if(input$yr_select=="2020") {
        cnties.leaf.2020
      }
      else  if(input$yr_select=="2021") {
        cnties.leaf.2021
      }
    }
  )
  
  
  
  # reactive for summary statistics variable selector
  metric.reactive <- reactive({
    if (input$metric =='Density'){
      "n"
    }
  })
  
  ## reactive for chloropleth variable selector
  # values should the continuous variables in the dataset. The plotting function automatically
  # bins the variable into 7 categories, including a separate bin for zeros
  
  
  metric.reactive.b <- reactive({
    if (input$metric =='Density'){
      "dens"
    }
  })
  
  
  ### Rendering ###
  
  
  # Radio button for plot type
  output$ptype <- renderPlot( {
    switch(input$ptype,
           "chloro" = lp.plot.chloro( df = year.reactive.df.chloro(), input = metric.reactive.b() ),
           "dorling" = lp.plot.chloro( df = year.reactive.df.dorling(), input = metric.reactive.b() ) )
  }
  
  )
  
  # histograms
  output$lp.h.1 <- renderPlot( lp.plot.hist( df = year.reactive.df.chloro(), input = metric.reactive() ) )
  
  # tables
  output$lp.t.1 <- renderTable( lp.tbl( df= year.reactive.df.chloro(), input = metric.reactive() ) )
  
  # leaflet map
  
  output$lp.l.1 <- renderLeaflet( lp.plot.leaf( year.reactive.df.leaflet() ) )
}

shinyApp( ui, server )

