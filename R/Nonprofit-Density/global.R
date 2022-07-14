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

## County-Level Data:

# main std projection file
p <- paste0( "USA-Counties" )
ct <- paste0( main, "np-density-dashboard/Data-Rodeo/Dashboard-County-Data" )
setwd( ct )
assign( "cnties.", readRDS( paste0( p, ".rds") ) )

# yearly std projection files
setwd( "By-Year")

yrs <- c(2014:2021)

for ( i in 1:length( yrs ) ){
  
  assign( paste0( "cnties.", yrs[i] ), readRDS( paste0( p,"-", yrs[i], ".rds") ) )
  
}

# main Dorling Cartogram file
setwd( "../Dorling-Shapefiles" )
assign( "cnties.dorling.", readRDS( paste0( p,"-Dorling", ".rds") ) )

# yearly Dorling Cartogram files
setwd( "Dorling-By-Year")

yrs <- c(2014:2021)

for ( i in 1:length( yrs ) ){
  
  assign( paste0( "cnties.dorling.", yrs[i] ), readRDS( paste0( p,"-Dorling", "-", yrs[i], ".rds") ) )
  
}

# Leaflet file
setwd( paste0( ct, "/Leaflet-Shapefiles") )
assign( "cnties.leaf", readRDS( paste0( p,"-Leaflet", ".rds") ) )

# yearly leaflet files
setwd( "Leaflet-By-Year")

for ( i in 1:length( yrs ) ){
  
  assign( paste0( "cnties.leaf.", yrs[i] ), readRDS( paste0( p,"-Leaflet", "-", yrs[i], ".rds") ) )
  
}


### Plotting Functions ###

# landing page chloropleths
lp.plot <- function( df, input ){
  
  # there are issues binning variables with high quantitites of zero values. Thus,
  # we will separate the zero and non-zero values of the metrics into separate datasets, bin them separately
  # and then row bind them back together for the final plot:
  df.zeros <- df[ df[[ input ]] == 0, ] # metric = 0
  df.zeros[[ paste0( input, ".q" ) ]]<- 0           # assign value of zero for the ordinal variable
  
  df.nonzeros <- df[ df[[ input ]] != 0, ] # metric != 0
  df.nonzeros[[ paste0( input, ".q" ) ]]<- factor( quant.cut( var = input, x = 6 , df = df.nonzeros ) ) # bin into 6 
  # ordinal categories
  
  df.out <- rbind( df.zeros, df.nonzeros)  # bind
  df.out[[ paste0( input, ".q" ) ]]<- factor( df.out[[ paste0( input, ".q" ) ]] ) # set to factor
  
  
  ggplot( ) + geom_sf( df.out,             # plot
                       mapping = aes_string( fill = paste0( input, ".q" ) ),
                       color = NA, size = 0.5 ) +
    scale_fill_brewer( "Density Scale", palette = 1 ) +
    theme_minimal( ) +
    theme( text = element_text( family = "Avenir" ) )
  
}

# landing page interactive Leaflet

lp.plot.leaf <- function( df ){
  
  leaflet(df) %>% 
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

