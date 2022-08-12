

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


# boardmember NPO directions
spatial_grid <- function( df ){
  
  # alter boundary coordinates 
  # get current bounding box
  current.bbox <- st_bbox( df )
  
  xrange <- current.bbox$xmax - current.bbox$xmin # range of x values
  yrange <- current.bbox$ymax - current.bbox$ymin # range of y values
  
  current.bbox[1] <- current.bbox[1] - (0.4 * xrange) # xmin - left
  current.bbox[3] <- current.bbox[3] + (0.4 * xrange) # xmax - right
  current.bbox[2] <- current.bbox[2] - (0.4 * yrange) # ymin - bottom
  current.bbox[4] <- current.bbox[4] + (0.4 * yrange) # ymax - top
  
  
  # average distance label and organization name
  avg.label <- paste0( "Mean Distance: ", unique( round( df$avg.miles, 2) ), " miles" )
  org.title <- paste0( "Org Name: ", unique( df$ORGNAME ) )
  
  df.df <- data.frame( df )
  
  ann.list <- list()               # list to hold annotations
  for (i in 1: nrow( df ) ){
    
    # lat/lon positions for text
    assign( paste0( "bm.label.lon.", i ), df.df[ i, "lon.bm"] )
    assign( paste0( "bm.label.lat.", i ), df.df[ i, "lat.bm"] )
    
    # labels
    assign( paste0( "bm.label.mile.", i ), paste0( round( df.df[ i, "miles"], 2 ), "miles" ) )
    
    # annotations in ggplot to write labels
    ann.list[[i]] <- paste0( "annotate( 'text', bm.label.lon.", i , ", bm.label.lat.", i , ", label = bm.label.mile.", i,", size = 2.5, vjust = 2, family = 'Avenir' ) +" )
    
    
  }
  
  options(repr.plot.width =9, repr.plot.height =9)
  
  eval( parse( text = paste0( 'ggplot( df ) +
    geom_sf( lwd = 0.3, lineend = "round" ) +
    theme_classic()+
    ggtitle( org.title ) +
    annotate( "text", -Inf, Inf, label = avg.label, hjust = -0.08, vjust = 1, family = "Avenir" ) +',
                              do.call( 'paste0', ann.list ),
                              'coord_sf( xlim = c( current.bbox[1], current.bbox[3] ), ylim = c( current.bbox[2], current.bbox[4] ) ) + # boundaries
    theme( text = element_text( family = "Avenir" ) ) +
    xlab( "Longitude" ) + 
    ylab( "Latitude" )' ) ) )
  
}

