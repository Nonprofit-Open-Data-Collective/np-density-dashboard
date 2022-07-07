library( tidyverse )
library( urbnmapr )
library( sf )
library( fipio )
library( tigris )
library( tidycensus )
options( tigris_class = "sf" )
options( tigris_use_cache = TRUE )

lf <- "/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Geospatial Dashboard/Large-Files-Bank"


# import data and standardize FIPS codes
setwd( lf )
np <- readRDS( "NONPROFITS-2014-2021v6.rds" )
np <- np %>%
  mutate( COUNTYFIPS = ifelse( str_count( COUNTYFIPS, '\\d' ) == 1, paste0( '00', COUNTYFIPS ),
                               ifelse( str_count( COUNTYFIPS, '\\d' ) == 2,paste0( '0', COUNTYFIPS ),
                                       ifelse( str_count( COUNTYFIPS, '\\d' ) == 3, paste0( COUNTYFIPS ), NA ) ) ),
          fips_ct = ifelse( is.na( STATEFIPS ) == F & is.na( COUNTYFIPS ) == F, paste0( STATEFIPS, COUNTYFIPS ),NA ) )


# obtain FIPS using `fipio package` for those still missing FIPS codes but having coordinates present                         
trim.sf <- np[ which( is.na( np$COUNTYFIPS ) & is.na( np$lat ) == F & is.na( np$lon ) == F ), 
               c( 'lat','lon','key' ) ]

s <- list( )
for ( i in 1:nrow( trim.sf ) ) {
  fips_c <- coords_to_fips( x = trim.sf$lon[i], y = trim.sf$lat[i] )
  s[[i]] <- data.frame( f = ifelse( length( fips_c ) == 0, NA, fips_c ),
                        key = trim.sf$key[i] )
}   
s <- do.call( 'rbind', s )    

np.b <- left_join( np, s, by = 'key' ) %>%
  mutate( fips_ct = ifelse( is.na( fips_ct ), f, fips_ct ) ) %>%
  select( -f )

invisible( sum( is.na( np$fips_ct ) ) ) # previously had 642 missing county FIPS
invisible( sum( is.na( np$fips_ct ) ) ) # now reduced to 278

# Convert lat/long to a sf
np.sf <- np %>%
  filter( is.na( lat ) == F ) %>% # st_as_sf does not allow missing values in coordinate columns; n = 167 missing
  st_as_sf( coords = c( "lon", "lat" ), crs = "NAD83" )

# obtain county geometries
states.sf <- get_urbn_map( map = "counties", sf = TRUE )

st_crs( states.sf ) 
st_crs( states.sf ) <- st_crs( states.sf )

# Remove PR and GU from npos as well
np   <- np  [ !( np$State      %in% c( 'PR', 'GU' ) ), ] # exclude Puerto Rico and Guam

ggplot( ) +
  geom_sf( data = np.sf ) + 
  geom_sf( data = states.sf, fill = NA, color = "black", size = 0.15, alpha = 0 ) +
  coord_sf( datum = st_crs( 2163 ) ) +   
  labs( fill  = "", 
        title = "",
        caption = '' ) + 
  theme_bw( )
# we have coordinates in questionable places

# Constrain lat long coordinates to diagnose the issue and identify those points
np %>%
  filter( lat > 50 & State != "AK" ) %>%
  select( State, City, input_address )

# there are mispecified addresses that linger in the data--will need to manually update data


## remove points manually beyond latitude/longitude confines of lower 48 and plot
np.sf.o <- np %>%
  filter( lat < 50 & lat > 24 & lon < ( -66 )  & lon > ( -125 ) ) %>%
  st_as_sf( coords = c( "lon", "lat" ), crs = "NAD83" )

# plot
ggplot( ) +
  geom_sf( data = np.sf.o ) + 
  geom_sf( data = states.sf, fill = NA, color = "black", size = 0.15, alpha = 0 ) +
  coord_sf( datum = st_crs( 2163 ) ) +   
  labs( fill  = "", 
        title = "",
        caption = '' ) + 
  theme_bw( )




## Maps of MSA's

# Seattle-Bellevue as an example

wa <- tracts( "WA", cb = TRUE ) # Washington shapefiles

cb <- core_based_statistical_areas( cb = TRUE ) # fetches cartographic boundary files for MSAs
sea <- filter( cb, grepl( "Seattle", NAME ) ) # match "Seattle" and return boundary files

p1 <- wa[ sea, ] # subset tracts from within ( or touching ) MSA boundary

ggplot( ) + 
  geom_sf( data = p1 ) + 
  geom_sf( data = sea, fill = NA, color = "red" ) + # red lines show cartographic boundaries of MSA
  theme_minimal( ) 
# we have census tracts at the boundaries of the MSA that need to be removed

# remove them
s <- st_within( wa, sea ) # spatial overlay

these <- map_lgl( s, function( x ) {
  if ( length( x ) == 1 ) {
    return( TRUE )
  } else {
    return( FALSE )
  }
} )

# final subset and plot
d <- wa[ these, ]
ggplot( ) + 
  geom_sf( data = d ) +
  geom_sf( data = sea, fill = NA, color = "red" )+
  theme_minimal( ) 


## Now, layer on 1023-EZ point data

#  subset 1023 data sf files based on those having geometries within the MSA
lf <- "/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Geospatial Dashboard/Large-Files-Bank"

setwd( lf )
np <- readRDS( "NONPROFITS-2014-2021v6.rds" )

# Convert lat/long to a sf
np.sf <- np %>%
  filter( is.na( lat ) == F ) %>% # st_as_sf does not allow missing values in coordinate columns; n = 167 missing
  st_as_sf( coords = c( "lon", "lat" ), crs = "NAD83" )

# subset
s2 <- st_within( np.sf, sea )

these.2 <- map_lgl( s2, function( x ) {
  if ( length( x ) == 1 ) {
    return( TRUE )
  } else {
    return( FALSE )
  }
} )


np.sf.2 <- np.sf [these.2, ]

ggplot( ) + 
  geom_sf( data = d ) +
  geom_sf( data = np.sf.2 ) + 
  geom_sf( data = sea, fill = NA, color = "red" ) +
  theme_minimal( )



## Chloropleths

# Binning function import
quant.cut <- function( var,x,df ){
  xvec <- vector( )
  for ( i in 1:x ){
    xvec[i] <- i/x
  }
  
  qs <- c( min( df[[var]],na.rm = T ), quantile( df[[var]],xvec,na.rm = T ) )
  
  df[['new']] = x+1 #initialize variable
  
  
  for ( i in 1:( x ) ){
    df[['new']] <- ifelse( df[[var]]<qs[i+1] & df[[var]]>= qs[i],
                           c( 1:length( qs ) )[i],
                           ifelse( df[[var]] == qs[qs == max( qs )],x,df[['new']] ) )
  }
  
  return( df[['new']] )
}


# compound fips code
np.sf.2$fips <- paste0( np.sf.2$STATEFIPS, np.sf.2$COUNTYFIPS, np.sf.2$TRACTFIPS )

dat.geo <- st_join( d, np.sf.2, join = st_nearest_feature, left = T )

# count rows by FIPS, create density measure
c.dens <- dat.geo %>%
  group_by( fips ) %>%
  mutate( pop = ceiling( p.density * ( ALAND*1e-06 ) ), # convert sq. meters to sq. km
          n = n( ), 
          o.density = ( n / pop ) * 1000 ) %>%
  distinct( fips, pop , o.density , geometry )


# bin density for plotting
c.dens$o.den.bin = factor( quant.cut( var = 'o.density', x = 5 ,df = c.dens ) )


# plotting

ggplot( ) + 
  geom_sf( data = d ) +
  geom_sf( data = sea, fill = NA, color = "red" )+
  geom_sf( c.dens,
           mapping = aes( fill = o.den.bin ),
           color = NA, size = 0.5 ) +
  scale_fill_manual( 'Quintile', values = c( "#DCE319FF", "#55C667FF", "#238A8DFF",
                                             "#39568CFF","#440154FF" ) ) +
  theme_minimal( ) 



## Dashboard Landing Page Chloropleth

# Convert lat/long to a sf
np.sf <- np.b %>%
  filter( is.na( lat ) == F ) %>% # st_as_sf does not allow missing values in coordinate columns; n = 167 missing
  st_as_sf( coords = c( "lon", "lat" ), crs = "NAD83" )


# no. of NPOs by county FIPS
n.ct <- as.data.frame( np.sf %>%
                         group_by( fips_ct ) %>%
                         mutate( n = n( ),
                                 fips_ct = as.numeric( fips_ct ) ) %>% # count of NPO's within county FIPS
                         distinct( fips_ct, n ) )


# county population 
library( tidycensus )

##invisible( census_api_key( "apigoeshere", install = TRUE,
##                         overwrite = TRUE ) )
##invisible( readRenviron( "~/.Renviron" ) )

pop.ct <- as.data.frame( get_acs( geography = "county", 
                                  variables = c( pop = "B07401_001" ), 
                                  year = 2020 ) %>%
                           select( fips_ct = GEOID, estimate ) )


# merge
n.pop.merge <- pop.ct %>%
  mutate( fips_ct = as.numeric( fips_ct ) ) %>%
  left_join( n.ct, . ) %>%
  mutate( dens = n / estimate ) %>%
  filter ( is.na( dens ) == F ) %>%
  mutate( dens.q = factor( quant.cut( var = 'dens', x = 5 ,df = . ) ) )


ct <- st_transform( urbnmapr::get_urbn_map( map = "counties", sf = TRUE ), crs = 'NAD83' ) %>%
  mutate( county_fips = as.numeric( county_fips ) ) %>%
  left_join( ., n.pop.merge, by = c( 'county_fips' = 'fips_ct' ) )




ggplot( ) + geom_sf( ct,
                     mapping = aes( fill = dens.q ),
                     color = NA, size = 0.5 )+
  scale_fill_manual( 'Quintile', values = c( "#DCE319FF", "#55C667FF", 
                                             "#238A8DFF", "#39568CFF","#440154FF" ) ) +
  theme_minimal( )+
  theme( legend.position = 'none' )
