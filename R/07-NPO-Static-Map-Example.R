###---------------------------------------------------
###   07-NPO STATIC MAPS USING SF OBJECTS
###---------------------------------------------------

# In this script, we will generate some static maps using ggplot and the sf package. We will obtain US state, county
# and tract shapefiles from the `urbnmapr` and `tigris` package, do some exploratory mapping and clean up the 
# FIPS codes in the non-profit dataset so that they are standardized and can be merged with data from 
# other sources (i.e., any census data we pull from `tidycensus`).
#
# These maps are for the midsummer update presentation delivered to Jesse and the other interns

## New dataset that will be stored after running this script: "NONPROFITS-2014-2021v7.rds"

library( tidyverse )
library( urbnmapr )         # state/county shapefiles
library( sf )               # simple features framework
library( fipio )            # coordinates to FIPS codes
library( tigris )           # TIGER shapefiles
library( tidycensus )       # Census data queries
options( tigris_class = "sf" )
options( tigris_use_cache = TRUE )

# import helper functions
source('/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Geospatial Dashboard/np-density-dashboard/R/helpers.R')

lf <- "/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Geospatial Dashboard/Large-Files-Bank"


## import data and standardize FIPS codes

# state FIPS codes should be 2 digits long, county FIPS codes should be 5 digits long, and tract FIPS
# codes should be 6 digits long. Some of the FIPS code variables in our dataset have been coerced to numeric
# resulting in meaningful "0"'s being omitted from those variables. We will add missing "0"'s to the FIPS codes
# and store them as character vectors. A GEOID variable will be created (11 digits long) that concatenates the three
# columns of FIPS code.

setwd( lf )

np <- readRDS( "NONPROFITS-2014-2021v6.rds" ) %>%
  mutate( STATEFIPS = ifelse( str_count( STATEFIPS, '\\d' ) == 1, paste0( '0', STATEFIPS ),STATEFIPS ),
          fips.st = ifelse( is.na( STATEFIPS ) == F , paste0( STATEFIPS ),NA ),
          
          COUNTYFIPS = ifelse( str_count( COUNTYFIPS, '\\d' ) == 1, paste0( '00', COUNTYFIPS ),
                               ifelse( str_count( COUNTYFIPS, '\\d' ) == 2,paste0( '0', COUNTYFIPS ),
                                       ifelse( str_count( COUNTYFIPS, '\\d' ) == 3, paste0( COUNTYFIPS ), NA ) ) ),
          fips.ct = ifelse( is.na( STATEFIPS ) == F & is.na( COUNTYFIPS ) == F, paste0( STATEFIPS, COUNTYFIPS ),NA ),
          
          TRACTFIPS = ifelse( str_count( TRACTFIPS, '\\d' ) == 2, paste0( '0000', TRACTFIPS ),
                              ifelse( str_count( TRACTFIPS, '\\d' ) == 3, paste0( '000', TRACTFIPS ),
                                      ifelse( str_count( TRACTFIPS, '\\d' ) == 4, paste0( '00', TRACTFIPS ),
                                              ifelse( str_count( TRACTFIPS, '\\d' ) == 5, paste0( '0', TRACTFIPS ),
                                                      TRACTFIPS ) ) ) ),
          GEOID = ifelse( is.na( STATEFIPS ) == F & is.na( COUNTYFIPS ) == F & is.na( TRACTFIPS ) == F,
                          paste0( STATEFIPS, COUNTYFIPS, TRACTFIPS ), NA ) ) %>%
  select( -c( tract_fips, state_fips, county_fips ) ) # remove redundant, unstandardized columns
          
  

# obtain FIPS using `fipio package` for those still missing FIPS codes but having coordinates present  

# NOTE: this function has vectorized functionality (https://github.com/UFOKN/fipio) but for some reason
# it does not seem to work on our data frame. Thus, I looped it instead:

trim.sf <- np[ which( is.na( np$COUNTYFIPS ) & is.na( np$lat ) == F & is.na( np$lon ) == F ), 
               c( 'lat','lon','key' ) ]

# s <- list( )
# for ( i in 1:nrow( trim.sf ) ) {
#   fips_c <- coords_to_fips( x = trim.sf$lon[i], y = trim.sf$lat[i] )
#   
#   s[[i]] <- data.frame( f = ifelse( length( fips_c ) == 0, NA, fips_c ),
#                         key = trim.sf$key[i] )
# }   
# s <- do.call( 'rbind', s )    
# 
# np.b <- left_join( np, s, by = 'key' ) %>%
#   mutate( fips.ct = ifelse( is.na( fips.ct ), f, fips.ct ) ) %>%
#   select( -f )
# 
# sum( is.na( np$fips.ct ) )  # previously had 642 missing county FIPS
# sum( is.na( np$fips.ct ) )  # now reduced to 278



## Instead of `fipio` (which only returns 5-digit FIPS codes)
# We can manually submit a spreadsheet of the latitude longitude codes to GEOCODIO service to do reverse
# geocoding which returns the FIPS codes (url: https://www.geocod.io/convert-addresses-to-census-tracts/).
# This service provides up to 2,500 free queries per day.


lf2<- paste0( lf, '/7-Geocodio' )

setwd( lf2 )

# write.csv( trim.sf, 'trim.csv')   # push this file through the GEOCODIO service

# read in reverse geocoded files from GEOCODIO and clean up FIPS columns
np.trim <- read.csv("trim-geocodio.csv") %>%
  
  rename( COUNTYFIPS = County.FIPS,
         STATEFIPS = State.FIPS,
         TRACTFIPS = Census.Tract.Code ) %>%
  
  mutate( STATEFIPS = ifelse( str_count( STATEFIPS, '\\d' ) == 1, paste0( '0', STATEFIPS ),STATEFIPS ),
          fips.st = ifelse( is.na( STATEFIPS ) == F , paste0( STATEFIPS ),NA ),
          
          COUNTYFIPS = str_sub( COUNTYFIPS, -3, -1 ), # COUNTYFIPS column is only column not standardized like the others
          # since it is concatenated with the state FIPS code. We will keep only the first three digits of each string
          COUNTYFIPS = ifelse( str_count( COUNTYFIPS, '\\d' ) == 1, paste0( '00', COUNTYFIPS ),
                               ifelse( str_count( COUNTYFIPS, '\\d' ) == 2,paste0( '0', COUNTYFIPS ),
                                       ifelse( str_count( COUNTYFIPS, '\\d' ) == 3, paste0( COUNTYFIPS ), COUNTYFIPS ) ) ),
          fips.ct = ifelse( is.na( STATEFIPS ) == F & is.na( COUNTYFIPS ) == F, paste0( STATEFIPS, COUNTYFIPS ),NA ),
          
          TRACTFIPS = ifelse( str_count( TRACTFIPS, '\\d' ) == 2, paste0( '0000', TRACTFIPS ),
                              ifelse( str_count( TRACTFIPS, '\\d' ) == 3, paste0( '000', TRACTFIPS ),
                                      ifelse( str_count( TRACTFIPS, '\\d' ) == 4, paste0( '00', TRACTFIPS ),
                                              ifelse( str_count( TRACTFIPS, '\\d' ) == 5, paste0( '0', TRACTFIPS ),
                                                      TRACTFIPS ) ) ) ),
          
          GEOID = ifelse( is.na( STATEFIPS ) == F & is.na( COUNTYFIPS ) == F & is.na( TRACTFIPS ) == F,
                          paste0( STATEFIPS, COUNTYFIPS, TRACTFIPS ), NA ) ) %>%
  
  select( key, STATEFIPS, COUNTYFIPS, TRACTFIPS, fips.ct, fips.st, GEOID )
# STATEFIPS, COUNTYFIPS, TRACTFIPS are standardized to 2, 3, and 6 digits, respectively.
# fips.ct is the 5-digit county FIPS code (state and county FIPS concatenated)
# fips.st is the 2-digit state code (redundant--same as STATEFIPS)
# GEOID is the 11-digit concatenated tract FIPS (named GEOID due to overlap with data coming from
# `tidycensus` and `tigris`--this makes it easier for merging later on)


## Replace values in the original `np` dataset that had FIPS values missing but were recovered by GEOCODIO
these.keys <- which( np$key %in% np.trim$key )
these.cols <- colnames( np.trim )

# make copy  to overwrite missings
np.b <- np
np.b[ these.keys, these.cols ] <- np.trim

sum( is.na( np$GEOID ) ) # 642 missing before GEOCODIO reverse geocoding
sum( is.na( np.b$GEOID ) ) # only 312 missing now

# save

setwd( lf )

saveRDS( np.b, 'NONPROFITS-2014-2021v7.rds' )




## Transform data to an `sf` object 

# Convert lat/long to a sf
np.sf <- np.b %>%
  filter( is.na( lat ) == F ) %>% # st_as_sf does not allow missing values in coordinate columns; n = 167 missing
  st_as_sf( coords = c( "lon", "lat" ), crs = "NAD83" )

# obtain county geometries
states.sf <- get_urbn_map( map = "counties", sf = TRUE )
class( states.sf ) # sf

st_crs( states.sf ) 

# project states sf
states.sf <- transform( states.sf, crs = 'NAD83' )

ggplot( ) +
  geom_sf( data = states.sf )+
  geom_sf( data = np.sf ) 

# we have coordinates in questionable places

# Constrain lat long coordinates to diagnose the issue and identify those points
np.b %>%
  filter( lat > 50 & State != "AK" ) %>%
  select( State, City, input_address )

# there are misspecified addresses that linger in the data--will need to manually update data


## remove points manually beyond latitude/longitude confines of lower 48 and plot
np.sf.o <- np.b %>%
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

# Convert lat/long to a sf
np.sf <- np.b %>%
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



# compound fips code

# no. of NPOs by census tract FIPS
n.t <- np.b %>%
  group_by( GEOID ) %>%
  mutate( n = n( ) ) %>% # count of NPO's within county FIPS
  distinct( GEOID, n ) 

# count rows by FIPS, create density measure ( but first obtain tract-level population estimates)

## Create environmental variable to store API key:
##invisible( census_api_key( "apigoeshere", install = TRUE,
##                         overwrite = TRUE ) )
##invisible( readRenviron( "~/.Renviron" ) )

c.dens <- get_acs( geography = "tract", 
         variables = "B01003_001",       # TOTAL_POPULATION
         state = "53") %>%         # restrict to Seattle Region
  select( GEOID, estimate ) %>%
  rename( pop = estimate ) %>%
  left_join( d, .) %>%
  left_join( ., n.t ) %>%
  mutate( o.density = ( n / pop ) * 100 ) %>%          # NPO create density metric
  distinct( GEOID, pop , o.density , geometry ) %>%
  mutate( o.den.bin = factor( quant.cut( var = 'o.density', x = 5 ,df = . ) ) )      # bin into quintiles

sapply( c.dens , function( x ) sum( is.na(x ) ) ) # 294 tracts w/o any npo's


# plotting

ggplot( ) + 
  geom_sf( data = d )  +
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
n.ct <- np.b %>%
  group_by( fips.ct ) %>%
  mutate( n = n( ) ) %>% # count of NPO's within county FIPS
  distinct( fips.ct, n ) 


# county population merge

pop.ct <- get_acs( geography = "county", 
                   variables = "B01003_001" )  %>%      # TOTAL_POPULATION
  select( fips.ct = GEOID, pop = estimate ) 


# merge
n.pop.merge <- pop.ct %>%
  mutate( fips.ct =  fips.ct  ) %>%
  left_join( n.ct, . ) %>%
  mutate( dens = n / pop ) 
  
n.pop.merge$dens.q <- factor( quant.cut( var = 'dens', x = 5 ,df = n.pop.merge ) )      # bin


ct <- st_transform( urbnmapr::get_urbn_map( map = "counties", sf = TRUE ), crs = 'NAD83' ) %>%
  left_join( ., n.pop.merge, by = c( 'county_fips' = 'fips.ct' ) )


ggplot( ) + geom_sf( ct,
                     mapping = aes( fill = dens.q ),
                     color = NA, size = 0.5 )+
  scale_fill_manual( 'Quintile', values = c( "#DCE319FF", "#55C667FF", 
                                             "#238A8DFF", "#39568CFF","#440154FF" ) ) +
  theme_minimal( )
  theme( legend.position = 'none' )
