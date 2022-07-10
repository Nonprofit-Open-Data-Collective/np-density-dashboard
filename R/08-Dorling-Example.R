###---------------------------------------------------
###   08-DORLING CARTOGRAM EXAMPLE
###---------------------------------------------------

# In this script, I will transform some static maps done in the previous script (07-Static-Map-Making.R) and
# Dorling Cartograms. This example will be a prototype to then scale up to all MSA regions and subsequetly
# into the dashboard. The idea is that the dasbboard interface will allow users to view the MSA as a chloropleth
# and then as a Dorling Cartogram.

## repurposing code from: https://ds4ps.org/cpp-529-spr-2021/LABS/lab-04-instructions.html#step-4-transform-the-shapefile-into-a-dorling-cartogram
library( sp )          # work with shapefiles
library( sf )   
library( tigris )
library( tidyverse )
library( cartogram )
library( ggpubr )
library( urbanmapr )
library( tidycensus )
# import helper functions
source('/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Geospatial Dashboard/np-density-dashboard/R/helpers.R')

## Use Seattle-Bellevue as example:



wa <- tracts( "WA", cb = TRUE ) # Washington Census Tract TIGER shapefiles

cb <- core_based_statistical_areas( cb = TRUE ) # fetches cartographic boundary files for MSAs
sea <- dplyr::filter( cb, grepl( "Seattle", NAME ) ) # match "Seattle" and return boundary files

p1 <- wa[ sea, ] # subset tracts from within ( or touching ) MSA boundary

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

# final subset and basic plot of Seattle Bellevue MSA
d <- wa[ these, ]
ggplot()+
  geom_sf( data = d ) + 
  theme_minimal()


## append population data from census

# invisible( census_api_key( "apigoeshere", install = TRUE,
#                         overwrite = TRUE ) )
# invisible( readRenviron( "~/.Renviron" ) )
# # search variable names
# v17 <- load_variables( 2020, "acs5", cache = TRUE )
# View( v17 )


d <- (pop.ct <- get_acs( geography = "tract", 
                        variables = "B01003_001",       # TOTAL_POPULATION
                        state = "53") %>%         # restrict to Seattle Region
        select( GEOID, estimate ) %>%
        rename( POP = estimate ) ) %>%
  left_join( d, .)


## append 1023-EZ data

lf <- "/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Geospatial Dashboard/Large-Files-Bank"

setwd( lf )
np <- readRDS( "NONPROFITS-2014-2021v7.rds" )

# count rows by FIPS, create density measure
d <- ( np %>%
  group_by( GEOID ) %>%
  mutate( n = n( ) ) %>%     # count number of nonprofits per census tract
  distinct( GEOID, n ) ) %>%
  left_join( d, . ) %>%           # join NPO/GEOID count data to sf object
  mutate( dens = n / POP ) %>%
  mutate( dens.q = factor( quant.cut( var = 'dens', x = 5 ,df = . ) ) )

class( d )

sapply( d %>% select( POP, n, dens, dens.q ), function( x ) sum( is.na(x ) ) ) # 294 tracts w/o any npo's


# project map onto new coordinate system and remove empty tracts (`cartogram_dorling` likes the
# EPSG:3395 coordinate system--it did not like me passing the NAD83 projection)

d <- st_transform( d, crs=3395)
d <- d[ d$POP != 0 & (! is.na( d$POP )) , ] # remove tracts with 0 population

# convert census tract polygons to dorling cartogram
# no idea why k=0.03 works, but it does - default is k=5 ##what does the k parameter do?
d$pop.w <- d$POP /  max( d$POP )   # standardizes it to max of 1.5
d.dorling <- cartogram_dorling( x=d, weight="pop.w" , k=0.05 )

# plot
dor <- ggplot( d.dorling ) +
  geom_sf(mapping = aes( fill = dens.q ),
          color = NA, size = 0.5 )+
  scale_fill_manual( 'Quintile', values = c( "#DCE319FF", "#55C667FF", 
                                             "#238A8DFF", "#39568CFF","#440154FF" ) ) +
  theme_minimal() 


chloro <- ggplot()+
  geom_sf( data= d,
           mapping = aes( fill = dens.q ),
           color = NA, size = 0.5 ) +
  scale_fill_manual( 'Quintile', values = c( "#DCE319FF", "#55C667FF", "#238A8DFF",
                                             "#39568CFF","#440154FF" ) ) +
  theme_minimal( ) 

ggarrange( chloro, dor, nrow=1, ncol=2, common.legend = T, legend='bottom')





## US Counties Example


ct.sf <- counties( cb=T ) %>%
  shift_geometry(  ) %>%      # shift and rescale HI, AK, and PR
  transform( crs = 3395 )    # project counties sf to compatible crs


# no. of NPOs by county 5-digit FIPS in the NPO dataset
n.ct <- np %>%
  group_by( fips.ct ) %>%
  mutate( n = ifelse( is.na( n() ) ==T, 0, n( ) ) ) %>% # count of NPO's within county FIPS
  distinct( fips.ct, n ) 


# county population query and merge
pop.ct <- get_acs( geography = "county", 
                   variables = "B01003_001" ) %>%   # TOTAL_POPULATION
  select( fips.ct = GEOID, pop = estimate )    # select and rename


# merge ACS data to NPO data where rows are counties identified by their 5-digit FIPS codes
dat <- st_as_sf( pop.ct %>%
                   mutate( fips.ct =  fips.ct  ) %>%
                   left_join( n.ct, . ) %>%
                   mutate( dens = ( n / pop ) * 1000 ) %>%
                   mutate( pop.w = pop /  min( pop.ct$pop, na.rm=T ) ) %>%   # ctandardizes it to max of 1.5
                   left_join( ., ct.sf, by = c( "fips.ct" = 'GEOID') ) ) %>%
  st_transform( crs = 3395 ) %>%
  filter( is.na( pop.w )==F ) # remove FIPS in Guam, VI, and some Alaska counties without population estimates
# `cartogram_dorling` function will break if there are missing values in the weight parameter

ct.dorling <- cartogram_dorling( x = dat, weight = "pop.w" , k = 0.8 )  # k parameter was increased to augment circle size

ggplot(  )  +
  geom_sf(d.dorling, mapping = aes( fill = dens ),  color=NA) +
  theme_minimal()

# overlay state shapefiles
sts <- states( cb= T ) %>%
  shift_geometry() %>%
  st_transform( crs = 3395) %>%
  filter( STUSPS %in% c( state.abb, 'PR' ) ) 

ggplot(sts) +
  geom_sf(  )


st.u<-urbnmapr::get_urbn_map( map = "states", sf = T) %>%
  filter(state_abbv %in% c( "AK", "HI", "PR" ) ) %>%
  st_transform( crs = 3395)


nat <- tigris::states( cb = T) %>%
  shift_geometry()

ggplot(nat)+
  geom_sf()

ggplot( sts ) +
  geom_sf ( fill = 'white' ) +
  geom_sf(d.dorling, mapping = aes( fill = dens ),
          color = NA, size = 0.5 ) + theme_minimal()
   
  theme_minimal()
  scale_fill_manual( 'Quintile', values = c( "#DCE319FF", "#55C667FF", 
                                             "#238A8DFF", "#39568CFF","#440154FF" ) ) +
  theme_minimal()
  

  library(tmap)
  
  
  tm_shape(sts)+tm_polygons("geometry", fill= "black")

  
    st <- tigris::states(cb=T) %>%
    left_join(., get_acs( geography = "state", 
                          variables = "B01003_001" ) ) %>%
    rename( pop = estimate ) %>%
    mutate( maxpop = max( pop, na.rm = T )) %>%
    mutate( pop.w = pop /maxpop ) %>%
    filter( is.na( pop.w ) ==F ) %>%
    st_transform( crs = 3395)
  
  sd.dorling <- cartogram_dorling( x = st, weight = "pop.w" , k=0.05 )
  

  
  ## US States example
  
  st.sf <- states( cb=T ) %>%
  shift_geometry(  ) %>%      # shift and rescale HI, AK, and PR
    transform( crs = 3395 )    # project counties sf to compatible crs
  
  
  # no. of NPOs by county 5-digit FIPS in the NPO dataset
  n.st <- np %>%
    group_by( STATEFIPS ) %>%
    mutate( n = ifelse( is.na( n() ) ==T, 0, n( ) ) ) %>% # count of NPO's within county FIPS
    distinst( STATEFIPS, n ) 
  
  
  # county population query and merge
  pop.st <- get_acs( geography = "state", 
                     variables = "B01003_001" ) %>%   # TOTAL_POPULATION
    selest( STATEFIPS = GEOID, pop = estimate )    # selest and rename
  
  
  # merge ACS data to NPO data where rows are counties identified by their 5-digit FIPS codes
  dat <- st_as_sf( pop.st %>%
                     mutate( STATEFIPS =  STATEFIPS  ) %>%
                     left_join( n.st, . ) %>%
                     mutate( dens = ( n / pop ) * 1000 ) %>%
                     mutate( pop.w = pop /  min( pop.st$pop, na.rm=T ) ) %>%   # standardizes it to max of 1.5
                     left_join( ., st.sf, by = c( "STATEFIPS" = 'GEOID') ) ) %>%
    st_transform( crs = 3395 ) %>%
    filter( is.na( pop.w )==F ) # remove FIPS in Guam, VI, and some Alaska counties without population estimates
  # `cartogram_dorling` funstion will break if there are missing values in the weight parameter
  
  
  d.dorling <- cartogram_dorling( x = dat, weight = "pop.w" , k=1.5 )  # k parameter was increased to augment circle size
  
  ggplot(  )  +
    geom_sf(d.dorling, mapping = aes( fill = dens ),  color=NA) +
    theme_minimal()
  
  
