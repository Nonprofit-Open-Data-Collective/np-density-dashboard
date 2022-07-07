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

# final subset and basic plot
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

