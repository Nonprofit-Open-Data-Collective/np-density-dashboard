###---------------------------------------------------
###   09-DATASETS FOR SHINY APP
###---------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------------------------------------------------
# 
# In this script, we will generate some general shaepfiles (to be stored as .rds) that will, hopefully, speed up the 
# data import process on the Shiny app we construct. Several files will be generated and stored in the "/Data-Rodeo"
# folder of our directory. The idea will be to generate data files for both Dorling Cartograms and general maps that are
# not resized. In every scenario, we will have files at the level of census tracts and counties.
# 
# These datasets will also contain metrics that we will plot in our Shiny app. Thus, this script may evolve and be modified
# as we add more metrics to the dashboard.
#
# We will import data from a variety of packages. With regard to data in our respository, we will import the 
# "NONPROFITS-2014-2021v7.rds" file, created in a previous script ( "07-NPO-Static-Map-Example.R" )
#
# ---------------------------------------------------------------------------------------------------------------------------------------------------------


library( tidyverse )
library( urbnmapr )         # state/county shapefiles
library( sf )               # simple features framework     
library( tigris )           # TIGER shapefiles
library( tidycensus )       # Census data queries
options( tigris_class = "sf" )
options( tigris_use_cache = TRUE )


lf <- "/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Geospatial Dashboard/Large-Files-Bank"

setwd( lf )


# data containing NPO coordinates as points
npo <- readRDS( 'NONPROFITS-2014-2021v7.rds' )

# convert lat/long to a sf
npo.sf <- npo %>%
  filter( is.na( lat ) == F ) %>% # st_as_sf does not allow missing values in coordinate columns; n = 167 missing
  st_as_sf( coords = c( "lon", "lat" ), crs = 3395 ) # change projection accordingly

# obtain county geometries from `urbanmapr`
ct.sf <- get_urbn_map( map = "counties", sf = TRUE )

st_crs( ct.sf ) 

# project counties sf to compatible crs
ct.sf <- transform( ct.sf, crs = 3395 )



# pull tract-level shapefiles from `tigris`
t <- tracts( cb = TRUE ) # USA Census tract shapefiles

t <- transform( t, crs = 3395 ) # project onto compatible crs



# pull Census population (at the tract level) data from `tidycensus`
# merge with NPO dataset and count numbers of NPOs within tracts to create density metric

# loop through state codes as requesting all states at once will cause crash
state.codes <- unique( str_sub( t$GEOID, -11, -10 ) )

# remove FIPS codes for Mariana Islands, American Samoa, Guam, Virgin Islands (tidycensus won't return data for these)
state.codes <- state.codes[ !state.codes %in% c( "69", "60", "66", "78" ) ]

l <- list()
for (i in 1:length( state.codes ) ) {
  
  start.time <- Sys.time()
  
l[[i]] <- get_acs( geography = "tract", 
                variables = "B01003_001",       # TOTAL_POPULATION
                state = state.codes[i]  ) %>%   # This includes all states in the Tigris dataset
  select( GEOID, estimate ) %>%
  rename( pop = estimate )

end.time <- Sys.time()

print( end.time - start.time)
print( paste0( "Iteration ", i, "/", length( state.codes ), " complete" ) ) 

}

d.1 <- do.call( "rbind", l ) # dataframe containing a column for tract GEOID and a column for its total population


d.2 <- npo.sf %>%
  group_by( GEOID ) %>%         # group by tract FIPS for subsequent computation
  mutate( n = n() ) %>%         # count number of rows per tract FIPS since rows are the nonprofits
  ungroup() %>%
  distinct( GEOID, n ) %>%      # retain only a dataframe of tract FIPS and the no. of nonprofits in them
  left_join( d.1, .) %>%        # join with Census population data
  mutate( dens = ( n / pop ) * 1000 ) %>%         # NPO create density metric (NPOs per 1k in the population)
  distinct( GEOID, pop , n, dens )                # keep unique rows (i.e., 1 for each tract)
  mutate( dens.q = factor( quant.cut( var = 'dens', x = 5 ,df = . ) ) )       # bin into quintiles





