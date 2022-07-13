###---------------------------------------------------
###   09-SHAPEFILES FOR SHINY APP
###---------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------------------------------------------------
# 
# In this script, we will generate some general shapefiles (to be stored as .rds) that will, hopefully, speed up the 
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
#
# Several shapefiles will be produced from this script (64 standard projection, 64 Dorling, and 1 for counties) and 
# will be stored in the Data-Rodeo folder within the directory
# ---------------------------------------------------------------------------------------------------------------------------------------------------------


library( tidyverse )
library( urbnmapr )         # state/county shapefiles
library( sf )               # simple features framework     
library( tigris )           # TIGER shapefiles
library( tidycensus )       # Census data queries
library( cartogram )        # Create sf objects for Dorling Cartograms
options( tigris_class = "sf" )
options( tigris_use_cache = TRUE )

source('/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Geospatial Dashboard/np-density-dashboard/R/helpers.R')


lf <- "/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Geospatial Dashboard/Large-Files-Bank"

setwd( lf )


# data containing NPO coordinates as points
npo <- readRDS( 'NONPROFITS-2014-2021v7.rds' )

# convert lat/long to a sf
npo.sf <- npo %>%
  filter( is.na( lat ) == F ) %>% # st_as_sf does not allow missing values in coordinate columns; n = 167 missing
  st_as_sf( coords = c( "lon", "lat" ), crs = 3395 ) # change projection accordingly



# pull tract-level shapefiles from `tigris`
t <- tracts( cb = TRUE ) # USA Census tract shapefiles

t <- transform( t, crs = 3395 ) # project onto compatible crs



# pull Census population (at the tract level) data using `tidycensus`
# merge with NPO dataset and count numbers of NPOs within tracts to create density metric

# loop through state codes as requesting all states at once will cause crash
state.codes <- unique( str_sub( t$GEOID, -11, -10 ) )

# remove FIPS codes for Mariana Islands, American Samoa, Guam, Virgin Islands (`tidycensus` won't return data for these)
state.codes <- state.codes[ !state.codes %in% c( "69", "60", "66", "78" ) ]

# loop through state FIPS codes
l <- list( )
for ( i in 1:length( state.codes ) ) {
  
  start.time <- Sys.time()
  
l[[i]] <- get_acs( geography = "tract", 
                variables = "B01003_001",       # TOTAL_POPULATION
                state = state.codes[i]  ) %>%   # This includes all states that we specify in the loop
  select( GEOID, estimate ) %>%
  rename( pop = estimate )

end.time <- Sys.time()

print( end.time - start.time)
print( paste0( "Iteration ", i, "/", length( state.codes ), " complete" ) ) 

}

d.1 <- do.call( "rbind", l ) # dataframe containing a column for tract GEOID and a column for its total population


d.2 <- npo.sf %>%
  group_by( GEOID ) %>%         # group by tract FIPS for subsequent computation
  mutate( n = ifelse( is.na( n() ) ==T, 0, n( ) ) ) %>%         # count number of rows per tract FIPS since rows are the nonprofits
  ungroup() %>%
  distinct( GEOID, n ) %>%      # retain only a dataframe of tract FIPS and the no. of nonprofits in them
  left_join( d.1, .) %>%        # join with Census population data
  mutate( dens = ( n / pop ) * 1000 ) %>%         # create NPO density metric (NPOs per 1k in the population)
  distinct( GEOID, pop , n, dens )                # keep unique rows (i.e., 1 for each tract)

# obtain MSA cartographic boundary shapefiles
m <- core_based_statistical_areas( cb = T )
m <- st_transform( m, crs = 3395 )             # compatible crs

# spatial join MSA names to tract-level data
sj <- m %>%
  rename (MSA = NAME ) %>%
  select( MSA, geometry ) %>%
  st_join( t, ., left = T ) %>%          # spatial left join
  left_join( ., d.2) %>%                 # append Census data
  distinct( )

length( unique( sj$GEOID ) ) # we have some duplicated rows

sj.dup <- sj[ which( duplicated( sj$GEOID ) ), ]
# View( sj.dup )

#### After diagnosing, it appears that some Census tracts are counted in more than 1 MSA. Thus, 
#### No need for any changes since we will be looking and subsetting based on MSA, not tract


## number of MSA's with > 500k population

keep.msa.500k <- sj %>%
  group_by( MSA ) %>%
  mutate( sum.pop = sum( pop )) %>%
  ungroup() %>%
  filter( sum.pop > 500000 ) %>%
  distinct( MSA ) 

# results in 130 MSAs

## number of MSAs with > 1.5m population

keep.msa.1.5m <- sj %>%
  group_by( MSA ) %>%
  mutate( sum.pop = sum( pop )) %>%
  ungroup() %>%
  filter( sum.pop > 1500000 ) %>%
  distinct( MSA ) 

# results in 64 MSAs


## Decision: for now, we will move forward with keeping only the Census tracts in those MSAs with > 1.5 m 
# in the population--this will keep the load times moving quickly in the dashboard

# subset census data in those 64 MSAs

d.3 <- sj %>%
  filter( MSA %in% keep.msa.1.5m$MSA )

## Save separate datasets according to MSA to later load into the dashboard app:

msas <- unique( d.3$MSA ) # dataset names will be according to MSA names

# text process them a bit to make them easier to store
msa.file <- msas %>%
  str_extract( ., "^.*(?=(\\,))" ) %>%   # first, extract everything before the comma (subsequently leading to the state)
  str_replace( ., "\\s", "-" )%>%        # replace white space with a "-"
  str_remove( ., "\\." ) %>%             # remove any "." from the strings
  str_replace( ., "\\s", "-" )           # run again because we still have some white spaces that have not been replaced

# now loop and make store shapefiles as .rds in "/Data-Rodeo"

setwd("../np-density-dashboard/Data-Rodeo")
# dir.create( "Dashboard-MSA-Data" )
# dir()
setwd("Dashboard-MSA-Data")
# dir.create( "Dorling-Shapefiles" ) # create subfolder to store Dorling Cartogram sf objects
# dir()


# create LOG file/metadata
log.head <- c( "Iteration", "MSA", "No.Tracts", "Pop", "No.NPO"," Save.Time" )

write( log.head, file = "MSA-Data-Log.txt", append = F, sep = "\t" )

d.3 <- st_transform( d.3, crs = 3395 ) # ensure data are in compatible projection before using cartogram fct


### NOTE: The following loop may take quite a while to run depending on your machine's specifications ###

# loop
for ( i in 1:length( msa.file ) ) {
  
  start.time <- Sys.time()
  
  # subset by MSA
  dat <- d.3[ which( d.3$MSA == msas[i] ), ]
  
  # do a spatial intersection to ensure Census tracts touching but outside the boundaries are excluded
  sub.area <- filter( m, grepl( msas[i], NAME ) ) # obtain cartographic boundary geometries
  
  s <- st_within( dat, sub.area ) # spatial overlay
  
  these <- map_lgl( s, function( x ) {        # logical for rows to keep
    if ( length( x ) == 1 ) {
      return( TRUE )
    } else {
      return( FALSE )
    }
  } )
  
  # final subset to remove boundary Census tracts outside MSA
  dat <- dat[ these, ]
  
  # save in parent dir
  saveRDS( dat , paste0( msa.file[i], "-MSA.rds" ))
  
  ## Dorling Cartogram transformation and save in subfolder
  
  setwd( "Dorling-Shapefiles" ) # save sf projection for Dorling Cartogram in a subfolder
  dat$pop.w <- dat$pop /  max( dat$pop, na.rm = T )   # standardizes it by max weight
  d.dorling <- cartogram_dorling( x = dat, weight = "pop.w" , k = 0.05 ) # projects to Dorling Cartogram
  
  saveRDS( d.dorling , paste0( msa.file[i], "-Dorling.rds" ))
  
  setwd( ".." ) # back up to parent dir
  
  end.time <- Sys.time()
  
  # update log/metadata
  Iteration <- i
  MSA <- as.character( levels( factor( dat$MSA ) )[1] )
  No.Tracts <- as.character( nrow( dat ) )
  Pop <- as.character( sum( dat$pop, na.rm = T ) )
  No.NPO <- as.character( sum( dat$n, na.rm = T ) )
  Save.Time <- as.character( end.time - start.time )
  log <- c( Iteration, MSA, No.Tracts, Pop, No.NPO, Save.Time)
  log <- paste( log, collapse = "\t" ) 
  write( log, file = "MSA-Data-Log.txt", append = T, sep = "\t" )
  
  print( end.time - start.time)
  print( paste0( "Iteration ", i, "/", length( msa.file ), " complete" ) ) 
  
}


# --------------------------------------------------------------------------------------------------------

## US Counties datasets

# obtain county geometries from `urbanmapr`
ct.sf <- get_urbn_map( map = "counties", sf = TRUE ) %>%
  st_transform( crs = 3395 )%>%
  rename( fips.ct = county_fips ) %>%
  left_join(., ( get_acs( geography = "county", # obtain population data from Census
                          variables = "B01003_001" )%>%   # TOTAL_POPULATION
                   select( fips.ct = GEOID, pop = estimate ) ) )     # select and rename


n.ct <- npo %>%
  group_by( fips.ct ) %>%
  mutate( n = ifelse( is.na( n() ) ==T, 0, n( ) ) ) %>% # count of NPO's within county FIPS
  distinct( fips.ct, n )

( ct <- 
    left_join( ct.sf, n.ct ) %>%
    st_transform( crs = 3395 ) %>%
    mutate( n = ifelse( is.na( n )==T, 0, n) )  %>%            # fix NAs for counties without new NPOs
  mutate( dens = ( n / pop )* 1000 ) %>%
  mutate( dens.q = factor( quant.cut( var = 'dens', x = 7 ,df = . ) ) ) )
  

## generate final county file and save
setwd( lf )

setwd("../np-density-dashboard/Data-Rodeo")
# dir.create( "Dashboard-County-Data" )

setwd( "Dashboard-County-Data" )

saveRDS( ct, "USA-Counties.rds")

# --------------------------------------------------------------------------------------------------------






# --------------------------------------------------------------------------------------------------------

## Counties Dorling Cartogram

ct.sf.tigris <- counties( cb=T ) %>%
  shift_geometry(  ) %>%      # shift and rescale HI, AK, and PR
  transform( crs = 3395 )    # project counties sf to compatible crs


# no. of NPOs by county 5-digit FIPS in the NPO dataset
n.ct <- npo %>%
  group_by( fips.ct ) %>%
  mutate( n = ifelse( is.na( n() ) ==T, 0, n( ) ) ) %>% # count of NPO's within county FIPS, if no rows for a county are detected, they are assigned 0 NPOs
  distinct( fips.ct, n ) 
# we should have 3186 counties in this dataset with no missing in the `n` column

# county population query from ACS
pop.ct <- get_acs( geography = "county", 
                   variables = "B01003_001" ) %>%   # TOTAL_POPULATION
  select( fips.ct = GEOID, pop = estimate )    # select and rename


# merge ACS data to NPO data where rows are counties identified by their 5-digit FIPS codes
dat <-  left_join( ct.sf.tigris, n.ct, by = c( "GEOID" = 'fips.ct') ) %>% # join count data to `tigris` shapefile
  rename( fips.ct = GEOID ) %>%             # rename identifier
  left_join(., pop.ct, by='fips.ct') %>%    # join to Census data
  mutate( n = ifelse( is.na( n )==T, 0, n ) ) %>%     # if a county has NA in its count of NPOs, it is because it was not present in the 1023-EZ data for that given year, meaning there were no filers in that county...thus, zero NPOs
  mutate( dens = ( n / pop ) * 1000 ) %>%           # calculate density metric
  mutate( pop.w = pop /  max( pop.ct$pop, na.rm=T ) ) %>%   # standardizes it
  st_transform( crs = 3395 ) %>%
  filter( is.na( pop.w )==F )  # remove FIPS in Guam, VI, and some Alaska counties without population estimates
# `cartogram_dorling` function will break if there are missing values in the weight parameter.
# no. of rows in this dataset should be ~3234 (one for each county in the US--actually around 3221 since we removed territories)

ct.dorling <- cartogram_dorling( x = dat, weight = "pop.w" , k = 0.8 )  # k parameter was increased to augment circle size

# dir.create( "Dorling-Shapefiles")

setwd( "Dorling-Shapefiles" )

saveRDS( ct.dorling, "USA-Counties-Dorling.rds")
# --------------------------------------------------------------------------------------------------------




# --------------------------------------------------------------------------------------------------------

## Nonprofits by year shapefiles/data

setwd( lf )
setwd("../np-density-dashboard/Data-Rodeo/Dashboard-County-Data/")
#dir.create( "By-Year")

yr.levels <- levels( factor( npo$YR ) )

 ct.sf.urbn <- get_urbn_map( map = "counties", sf = TRUE ) %>%
   st_transform( crs = 3395 )%>%
   rename( fips.ct = county_fips ) %>%
   left_join(., ( get_acs( geography = "county", 
                  variables = "B01003_001" )%>%   # TOTAL_POPULATION
  select( fips.ct = GEOID, pop = estimate ) ) )    # select and rename


for(i in 1: length( yr.levels ) ) {
  start.time <- Sys.time()
  setwd( lf )
  setwd("../np-density-dashboard/Data-Rodeo/Dashboard-County-Data/By-Year" )
  
  n.ct.yr <- npo %>%
  filter( YR == yr.levels[i] ) %>%
  group_by( fips.ct ) %>%
  mutate( n = ifelse( is.na( n() ) ==T, 0, n( ) ) ) %>% # count of NPO's within county FIPS
  distinct( fips.ct, n, YR )
  
  ( ct <- 
      left_join( ct.sf.urbn, n.ct.yr ) %>%
      st_transform( crs = 3395 ) %>%
    mutate( n = ifelse( is.na( n )==T, 0, n) )  %>%            # fix NAs for counties without new NPOs
    mutate( dens = ( n / pop )* 1000 ) )%>%
    filter( is.na( pop )==F )

    saveRDS( paste0( "USA-Counties-", yr.levels[i],".rds") )
  
  end.time <- Sys.time()
  
  print( end.time - start.time)
  print( paste0( "Iteration ", i, "/", length( yr.levels ), " complete" ) ) 
  
  
}
 # --------------------------------------------------------------------------------------------------------
 
 
 
 
 
 # --------------------------------------------------------------------------------------------------------
 
## Nonprofits by year shapefiles/data (Dorling Cartograms)

 
 # NEED TO RUN THIS PRIOR TO LOOP:
 #
 # yr.levels <- levels( factor( npo$YR ) )
 # lines 284-286
 # ct.sf.tigris <- counties( cb=T ) %>%
 #   shift_geometry(  ) %>%      # shift and rescale HI, AK, and PR
 #   transform( crs = 3395 )    # project counties sf to compatible crs
 #
 # lines 297-299
 # county population query and merge
 # pop.ct <- get_acs( geography = "county", 
 #                    variables = "B01003_001" ) %>%   # TOTAL_POPULATION
 #   select( fips.ct = GEOID, pop = estimate )    # select and rename
 
 setwd( lf )
 setwd("../np-density-dashboard/Data-Rodeo/Dashboard-County-Data/Dorling-Shapefiles")
 # dir.create( "Dorling-By-Year" )
 setwd( "Dorling-By-Year" )
 
for( i in 1:length( yr.levels ) ) {
  
  start.time <- Sys.time()
  
 # no. of NPOs by county 5-digit FIPS in the NPO dataset
 n.ct <- npo %>%
   filter( YR == yr.levels[i] ) %>%                       # filter by year
   group_by( fips.ct ) %>%
   mutate( n = ifelse( is.na( n() ) ==T, 0, n( ) ) ) %>% # count of NPO's within county FIPS
   distinct( fips.ct, n, YR )
 # no. of rows in this data should reflect the number of counties for which we have data for
 # i.e., less than 3142 counties in the USA since there weren't filers in every county
 
 # merge ACS data to NPO data where rows are counties identified by their 5-digit FIPS codes

 dat.yr <-  left_join( ct.sf.tigris, n.ct, by = c( "GEOID" = 'fips.ct') ) %>% # join count data to `tigris` shapefile
   rename( fips.ct = GEOID ) %>%             # rename identifier
   left_join(., pop.ct, by='fips.ct') %>%    # join to Census data
   mutate( n = ifelse( is.na( n )==T, 0, n ) ) %>%     # if a county has NA in its count of NPOs, it is because it was not present in the 1023-EZ data for that given year, meaning there were no filers in that county...thus, zero NPOs
     mutate( dens = ( n / pop ) * 1000 ) %>%           # calculate density metric
     mutate( pop.w = pop /  max( pop.ct$pop, na.rm=T ) ) %>%   # standardizes it
   st_transform( crs = 3395 ) %>%
   filter( is.na( pop.w )==F )  # remove FIPS in Guam, VI, and some Alaska counties without population estimates
 # `cartogram_dorling` function will break if there are missing values in the weight parameter.
 # no. of rows in this dataset should be ~3234 (one for each county in the US)
 
 yr.dorling <- cartogram_dorling( x = dat.yr, weight = "pop.w" , k = 0.8 )  # k parameter was increased to augment circle size
 

 saveRDS( yr.dorling, paste0("USA-Counties-Dorling-", c( 2014:2021 )[i],".rds" ) )
 
 end.time <- Sys.time()
 
 print( end.time - start.time)
 print( paste0( "Iteration ", i, "/", length( yr.levels ), " complete" ) ) 
}
# --------------------------------------------------------------------------------------------------------

 
 

 
# --------------------------------------------------------------------------------------------------------
## Shapefiles for Leaflet Maps
 
## Cumulative county data
 
setwd( lf )
 
setwd("../np-density-dashboard/Data-Rodeo/Dashboard-County-Data" )
cnties <- readRDS( "USA-Counties.rds" )
 
# Cumulative County Map (2014-2021)

 ct.leaf <- counties( cb = T) %>%
   left_join(., 
             data.frame( cnties ) %>% select( fips.ct, n, dens ) 
             , by = c( "GEOID" = "fips.ct" ) ) %>% # join Tigris county map to county data
   
   mutate( dens = round( dens, digits = 2 ), # round density metric to keep tidy on Leaflet popup
           popup = paste(sep = "<br/>",      # create popup script for Leaflet map
                         paste0("<b>",NAMELSAD,", ", STUSPS, "</b>"),
                         paste0( "<i># New Nonprofits<i>: ", n ),
                         paste0( "<i>Density New Nonprofits<i>: ", dens, " nonprofits / 1,000 inhabitants" ) ) )
 
 
 # dir.create( "Leaflet-Shapefiles")
 setwd( "Leaflet-Shapefiles" )
 saveRDS( ct.leaf, "USA-Counties-Leaflet.rds")
 
 # Now, create county leaflet maps by year which will summarize new nonprofits per year
 # dir.create( "Leaflet-By-Year" )
 
 setwd("../By-Year")

  for ( i in 1:length( dir() ) ) {
    
    start.time <- Sys.time()
    
    ct.in <- readRDS( dir()[i] )
    
    ct.leaf <- counties( cb = T) %>%
      left_join(., 
                data.frame( ct.in ) %>% select( fips.ct, n, dens ) 
                , by = c( "GEOID" = "fips.ct" ) ) %>% # join Tigris county map to county data
      
      mutate( dens = round( dens, digits = 2 ), # round density metric to keep tidy on Leaflet popup
              popup = paste(sep = "<br/>",      # create popup script for Leaflet map
                            paste0("<b>",NAMELSAD,", ", STUSPS, "</b>"),
                            paste0( "<i># New Nonprofits<i>: ", n ),
                            paste0( "<i>Density New Nonprofits<i>: ", dens, " nonprofits / 1,000 inhabitants" ) ) )
    
    
  
    setwd( "../Leaflet-Shapefiles/Leaflet-By-Year" ) # save directory
    
    saveRDS( ct.leaf, paste0( "USA-Counties-Leaflet-", c(2014:2021)[i], ".rds" ) )
    
    setwd("../../By-Year") # back up to yearly data storage directory
    
    end.time <- Sys.time()
    
    print( end.time - start.time)
    print( paste0( "Iteration ", i, "/", length( dir() ), " complete" ) ) 
  }
# --------------------------------------------------------------------------------------------------------
 
 
 