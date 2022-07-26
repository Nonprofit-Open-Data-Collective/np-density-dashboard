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



# --------------------------------------------------------------------------------------------------------

## Metropolitan Statistical Areas (MSAs) Shapefiles and Dorling  Cartogram Shapefiles

# convert lat/long to a sf
npo.sf <- npo %>%
  filter( is.na( lat ) == F ) %>% # st_as_sf does not allow missing values in coordinate columns; n = 167 missing
  st_as_sf( coords = c( "lon", "lat" ), crs = 3395 ) # change projection accordingly



# pull tract-level shapefiles from `tigris`
t <- tracts( cb = TRUE ) # USA Census tract shapefiles

t <- st_transform( t, crs = 3395 ) # project onto compatible crs



# pull Census population (at the tract level) data using `tidycensus`
# merge with NPO dataset and count numbers of NPOs within tracts to create density metric

# loop through state codes as requesting all states at once will cause crash
state.codes <- unique( str_sub( t$GEOID, -11, -10 ) )

# remove FIPS codes for Mariana Islands, American Samoa, Guam, Virgin Islands (`tidycensus` won't return data for these)
state.codes <- state.codes[ !state.codes %in% c( "69", "60", "66", "78" ) ]

# loop through state FIPS codes
l <- list( )                      # initialize null list to store state results
ys <- c( 2014:2020, 2020 )        # years to store data from 2014-2020 (2021 will have to use 2020 data since 2021 data is not available)

for ( i in 1:length( state.codes ) ) {   # index on state FIPS code
  
  start.time <- Sys.time()
  
  
  k <- list() # initialize null list to store yearly results
  for ( j in 1: length( c( ys ) ) ){
k[[j]] <- get_acs( geography = "tract", year = ys[j],
                variables = c( pop = "B01003_001", pov = "B17001_002", med.income = "B19013_001",
                               female = "B01001_026", male = "B01001_002" ),    
                state = state.codes[i]  ) %>%   # This includes all states that we specify in the loop
  pivot_wider( id_cols = GEOID, names_from = variable, values_from = estimate ) %>%   # Pivot data since tidycensus returns data table as a long data frame
  mutate( poverty.rate = ( pov / pop )*100,
          perc.female = ( female / pop )*100,
          perc.male = ( male / pop )*100 ,
          year = ys[j] ) %>%
  select( GEOID, pop, poverty.rate, med.income, perc.female, perc.male, year ) 
  }
  
l[[i]] <- do.call( "rbind", k )
  
end.time <- Sys.time()

print( end.time - start.time)
print( paste0( "Iteration ", i, "/", length( state.codes ), " complete" ) ) 

}

setwd( lf )
setwd( "../np-density-dashboard/Data-Wrangled")
# dir.create( "01-Temp" )

d.1 <- do.call( "rbind", l ) %>% mutate( year = as.character( year ) ) # dataframe containing a column for tract GEOID and columns for its yearly census data
# there are ~614796 rows in this dataset

# saveRDS( d.1, "01-MSA-Census.rds" )

setwd( "01-Temp" )
d.1 <- readRDS( "01-MSA-Census.rds" )


# save it for later access without rereunning code:


ys.np <- list( 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, c( 2014:2021 ) )       # we will index on the years we will subset
ys.np.nm <- c( "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2020", "cum" )
ys.np.cen <- list( "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2020", "2020" )

b <- list()
for ( i in 1: length( ys.np ) ){
  
  start.time <- Sys.time()
  
  dat.yr <- filter( d.1, year %in% ys.np.cen[[i]] )   # subset data by year
  
  b[[i]] <- npo.sf %>%
    filter( YR %in% ys.np[[i]] ) %>%                # group by tract FIPS for subsequent computation
    left_join( dat.yr, .) %>%                       # join with Census population data 
    group_by( GEOID ) %>%
    mutate( n = ifelse( is.na( n() ) ==T, 0, n( ) ) ) %>%          # count number of rows per tract FIPS since rows are the nonprofits
    ungroup() %>%
    mutate( n = ifelse( is.na( n ), 0, n ),                # tracts that have an NA (i.e., tracts not represented in the NCCS data) get allocated 0 new NPOs 
            dens = ( n / pop ) * 1000 ,                    # create NPO density metric (NPOs per 1k in the population)
            dens = ifelse( is.na( dens ), 0, 
                           ifelse( pop == 0 , 0, dens ) ),
            year = ys.np.nm[i] ) %>%  # those still having a "NA" for the density metric are tracts that have 0 population and 0 number of nonprofits (n = 763 instances of 0/0)       
    distinct( GEOID, pop, poverty.rate, med.income, 
              perc.female, perc.male, n, year, YR, dens )                # keep unique rows (i.e., 1 for each tract)
  # this dataset should have same number of rows as d.1 (all census tracts) and no missings
  # in any of the columns
  
  end.time <- Sys.time()
  
  print( end.time - start.time)
  print( paste0( "Iteration ", i, "/", length( ys.np ), " complete" ) ) 
  
}

# bind into single dataset and merge yearly NPO data to yearly census data 
d.2 <- do.call( "rbind", b )

# since we use 2020 census data for the 2021 NPOs, we need to fix the discrepancy in the year variables
d.2 <- d.2 %>%
  mutate( year = ifelse( year == "2020" & YR == "2021", "2021", year ) ) %>%
  select( -YR )


# save for later use 
setwd( lf )
setwd( "../np-density-dashboard/Data-Wrangled/01-Temp")
# saveRDS( d.2, "02-MSA-NPO.rds" )

d.2 <- readRDS( "02-MSA-NPO.rds" )


# obtain MSA cartographic boundary shapefiles
m <- core_based_statistical_areas( cb = T )
m <- st_transform( m, crs = 3395 )             # compatible crs

# spatial join MSA names to tract-level data
sj <- m %>%
  rename (MSA = NAME ) %>%
  select( MSA, geometry ) %>%
  st_join( t, ., left = T ) %>%          # spatial left join
  distinct( )

length( unique( sj$GEOID ) ) # we have some duplicated rows

sj.dup <- sj[ which( duplicated( sj$GEOID ) ), ]

#### After diagnosing, it appears that some Census tracts are counted in more than 1 MSA. Thus, 
#### No need for any changes since we will be looking and subsetting based on MSA, not tract


### Select MSAs to Include in Map ###

## number of MSA's with > 500k population in 2020

keep.msa.500k <- sj %>%
  left_join( ., d.1 %>% filter( year == "2020") %>% select( GEOID, pop ) %>% distinct( GEOID, pop ) ) %>%
  group_by( MSA ) %>%
  mutate( sum.pop = sum( pop )) %>%
  ungroup() %>%
  filter( sum.pop > 500000 ) %>%
  distinct( MSA ) 

# results in 130 MSAs

## number of MSAs with > 1.5m population in 2020

keep.msa.1.5m <- sj %>%
  left_join( ., d.1 %>% filter( year == "2020") %>% distinct( GEOID, pop ) ) %>%
  group_by( MSA ) %>%
  mutate( sum.pop = sum( pop ) ) %>%
  ungroup() %>%
  filter( sum.pop > 1500000 ) %>%
  distinct( MSA ) 

# results in 43 MSAs


## Decision: for now, we will move forward with keeping only the Census tracts in those MSAs with > 1.5 m 
# in the population--this will keep the load times moving quickly in the dashboard

## subset tract population and accompanying shapefile in those 43 MSAs


d.3 <- sj %>%
  filter( MSA %in% keep.msa.1.5m$MSA ) 


# merge with tract-level data 

d.4 <- st_as_sf( left_join(d.2, d.3, by = "GEOID") %>%      # merge shapefile with tract data
                   filter( MSA %in% d.3$MSA ) )                     # keep only tracts in the 43 selected MSAs

# text process them a bit to make them easier to store and for showing on the Shiny App
d.4$MSA <- d.4$MSA %>%
  str_extract( ., "^.*(?=(\\,))" ) %>%   # first, extract everything before the comma (subsequently leading to the state)
  str_replace( ., "\\s", "-" )%>%        # replace white space with a "-"
  str_remove( ., "\\." ) %>%             # remove any "." from the strings
  str_replace( ., "\\s", "-" )           # run again because we still have some white spaces that have not been replaced

setwd( lf )
setwd("../np-density-dashboard/Data-Rodeo")
# dir.create( "Dashboard-MSA-Data" )
# dir()
setwd("Dashboard-MSA-Data")
# dir.create( "Dorling-Shapefiles" ) # create subfolder to store Dorling Cartogram sf objects
# dir()

saveRDS( d.4, "USA-MSAs.rds" )

# create Dorling Cartogram file
d.4 <- st_transform( d.4, crs = 3395 ) # ensure data are in compatible projection before using cartogram fct
# dir.create( "Dorling-Shapefiles" )

setwd( "Dorling-Shapefiles" ) # save sf projection for Dorling Cartogram in a subfolder
d.4$pop.w <- d.4$pop /  max( d.4$pop, na.rm = T )   # standardizes it by max weight
d.dorling.msa <- cartogram_dorling( x = d.4, weight = "pop.w" , k = 0.05 ) # projects to Dorling Cartogram

saveRDS( d.dorling.msa , "USA-MSAs-Dorling.rds" )

         
# --------------------------------------------------------------------------------------------------------





# --------------------------------------------------------------------------------------------------------

## US Counties datasets (cumulative 2014-2021)

# census data
ct.acs <- get_acs( geography = "county", # obtain population data from Census
                   variables = c( pop = "B01003_001", pov = "B17001_002", med.income = "B19013_001",
                                  female = "B01001_026", male = "B01001_002" ) ) %>%   # This includes all states that we specify in the loop
  pivot_wider( id_cols = GEOID, names_from = variable, values_from = estimate ) %>%   # Pivot data since tidycensus returns data table as a long data frame
  mutate( poverty.rate = ( pov / pop )*100,
          perc.female = ( female / pop )*100,
          perc.male = ( male / pop )*100 ) %>%
  select( GEOID, pop, poverty.rate, med.income, perc.female, perc.male ) 

# obtain county geometries from `urbanmapr` and merge to census data
ct.sf <- get_urbn_map( map = "counties", sf = TRUE ) %>%
  st_transform( crs = 3395 )%>%
  rename( fips.ct = county_fips ) %>%
  left_join(., ct.acs, by = c( "fips.ct" = "GEOID" ) )


n.ct <- npo %>%
  group_by( fips.ct ) %>%
  mutate( n.cum = ifelse( is.na( n() ) ==T, 0, n( ) ) ) %>% # count of NPO's within county FIPS
  distinct( fips.ct, n.cum )

( ct <- 
    left_join( ct.sf, n.ct ) %>%
    st_transform( crs = 3395 ) %>%
    mutate( n.cum = ifelse( is.na( n.cum )==T, 0, n.cum) )  %>%            # fix NAs for counties without new NPOs
    mutate( dens.cum = ( n.cum / pop )* 1000 ) %>%
    filter( is.na( dens.cum )==F ))
  

## generate final county file and save
setwd( lf )

setwd("../np-density-dashboard/Data-Rodeo")
# dir.create( "Dashboard-County-Data" )

setwd( "Dashboard-County-Data" )

saveRDS( ct, "USA-Counties.rds")

# --------------------------------------------------------------------------------------------------------


# --------------------------------------------------------------------------------------------------------

## Append Yearly County NPO Data To USA-Counties Dataset/Shapefile

setwd( lf )
setwd("../np-density-dashboard/Data-Rodeo/Dashboard-County-Data/")

ct <- readRDS("USA-Counties.rds") # read in counties data created above

yr.levels <- levels( factor( npo$YR ) )


for(i in 1: length( yr.levels ) ) {
  start.time <- Sys.time()
  setwd( lf )
  setwd("../np-density-dashboard/Data-Rodeo/Dashboard-County-Data/" )
  
  count.nm <- paste0( "n.", yr.levels[i] ) # variable name for NPO count by year
  
  dens.nm <- paste0( "dens.", yr.levels[i] ) # variable name for density by year
  
  
  n.ct.yr <- npo %>%
    filter( YR == yr.levels[i] ) %>%
    group_by( fips.ct ) %>%
    mutate( !!rlang::sym( count.nm ) := ifelse( is.na( n() ) ==T, 0, n( ) ) ) %>% # count of NPO's within county FIPS
    distinct( fips.ct, !!rlang::sym( count.nm ) )
  
  
  ( ct <- 
      left_join( ct, n.ct.yr ) %>%
      st_transform( crs = 3395 ) %>%
      mutate( !!rlang::sym( count.nm ) := ifelse( is.na( !!rlang::sym( count.nm ) )==T, 0, !!rlang::sym( count.nm ) ) )  %>%            # fix NAs for counties without new NPOs
      mutate( !!rlang::sym( dens.nm ) := ( !!rlang::sym( count.nm ) / pop )* 1000 ) %>%
      filter( is.na( !!rlang::sym( count.nm ) )==F ) ) %>%
    saveRDS( "USA-Counties.rds")
  
  end.time <- Sys.time()
  
  print( end.time - start.time)
  print( paste0( "Iteration ", i, "/", length( yr.levels ), " complete" ) ) 
  
  
}

# now put into long format and save final version
setwd( lf )
setwd("../np-density-dashboard/Data-Rodeo/Dashboard-County-Data/")

st_as_sf( st_drop_geometry( ct ) %>% 
                      select( fips.ct, starts_with( "n.") )%>%                     # pivot on count columns only
                      pivot_longer( cols = !fips.ct, 
                                    names_to = 'year', 
                                    names_prefix = "n.", 
                                    values_to = "n" ) %>%
                      left_join(., st_drop_geometry( ct ) %>%                      # join the long format density and count tables
                                  select( fips.ct, starts_with( "dens.") )%>%      # pivot on density columns only
                                  pivot_longer( cols = !fips.ct, 
                                                names_to = 'year', 
                                                names_prefix = "dens.", 
                                                values_to = "dens" ) ) %>%
                      left_join(., ct%>% select( !starts_with( "n" ) & !starts_with( "dens" ) ),
                                by = "fips.ct") ) %>%
  saveRDS( "USA-Counties.rds" )

# --------------------------------------------------------------------------------------------------------





# --------------------------------------------------------------------------------------------------------

## Counties Dorling Cartogram

ct.sf.tigris <- counties( cb=T ) %>%
  shift_geometry(  ) %>%      # shift and rescale HI, AK, and PR
  transform( crs = 3395 )    # project counties sf to compatible crs


# no. of NPOs by county 5-digit FIPS in the NPO dataset
n.ct <- npo %>%
  group_by( fips.ct ) %>%
  mutate( n.cum = ifelse( is.na( n() ) ==T, 0, n( ) ) ) %>% # count of NPO's within county FIPS, if no rows for a county are detected, they are assigned 0 NPOs
  distinct( fips.ct, n.cum ) 
# we should have 3186 counties in this dataset with no missing in the `n` column

# ACS query
pop.ct <- get_acs( geography = "county", # obtain population data from Census
         variables = c( pop = "B01003_001", pov = "B17001_002", med.income = "B19013_001",
                        female = "B01001_026", male = "B01001_002" ) ) %>%   # This includes all states that we specify in the loop
  pivot_wider( id_cols = GEOID, names_from = variable, values_from = estimate ) %>%   # Pivot data since tidycensus returns data table as a long data frame
  mutate( poverty.rate = ( pov / pop )*100,
          perc.female = ( female / pop )*100,
          perc.male = ( male / pop )*100 ) %>%
  select( fips.ct = GEOID, pop, poverty.rate, med.income, perc.female, perc.male ) 

# merge ACS data to NPO data where rows are counties identified by their 5-digit FIPS codes
dat <-  left_join( ct.sf.tigris, n.ct, by = c( "GEOID" = 'fips.ct') ) %>% # join count data to `tigris` shapefile
  rename( fips.ct = GEOID ) %>%             # rename identifier
  left_join(., pop.ct, by='fips.ct') %>%    # join to Census data
  mutate( n.cum = ifelse( is.na( n.cum )==T, 0, n.cum ) ) %>%     # if a county has NA in its count of NPOs, it is because it was not present in the 1023-EZ data for that given year, meaning there were no filers in that county...thus, zero NPOs
  mutate( dens.cum = ( n.cum / pop ) * 1000 ) %>%           # calculate density metric
  mutate( pop.w = pop /  max( pop.ct$pop, na.rm=T ) ) %>%   # standardizes it
  st_transform( crs = 3395 ) %>%
  filter( is.na( pop.w )==F )  # remove FIPS in Guam, VI, and some Alaska counties without population estimates
# `cartogram_dorling` function will break if there are missing values in the weight parameter.
# no. of rows in this dataset should be ~3234 (one for each county in the US--actually around 3221 since we removed territories)

ct.dorling <- cartogram_dorling( x = dat, weight = "pop.w" , k = 0.8 )  # k parameter was increased to augment circle size

setwd( lf )
setwd("../np-density-dashboard/Data-Rodeo/Dashboard-County-Data/Dorling-Shapefiles" )

saveRDS( ct.dorling, "USA-Counties-Dorling.rds")
# --------------------------------------------------------------------------------------------------------

 

 
 # --------------------------------------------------------------------------------------------------------
 
##  Append Yearly County NPO Data To USA-Counties Dorling Cartogram Dataset/Shapefile

 
 # NEED TO RUN THIS PRIOR TO LOOP:
 #
 # yr.levels <- levels( factor( npo$YR ) )
 
 setwd( lf )
 setwd("../np-density-dashboard/Data-Rodeo/Dashboard-County-Data/Dorling-Shapefiles")

 # read in current Dorling Cartogram file
 ds <- readRDS( "USA-Counties-Dorling.rds" )

 # outer loop (indexed on year)
for( i in 1:length( yr.levels ) ) {
  
  start.time <- Sys.time()
  
  count.nm <- paste0( "n.", yr.levels[i] ) # variable name for NPO count by year
  
  dens.nm <- paste0( "dens.", yr.levels[i] ) # variable name for density by year
  
 # no. of NPOs by county 5-digit FIPS in the NPO dataset
 n.ct <- npo %>%
   filter( YR == yr.levels[i] ) %>%                       # filter by year
   group_by( fips.ct ) %>%
   mutate( !!rlang::sym( count.nm ) := ifelse( is.na( n() ) ==T, 0, n( ) ) ) %>% # count of NPO's within county FIPS
   distinct( fips.ct, !!rlang::sym( count.nm ) )
 # no. of rows in this data should reflect the number of counties for which we have data for
 # i.e., less than 3142 counties in the USA since there weren't filers in every county
 
 # merge ACS data to NPO data where rows are counties identified by their 5-digit FIPS codes

 ( ds <-  left_join( ds, n.ct ) %>% # join count data to `tigris` shapefile
   mutate( !!rlang::sym( count.nm ) := ifelse( is.na( !!rlang::sym( count.nm ) )==T, 0, !!rlang::sym( count.nm ) ) ) %>%     # if a county has NA in its count of NPOs, it is because it was not present in the 1023-EZ data for that given year, meaning there were no filers in that county...thus, zero NPOs
   mutate( !!rlang::sym(dens.nm ) := ( !!rlang::sym( count.nm ) / pop ) * 1000 ) ) %>%          # calculate density metric
   
   saveRDS( "USA-Counties-Dorling.rds" )

 end.time <- Sys.time()
 
 print( end.time - start.time)
 print( paste0( "Iteration ", i, "/", length( yr.levels ), " complete" ) ) 
}
 
 # now put into long format and save final version
 setwd( lf )
 setwd("../np-density-dashboard/Data-Rodeo/Dashboard-County-Data/Dorling-Shapefiles")
 
yr.out <- st_as_sf( st_drop_geometry( ds ) %>% 
  select( fips.ct, starts_with( "n.") )%>%                     # pivot on count columns only
  pivot_longer( cols = !fips.ct, 
                     names_to = 'year', 
                     names_prefix = "n.", 
                     values_to = "n" ) %>%
  left_join(., st_drop_geometry( ds ) %>%                      # join the long format density and count tables
              select( fips.ct, starts_with( "dens.") )%>%      # pivot on density columns only
              pivot_longer( cols = !fips.ct, 
                            names_to = 'year', 
                            names_prefix = "dens.", 
                            values_to = "dens" ) ) %>%
  left_join(., ds%>% select( !starts_with( "n" ) & !starts_with( "dens" ) ),
            by = "fips.ct") ) %>%
  saveRDS( "USA-Counties-Dorling.rds" )


# --------------------------------------------------------------------------------------------------------

 
 
# --------------------------------------------------------------------------------------------------------
 ## Yearly MSA Data (both standard projection and Dorling Cartogram)
 
# NEED TO RUN THIS BEFORE: lines 50-97 will need to be run before the following code chunk can be executed ###
# as well as line 358: yr.levels <- levels( factor( npo$YR ) )
# lines 148-153:
# keep.msa.1.5m <- sj %>%
# group_by( MSA ) %>%
#   mutate( sum.pop = sum( pop )) %>%
#   ungroup() %>%
#   filter( sum.pop > 1500000 ) %>%
#   distinct( MSA ) 
# line 170: `msas`
# line 173: `msa.file`
 
for ( i in 1:length( yr.levels ) ) {
  start.time <- Sys.time()
  
 d.2 <- npo.sf %>%
   filter( YR == yr.levels[i]) %>%
 group_by( GEOID ) %>%         # group by tract FIPS for subsequent computation
   mutate( n = ifelse( is.na( n() ) ==T, 0, n( ) ) ) %>%         # count number of rows per tract FIPS since rows are the nonprofits
   ungroup() %>%
   distinct( GEOID, n ) %>%      # retain only a dataframe of tract FIPS and the no. of nonprofits in them
   left_join( d.1, .) %>%        # join with Census population data
   mutate( n = ifelse( is.na( n ), 0, n ),                # tracts that have an NA (i.e., tracts not represented in the NCCS data) get allocated 0 new NPOs 
           dens = ( n / pop ) * 1000 ,                    # create NPO density metric (NPOs per 1k in the population)
           dens = ifelse( is.na( dens ), 0, dens ) ) %>%  # those still having a "NA" for the density metric are tracts that have 0 population and 0 number of nonprofits (n = 763 instances of 0/0)       
   distinct( GEOID, pop, poverty.rate, med.income, 
             perc.female, perc.male, n, dens )                # keep unique rows (i.e., 1 for each tract)
 # this dataset should have same number of rows as d.1 (all census tracts) and no missings
 # in any of the columns
 
 # spatial join MSA names to tract-level data
 sj <- m %>%
   rename (MSA = NAME ) %>%
   select( MSA, geometry ) %>%
   st_join( t, ., left = T ) %>%          # spatial left join
   left_join( ., d.2) %>%                 # append Census data
   distinct( )
 
 # now loop and make store shapefiles as .rds in "/Data-Rodeo"

 d.3 <- st_transform( d.3, crs = 3395 ) # ensure data are in compatible projection before using cartogram fct
 
 
 ### NOTE: The following loop may take quite a while to run depending on your machine's specifications ###
 
 
 end.time <- Sys.time()
 print( end.time - start.time)
 print( paste0( "Iteration ", i, "/", length( msa.file ), " complete" ) ) 
 
}

 
 
# --------------------------------------------------------------------------------------------------------

## Create Dataset/Shapefile for Leaflet Map with Popup Message for each County
 
setwd( lf )
 
setwd("../np-density-dashboard/Data-Rodeo/Dashboard-County-Data" )
cnties <- readRDS( "USA-Counties.rds" )
 
# Cumulative County Map (2014-2021)

# Cumulative County Map (2014-2021)

ct.leaf <- st_as_sf( st_drop_geometry( cnties )%>%
  left_join(., counties( cb = T ) , c( "fips.ct" = "GEOID" ) ) %>%
  mutate( dens = round( dens, digits = 2 ) ) %>%  # round density metric to keep tidy on Leaflet popup
group_by( year ) %>%                              # group by year to create year-specific variables that will be used for the popups
  mutate( popup = paste(sep = "<br/>",            # create popup script for Leaflet map
                        paste0("<b>",NAMELSAD,", ", STUSPS, " (", year ,")</b>"),
                        paste0( "<i># New Nonprofits<i>: ", n ),
                        paste0( "<i>Density New Nonprofits<i>: ", dens, " nonprofits / 1,000 inhabitants" ) ) )%>%
  ungroup() )


 
 # overwrite USA-Counties dataset/shapefile
 # dir.create( "Leaflet-Shapefiles" )

setwd( "Leaflet-Shapefiles" )
saveRDS( ct.leaf, "USA-Counties-Leaflet.rds" )
 # --------------------------------------------------------------------------------------------------------
 
 
 