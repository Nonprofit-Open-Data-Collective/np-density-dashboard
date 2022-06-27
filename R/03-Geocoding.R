###---------------------------------------------------
###   03-BATCH GEOCODING ADDRESSES
###---------------------------------------------------

# In this script we will geocode NPO and PPL addresses using the Census Bureau geocoding service. The new dataset will 
# be saved as a new version of the main files NONPROFITS-2014-2019v3.rds and PEOPLE-2014-2019v3.rds.
# 
# Steps
# 
# We will load the NONPROFIT-2014-2019v2.rds and PEOPLE-2014-2019v2.rds files and produce input files NPOAddresses_census.rds and PPLAddresses_census.rds. These hold the data that will be passed through the geocoding service.
# Intro and demo of the Census geocoding service.
# Geocoding NPO addressess( NPOAddresses_census.rds )through the Census geocoding service. The script will yield raw 
# output file NPOAddresses_censusGEO.rds. The new geocode information will be integrated into a new version of the 
# main file NONPROFIT-2014-2019v3.rds. Geocoding PPL addressess( PPLAddresses_census.rds )through the Census geocoding 
# service. The script will yield raw output file PPLAddresses_censusGEO.rds. The new geocode information will be 
# integrated into a new version of the main file PEOPLE-2014-2019v3.rds
# 
# Troubleshooting
# 
# Notes * This script includes a troubleshooting section. * Geocoding can take several hours, for this reason some 
# code chunks in this script are not evaluated. Outputs yielded from the process are loaded from stored files to 
# ilustrate the results.

library( tidyverse )
library( tidygeocoder ) # for geocoding addresses using Census API, Google API, etc.

wd<- ( '/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Geospatial Dashboard/np-density-dashboard/' )

wd2 <- paste0(wd,'/Data-Wrangled')
setwd( wd2 )

# The Census Geocoding Service

# DOCUMENTATION: https://www.census.gov/programs-surveys/geography.html
# WEB GEOCODING SERVICE: https://geocoding.geo.census.gov/geocoder/geographies/addressbatch?form

# This section runs a Demo to test the code is working. Addresses should be formatted in the following fields:
#   
#   Unique ID,
# House Number and Street Name,
# City,
# State,
# ZIP Code
# 
# Geocode adds the following variables to the dataset:
#   
#   match
# match_type
# out_address
# lat_lon
# tiger_line_id
# tiger_line_side
# state_fips
# county_fips
# tract_fips
# block_fips
# lon
# lat
# 
# Geocode outputs from the Census can either be:
#   
#   Match( Exact/Non_Exact ): can be exact or approximate? right? WHAT DOES THIS MEAN?
#   Tie:
#   No_Match:

# npo input_addresses data
npo <- readRDS( "NONPROFITS-2014-2021v2.rds" )

npo$input_address <- paste( npo$Address, npo$City, npo$State, npo$Zip, sep = "," )# creating an input_address field to match the geocode dataframes
npo <- npo[, c( 1, 73, 12:15, 71 )]
npo$ID <- 0
npo <- unique( npo )
npo <- npo[order( npo$input_address ), ]
npo$ID <- 1:nrow( npo )
rownames( npo )<- NULL

saveRDS( npo, "NPOAddresses_census.rds" )

# ppl input_addresses data
ppl <- readRDS( "PEOPLE-2014-2021v2.rds" )

ppl$input_address <- paste( ppl$Address, ppl$City, ppl$State, ppl$Zip, sep = "," )# creating an input_address field to match the geocode dataframes

ppl <- ppl[, c( 1, 21, 10:13, 19 )]
ppl$ID <- 0
ppl <- unique( ppl )

ppl <- ppl[order( ppl$input_address ), ]
ppl$ID <- 1:nrow( ppl )
rownames( ppl )<- NULL

saveRDS( ppl, "PPLAddresses_census.rds" )


# read in npo data
setwd( wd2 )
npo <- readRDS( "NPOAddresses_census.rds" )

### Split into batches of 500 and prepare batch .csv to pull from in geocoding step

# setting wd 
wd.npo <- paste0(wd,'/addresses_npo')
setwd( wd.npo )

# Selecting only essential variables
npo <- dplyr::select( npo, ID, Address, City, State, Zip )

# Splitting address files into files with 500 addresses each
loops <- ceiling( nrow( npo ) / 500 ) # ceiling function rounds up an integer. so loops has the amount of 500s that fit rounded up.

# loop to extract by addresses in 500 batches
for( i in 1:loops ){
  filename <- paste0( "AddressNPO", i, ".csv" )
  start.row <- ( ( i-1 )*500+1 )# i starts in 1 and this outputs: 1, 501, 1001, etc.
  end.row <- ( 500*i )# this outputs 500, 1000, etc.
  if( nrow( npo )< end.row ){ end.row <- nrow( npo )} # this tells the loop when to stop
  
  # writing a line in the csv address file.
  write.csv( npo[ start.row:end.row, ], filename, row.names = F )
  
  # output to help keep track of the loop.
  print( i )
  print( paste( "Start Row:", start.row ) )
  print( paste( "End Row:", end.row ) )
} # end of loop.

setwd( wd.npo )

## Geocode the addresses

# create a folder for npo census geocoding files if needed:
# dir.create( "~Results" )

# setting wd
setwd( wd.npo )

## Create log file

# producing LOG file
log <- c("Query_Number", "Start_time", "Time_taken")
log <- paste(log, collapse=',')
log.name <- as.character(Sys.time())
log.name <- gsub(":","-",log.name)
log.name <- gsub(" ","-",log.name)
log.name <- paste0("Results/Geocode_Log_",log.name,".txt")
write(log, file=log.name, append = F)

for( i in 1:5 ){
  
  # outputs in console to track progress
  print(Sys.time() )
  start_time <- Sys.time( )# document start times
  
  res<- tibble( read.csv( paste0( "AddressNPO", i, ".csv" ) ) )%>%
    geocode( ., street = Address, 
             city = City, 
             state = State,
             postalcode = Zip, 
             method = 'census',
             return_type = "geographies",
             full_results = T ) %>%
    select( id, input_address,
                    match = match_indicator, match_type, 
                    out_address = matched_address, lat, lon= long, 
                    tiger_line_id, tiger_line_side = tiger_side, 
                    state_fips, county_fips, 
                    tract_fips = census_tract, block_fips = census_block )
  
  write.csv( res, paste0( "Results/ResultsNPO", i, ".csv" ), row.names = F )

  end_time <- Sys.time( )# document start times
  
  #writing a line in the log file after query i ends
  query <- paste0("Batch ", as.character(i) )
  len <- as.character(end_time - start_time)
  start_time <- as.character(start_time)
  log <- c(query, start_time, len)
  log <- paste(log, collapse=',')
  write(log, file=log.name, append = T)
  

  print( paste0( "Batch #", i, " complete" ) ) # print iteration to keep track of loop

}



#### Now repeat the same set of steps for the boardmembers dataset ####

# read in ppl data
setwd( wd )
ppl <- readRDS( "pplAddresses_census.rds" )

# dir.create( "/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Geospatial Dashboard/np-density-dashboard/addresses_ppl" )

### Split into batches of 500 and prepare batch .csv to pull from in geocoding step

# setting wd 
wd.ppl <- paste0(wd,'/addresses_ppl')
setwd( wd.ppl )

# Selecting only essential variables
ppl <- dplyr::select( ppl, ID, Address, City, State, Zip )

# Splitting address files into files with 500 addresses each
loops <- ceiling( nrow( ppl ) / 500 ) # ceiling function rounds up an integer. so loops has the amount of 500s that fit rounded up.

# loop to extract by addresses in 500 batches
for( i in 1:loops ){
  filename <- paste0( "Addressppl", i, ".csv" )
  start.row <- ( ( i-1 )*500+1 )# i starts in 1 and this outputs: 1, 501, 1001, etc.
  end.row <- ( 500*i )# this outputs 500, 1000, etc.
  if( nrow( ppl )< end.row ){ end.row <- nrow( ppl )} # this tells the loop when to stop
  
  # writing a line in the csv address file.
  write.csv( ppl[ start.row:end.row, ], filename, row.names = F )
  
  # output to help keep track of the loop.
  print( i )
  print( paste( "Start Row:", start.row ) )
  print( paste( "End Row:", end.row ) )
} # end of loop.


## Geocode the addresses

# create a folder for ppl census geocoding files if needed:
# dir.create( "Results" )

# setting wd
setwd( wd.ppl )

for( i in 1:loops ){
  
  # outputs in console to track progress
  print(Sys.time() )
  start_time <- Sys.time( )# document start times
  
  res<- tibble( read.csv( paste0( "Addressppl", i, ".csv" ) ) )%>%
    geocode( ., street = Address, 
             city = City, 
             state = State,
             postalcode = Zip, 
             method = 'census',
             return_type = "geographies",
             full_results = T ) %>%
    select( id, input_address,
            match = match_indicator, match_type, 
            out_address = matched_address, lat, lon = long, 
            tiger_line_id, tiger_line_side = tiger_side, 
            state_fips, county_fips, 
            tract_fips = census_tract, block_fips = census_block )
  
  write.csv( res, paste0( "Results/ResultsPPL", i, ".csv" ), row.names = F )
  
  end_time <- Sys.time( )# document start times
  
  #writing a line in the log file after query i ends
  query <- paste0("Batch ", as.character(i) )
  len <- as.character(end_time - start_time)
  start_time <- as.character(start_time)
  log <- c(query, start_time, len)
  log <- paste(log, collapse=',')
  write(log, file=log.name, append = T)
  
  
  print( paste0( "Batch #", i, " complete" ) ) # print iteration to keep track of loop
}


### Combining Results ###

## NPO ##

# setting wd
wd.res.npo <- paste0(wd, "/addresses_npo/Results")

setwd(wd.res.npo)

#capturing filenames of all elements in dir() that have "ResultsNpo"
x <- grepl("ResultsNpo", dir()) 
these <- (dir())[x]

#loading first file in the string vector
npo <- read.csv( these[1], stringsAsFactors=F )

#compiling all Results into one
for( i in 2:length(these) )
{
  d <- read.csv( these[i], stringsAsFactors=F )
  npo <- bind_rows( npo, d )
}

#saving compiled geocodes
saveRDS( npo, "../../../NPOAddresses_censusGEO.rds" )
setwd(wd)


# merge to main NPO address file

# results
npo <- readRDS("Data/3_GeoCensus/NPOAddresses_censusGEO.rds")

#removind the IDs and pob.
npo <- npo[,-c(1,3)]

# main
npo.main <- readRDS(paste0( wd,"Data-Wrangled/NONPROFITS-2014-2019v2.rds") )

# join to NPO file
npo.main <- left_join(npo.main, npo, by = "input_address")

# Adding a geocode_type variable to all Match cases
npo.main$geocode_type <- NA

# Adding a value to all Match values yielded in the process
x <- which(npo.main$match %in% "Match")
npo.main$geocode_type[x] <- "census"

# Renaming the lat lon vars to make sure we know they come from the census

x <- which(names(npo.main) %in% "lon") 
names(npo.main)[x] <- "lon_cen"

x <- which(names(npo.main) %in% "lat") 
names(npo.main)[x] <- "lat_cen"

x <- which(names(npo.main) %in% "lat_lon") 
names(npo.main)[x] <- "lat_lon_cen"

# save
saveRDS(npo.main, paste0(wd,"Data-Wrangled/NONPROFITS-2014-2019v3.rds")  )


## PPL ##

# setting wd
wd.res.ppl <- paste0(wd, "/addresses_ppl/Results")
setwd(wd.res.ppl)

#capturing filenames of all elements in dir() that have "Resultsppl"
x <- grepl("ResultsPPL", dir()) 
these <- (dir())[x]

#loading first file in the string vector
ppl <- read.csv( these[1], stringsAsFactors=F )

#compiling all Results into one
for( i in 2:length(these) )
{
  d <- read.csv( these[i], stringsAsFactors=F )
  ppl <- bind_rows( ppl, d )
}

#saving compiled geocodes
saveRDS( ppl, "../../PPLAddresses_censusGEO.rds" )
setwd(wd)


# results
ppl <- readRDS("Data-Wrangled/3_GeoCensus/PPLAddresses_censusGEO.rds")
ppl <- ppl[,-c(1,3)]

# main
ppl.main <- readRDS("Data-Wrangled/PEOPLE-2014-2019v2.rds")

# join files
ppl.main <- left_join(ppl.main, ppl, by = "input_address")
#### Combining Result Files ####

# Adding a geocode_type variable to all Match results
ppl.main$geocode_type <- NA

# Adding a value to all Matches yielded in the process
x <- which(ppl.main$match %in% "Match")
ppl.main$geocode_type[x] <- "census"

# Renaming the lat lon vars to make sure we know they come from the census
x <- which(names(ppl.main) %in% "lon") 
names(ppl.main)[x] <- "lon_cen"

x <- which(names(ppl.main) %in% "lat") 
names(ppl.main)[x] <- "lat_cen"

x <- which(names(ppl.main) %in% "lat_lon") 

# save 
saveRDS(ppl.main, "Data-Wrangled/3_GeoCensus/PEOPLE-2014-2019v3.rds")
names(ppl.main)[x] <- "lat_lon_cen"