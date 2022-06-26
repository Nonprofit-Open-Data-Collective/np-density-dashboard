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

wd<- ( '/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Geospatial Dashboard/np-density-dashboard/Data-Wrangled' )
setwd( wd )
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
setwd( wd )
npo <- readRDS( "NPOAddresses_census.rds" )

### Split into batches of 500 and prepare batch .csv to pull from in geocoding step

# setting wd 
wd2 <- '/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Geospatial Dashboard/np-density-dashboard/addresses_npo'
setwd( wd2 )

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

setwd( wd2 )

## Geocode the addresses

# create a folder for npo census geocoding files if needed:
# dir.create( "~Results" )

# setting wd
setwd( wd2 )

for( i in 1:loops ){
  
  start_time <- Sys.time( )# document start times
  
  res<- tibble( read.csv( paste0( "AddressNPO", i, ".csv" ) ) )%>%
    geocode( ., street = Address, city = City, state = State, postalcode = Zip, method = 'census', full_results = T )
  
  write.csv( res, paste0( "Results/ResultsNPO", i, ".csv" ), row.names = F )

  end_time <- Sys.time( )# document start times

  print( paste0( "Iteration #", i, " complete" ) ) # print iteration to keep track of loop
  print( end_time - start_time ) # print system time to keep track of iteration progress
}



#### Now repeat the same set of steps for the boardmembers dataset ####

# read in ppl data
setwd( wd )
ppl <- readRDS( "pplAddresses_census.rds" )

# dir.create( "/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Geospatial Dashboard/np-density-dashboard/addresses_ppl" )

### Split into batches of 500 and prepare batch .csv to pull from in geocoding step

# setting wd 
wd2 <- '/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Geospatial Dashboard/np-density-dashboard/addresses_ppl'
setwd( wd2 )

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
setwd( wd2 )

for ( i in 1:loops ){
  
  start_time <- Sys.time( )# document start times
  
  res<- tibble( read.csv( paste0( "Addressppl", i, ".csv" ) ) )%>%
    geocode( ., street = Address, city = City, state = State, postalcode = Zip, method = 'census', full_results = T )
  
  write.csv( res, paste0( "Results/Resultsppl", i, ".csv" ), row.names = F )
  
  end_time <- Sys.time( )# document end times
  
  print( paste0( "Iteration #", i, " complete" ) ) # print iteration to keep track of loop
  print( end_time - start_time ) # print system time to keep track of iteration progress
}


#### Combining Result Files ####