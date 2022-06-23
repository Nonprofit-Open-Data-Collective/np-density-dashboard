###---------------------------------------------------
###   02-IDENTIFYING POBS AND CHECKING DATA
###---------------------------------------------------

# In this script we will work with the NONPROFIT-2014-2019.rds and PEOPLE-2014-2019.rds files to:
#   
# Identifying Addresses that are PO Boxes. POBs will be geocoded through their zips or city centroids
# Exploring data gaps in the addresses data
# Creating an input_address variable NEED TO UPDATE THIS IN THE SCRIPT
# 
# New datasets will be stored as NONPROFIT-2014-2019v2.rds and PEOPLE-2014-2019v2.rds. 
# In addition address files NPOAddresses_census.rds and PPLAddresses_census.rds will be created to pass them through
# the Census Geocoding service.

library( tidyverse )
library( pander )
library( httr )

# Reference: https://nonprofit-open-data-collective.github.io/open-1023-ez-dataset/Step-02-POBsandGaps.html#1_identifying_po_boxes

# update the path with your working directory:
setwd( '/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Geospatial Dashboard/np-density-dashboard/Data-Wrangled' )

dir() # view directory contents


## Step 1: Identifying POB's in the NPO dataset
# loading data
npo <- readRDS( "NONPROFITS-2014-2021.rds" )       

# creating a variable for pob flags
npo$pob <- NA

# Use regex to identify po boxes
x <- grepl( "Post Office|POST OFFICE BOX|PO BOX|POBOX|BOX\\s*\\d|POB\\s*\\d|CPO\\s*\\d|PO B\\s*\\d|^PO\\s*\\d+$", npo$Address, ignore.case=TRUE )
pob <- npo$Address[ which( x )]
head( pob, 5 )

# including a character length var
pob.len <- nchar( pob )
pobs <- tibble( pob, pob.len )

# arranging it
pobs <- arrange( pobs, desc( pob.len ))
head( pobs )

x <- table( pobs$pob.len )
barplot( x )

# Most NPO POB addresses are clustered around the 11 character length.
# After taking a closer look, we notice that lengthy POB addresses have a mix of both POB info and a regular address.

# Take a look at POBs with 25 characters
head( pobs[ pobs$pob.len==25,], 5 )

# A POB flag and address character length var will be added to the actual dataset

# char length
npo$add.len <- nchar( npo$Address )

# pob flag
x <- grepl( "Post Office|POST OFFICE BOX|PO BOX|POBOX|BOX\\s*\\d|POB\\s*\\d|CPO\\s*\\d|PO B\\s*\\d|^PO\\s*\\d+$", npo$Address, ignore.case=TRUE )
npo$pob <- as.numeric( x ) # adding a flag for pobs identified

x <- as.data.frame( prop.table( table( npo$pob )))
names( x ) <- c( "pob", "percent" )
x$percent <- paste0( round( x$percent*100, 1 ), " %" )
table( x )

# We will merge the ADDRESS, CITY, STATE and ZIP variable to 
# create an INPUT ADDRESS var in the NPO dataset for geocoding.
# creating an Input Address variable
npo$input_address <- paste( npo$Address, npo$City, npo$State, npo$Zip, sep = ", " ) 

saveRDS( npo, "NONPROFITS-2014-2021v2.rds" ) 


## Step 2: Identifying PO boxes in Board members dataset

# loading data
ppl <- readRDS( "PEOPLE-2014-2021.rds" )   

# creating a variable for pob flags
ppl$pob <- NA

# regex
x <- grepl( "Post Office|POST OFFICE BOX|PO BOX|POBOX|BOX\\s*\\d|POB\\s*\\d|CPO\\s*\\d|PO B\\s*\\d|^PO\\s*\\d+$", ppl$Address, ignore.case=TRUE )
pob <- ppl$Address[ which( x )]
head( pob, 5 )

# we have po boxes mixed with regular addresses
pob.len <- nchar( pob )
pobs <- tibble( pob, pob.len )
pobs <- arrange( pobs, desc( pob.len ))
head( pobs )

ppl$add.len <- nchar( ppl$Address )

x <- grepl( "Post Office|POST OFFICE BOX|PO BOX|POBOX|BOX\\s*\\d|POB\\s*\\d|CPO\\s*\\d|PO B\\s*\\d|^PO\\s*\\d+$", ppl$Address, ignore.case=TRUE )
ppl$pob <- as.numeric( x ) # adding a flag for pobs identified

x <- as.data.frame( prop.table( table( ppl$pob )))
names( x ) <- c( "pob", "percent" )
x$percent <- paste0( round( x$percent*100, 1 ), " %" )
table( x )

# creating an Input Address variable
ppl$input_address <- paste( ppl$Address, ppl$City, ppl$State, ppl$Zip, sep = ", " ) # creating an input_address field to match the geocode dataframes

saveRDS( ppl, "PEOPLE-2014-2021v2.rds" )


## Step 3: Exploring Address Data Gaps in NPO dataset

nmz <- c( "ID", "ORGNAME", "Address", "City", "State", "Zip", "pob", "add.len" )
add <- npo[ , nmz]

# dropping all pobs
x <- add$pob == 0
add <- add[ x,]

x <- table( add$add.len )
barplot( x )
# NPO Addresses are clustered around 18 characters

# Addresses with length 1-2 are unintelligible and will probably have to be geocoded using zip codes or city centers
x <- which( add$add.len %in% c( 1, 2 ))
head( add$Address[ x], 15 )

# Looking at those with length 3-4, they still look unintelligible
x <- which( add$add.len %in% c( 3, 4 ))
head( add$Address[ x], 15 )

# Addresses with length 5-6 also seem to be numbers with not much sense
x <- which( add$add.len %in% c( 5, 6 ))
head( add$Address[ x], 15 )

# However, looking at those with length 7-8, we start to recognize proper addressess.
x <- which( add$add.len %in% c( 7, 8 ))
head( add$Address[ x], 15 )

# City var has some unintelligible values
x <- as.data.frame( table( add$City, useNA = "always" ))
names( x )[ 1] <- "City"
x <- arrange( x, City )
x$City[ 1:50]
head( add$Address[ x], 15 )

# And some values that seem to be too short or too long
x$City <- as.character( x$City )
x$len <- nchar( x$City )
barplot( table( x$len ))

x <- arrange( x, len )
head( x$City, 20 )

tail( x$City, 15 )

sort( unique( add$State ))
# ZIP variable has 0 NAs and 0

## Step 4: Exploring Address Data Gaps in Board Member dataset
nmz <- c( "ID", "ORGNAME", "Address", "City", "State", "Zip", "pob", "add.len" )
add <- ppl[ , nmz]

# dropping all pobs
x <- add$pob == 0
add <- add[ x,]

x <- table( add$add.len )
barplot( x )

# PPL Addresses are clustered around 18 characters.

# Similar to NPO data, shorter addresses are unintelligeble.
x <- which( add$add.len %in% c( 1, 2 ))
head( add$Address[ x], 15 )

x <- which( add$add.len %in% c( 3, 4 ))
head( add$Address[ x], 15 )

x <- which( add$add.len %in% c( 5, 6 ))
head( add$Address[ x], 15 )

# And, again, Addresses with length 7-8 are recognizable as proper addressess.
x <- which( add$add.len %in% c( 7, 8 ))
head( add$Address[ x], 20 )

# City var has some unintelligible values
x <- as.data.frame( table( add$City, useNA = "always" ))
names( x )[ 1] <- "City"
x <- arrange( x, City )
x$City[ 1:50]

# And some values that seem to be too short or too long
x$City <- as.character( x$City )
x$len <- nchar( x$City )
barplot( table( x$len ))

x <- arrange( x, len )
head( x$City, 20 )
tail( x$City, 15 )
sort( unique( add$State ))


## Part 5: Creating Addresses files to pass through the Census Geocode service
# For the Census geocoding, addresses should be formatted in the following fields:
#   
#   Unique ID,
# House Number and Street Name,
# City,
# State,
# ZIP Code

## Part 5a: Creating a NPO input_address dataset
npo <- readRDS( "NONPROFITS-2014-2021v2.rds" )

npo$input_address <- paste( npo$Address, npo$City, npo$State, npo$Zip, sep = ", " ) # creating an input_address field to match the geocode dataframes
npo <- npo[ , c( 1, 73, 12:15, 71 )]
npo$ID <- 0
npo <- unique( npo )
npo <- npo[ order( npo$input_address ),]
npo$ID <- 1:nrow( npo )
rownames( npo ) <- NULL

saveRDS( npo, "NPOAddresses_census.rds" ) 

## Part 5b: Creating a PPL input_address dataset

ppl <- readRDS( "PEOPLE-2014-2021v2.rds" )

ppl$input_address <- paste( ppl$Address, ppl$City, ppl$State, ppl$Zip, sep = ", " ) # creating an input_address field to match the geocode dataframes

ppl <- ppl[ , c( 1, 21, 11:14, 19 )]
ppl$ID <- 0
ppl <- unique( ppl )

ppl <- ppl[ order( ppl$input_address ),]
ppl$ID <- 1:nrow( ppl )
rownames( ppl ) <- NULL

saveRDS( ppl, "pplAddresses_census.rds" ) 

