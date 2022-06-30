###---------------------------------------------------
###   04-GEOCODING ADDRESSES THROUGH GOOGLE
###---------------------------------------------------

# In this script we will geocode addresses that were not geocoded by the census service through the google geocode service. The new dataset will be saved as a new version of the main files NONPROFITS-2014-2019v4.rds and PEOPLE-2014-2019v4.rds.
# 
# STEPS
# 
# Subsetting input files. We work with the NONPROFIT-2014-2019v3.rds and PEOPLE-2014-2019v3.rds to subset non-POB failed addresses into the input files NPOAddresses_google.rds and PPLAddresses_google.rds.
# Intro to the Google geocode service and demo.
# Geocoding NPO Addresses (NPOAddresses_google.rds). The script will yield raw output file NPOAddresses_googleGEO.rds. The geocoded addressese will then be integrated into a new version of the main file NONPROFIT-2014-2019v4.rds.
# Geocoding PPL Addresses (PPLAddresses_google.rds). The script will yield raw output file PPLAddresses_googleGEO.rds.The geocoded addressese will then be integrated into a new version of the main file PEOPLE-2014-2019v4.rds.
# Troubleshooting
# 
# NOTES
# 
# Geocoding can take several hours, for this reason some code chunks in this script are not evaluated. Outputs yielded from the process are loaded from stored files to ilustrate the results.


#setting up the environment
library( tidyverse )
library( ggmap ) 
library( tidygeocoder)

# %notin% operator #
`%notin%` <- Negate(`%in%`)

# update the path with your working directory:
lf <- "/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Geospatial Dashboard/Large-Files-Bank"

##########################################################################################################
##########################################################################################################
###### FIRST: Loading in Ignacio's data files to see what has already been geocoded in this dataset ###### 
############## THEN: I will geocode the non-intersecting records between his and my datasets ############# 
##########################################################################################################
##########################################################################################################


###### Ignacio's Data ###### 

wd<-("/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Geospatial Dashboard/Large-Files-Bank/Ignacio-data-bank/Open_data_ignacio/Data")

setwd(paste0(wd,"/4_GeoGoogle/Results"))



#### NPO Addresses ####
# loading results file
npo.res <- readRDS("NPOAddresses_googleGEO.rds")
npo.res <- npo.res[,-c(1,2)]
names(npo.res) <- c("input_address", "lon_ggl", "lat_ggl", "address_ggl")


#### PPL Addresses ####
# Loading addresses to geocode

# loading results file
ppl.res <- readRDS("PPLAddresses_googleGEO.rds")
ppl.res <- ppl.res[,-c(1,2)]
names(ppl.res) <- c("input_address", "lon_ggl", "lat_ggl", "address_ggl")



###### Chris's (My) Data ###### 

# loading main files that include newer cycles
setwd( lf )
npo.main <- readRDS("NONPROFITS-2014-2021v3.rds") 
ppl.main <- readRDS("PEOPLE-2014-2021v3.rds")


## NPO'S STILL IN NEED OF GEOCODING ##
# will subset NA, Tie and No_match values
x <- which(npo.main$match %in% c("No_Match", "Tie", NA))
npo <- npo.main[x,c(1,71,73)]

# removing POBs
x <- which(npo$pob == 0)
npo <- npo[x,]

npo$ID <- 0
npo <- unique(npo)
npo <- npo[order(npo$input_address),]
npo$ID <- 1:nrow(npo)
rownames(npo) <- NULL

# number of observations that still need matching in our data - number of those that have already been geocoded
# by Ignacio
nrow(npo) - nrow(npo.res) # 8585 more organizations in our dataset compared to Ignacio's already geocoded data

nrow(npo[which(npo$input_address %notin% npo.res$input_address),]) # 25274 organizations still need geocoding

npo.left <- npo[which(npo$input_address %notin% npo.res$input_address),]

# save a file for geocoding
wd2 <- paste0(lf, "/4_GeoGoogle")
setwd(wd2)
saveRDS( npo.left, "LO-NPO-addresses-google.rds" )



## PPL STILL IN NEED OF GEOCODING ##
# will subset NA, Tie and No_match values and all POBs
x <- which(ppl.main$match %in% c("No_Match", "Tie", NA))
ppl <- ppl.main[x,c(1,19, 21)]

# removing POBs
x <- which(ppl$pob == 0)
ppl <- ppl[x,]

# Resetting ID var and removing duplicate addresses
ppl$ID <- 0
ppl <- unique(ppl)
ppl <- ppl[order(ppl$input_address),]
ppl$ID <- 1:nrow(ppl)
rownames(ppl) <- NULL

# we match records based on input address:
nrow(ppl[which(ppl$input_address %notin% ppl.res$input_address),])
# which tells us that there are 74544 board members in our dataset that need geocoding


# save them:
ppl.left <- ppl[which(ppl$input_address %notin% ppl.res$input_address),]

wd2 <- paste0(lf, "/4_GeoGoogle")
setwd(wd2)
saveRDS(ppl.left, "LO-PPL-addresses-google.rds")


### GOOGLE GEOCODING SERVICE ###
# Sources: • https://lucidmanager.org/geocoding-with-ggmap/
#   • https://www.wpgmaps.com/documentation/troubleshooting/this-api-project-is-not-authorized-to-use-this-api/ • https://www.rdocumentation.org/packages/ggmap/versions/3.0.0/topics/geocode
# 
# The google API receives data as a character vector of street addresses or place names 
# (e.g. “1600 pennsylvania avenue, washington dc” or “Baylor University”) and returns lat and lon coordinates.
# 
# Even though there should be no costs (because we are using the free geocodes available), you will need 
# to enter your credit card information. Google allows for 40,000 calls a month for free. WHERE CAN 
# WE CHECK THIS if THE POLICY CHANGES?
#   
#   Steps to set up a google API:
#   
# Use your Google Account (or create one) and obtain an API key from Google. Follow the instructions in the Link.
# At the Google Cloud Platform, you will need to create a project to which you will then get an API. Once 
# the project is created, follow the instructions and create an API Key.In your R project folder create a text 
# file with your API, so that you can load it afterwards (e.g. I created a txt called “google.api”). If you do not 
# have one, you will need to create a billing account to associate it with the project. At the Google Cloud Platform 
# Console, click on the menu button > Billing, and follow the instructions to create a Billing account. As the last 
# step, now you need to enable certain API services for the geocoding to work. Following these instructions
# 
# Using the API Library enable the following three APIs:
# Google Maps JavaScript API
# Google Maps Geocoding API
# Google Maps Places API
# Once this is ready, you should try running the code
# 
# Google recommends placing restrictions to your account, to limit the possibility of being charged.


# start with NPO addresses that require only 25274 queries
setwd( wd2 )

npo.left <- readRDS( "LO-NPO-addresses-google.rds" )


# 1. selecting the 40 after to test geocode

api <- insert.here # reading my personal API key from a local file

register_google(key = api) #The register_google function stores the API key.
getOption("ggmap") #summarises the Google credentials to check how you are connected.
dat <- mutate_geocode(npo.left, input_address, output = "latlona", source = "google", messaging = T) #generates an object where the original dataset is binded with the geocode results.

saveRDS(dat, "DemoResults.rds")
dat



#### Geocoding NPO Leftover Addresses

# setting wd

setwd(wd2)

# Loading addresses to geocode
npo <- readRDS("NPOAddresses_google.rds")

# We need to split the address file into two batches
npo1 <- npo[1:40000,]
npo2 <- npo[40001:nrow(npo)]

# The following code chunk will run the first batch npo1:
  
  # loading API 
  api <- readLines("../../../google1.api")

# Geocoding through google. This will generate an object where the priginal dataset is binded with the geocode results.
register_google(key = api) #The register_google function stores the API key.
getOption("ggmap") #summarises the Google credentials to check how you are connected.
npo1.res <- mutate_geocode(npo1, input_address, output = "latlona", source = "google", messaging = T) 

#saving results 
saveRDS(npo1.res, "Results/npo1res.rds") # change the name of the file accordingly

