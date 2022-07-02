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

#dir.create("Results")
saveRDS(dat, "Results/npo1res.rds") # change the name of the file accordingly




#### Geocoding PPL Leftover Addresses
# start with NPO addresses that require only 25274 queries
setwd( wd2 )

ppl.left <- readRDS( "LO-PPL-addresses-google.rds" )

ppl.left.1 <- ppl.left[1:40000,]
# 1. selecting 40000 for first pass through the geocoder

api <- "insert.here" # reading my personal API key from a local file

register_google(key = api) #The register_google function stores the API key.
getOption("ggmap") #summarises the Google credentials to check how you are connected.
dat.ppl <- mutate_geocode(ppl.left.1, input_address, output = "latlona", source = "google", messaging = T) #generates an object where the original dataset is binded with the geocode results.

#dir.create("Results")
saveRDS(dat.ppl, "Results/ppl1res.rds") # change the name of the file accordingly

# 1. selecting remaining cases for second pass through the geocoder
ppl.left.2 <- ppl.left[40001:nrow(ppl.left),] #34544 left to pass through the geocoder


api <- "insert.here" # reading my personal API key from a local file

register_google(key = api) #The register_google function stores the API key.
getOption("ggmap") #summarises the Google credentials to check how you are connected.
dat.ppl.2 <- mutate_geocode(ppl.left.2, input_address, output = "latlona", source = "google", messaging = T) #generates an object where the original dataset is binded with the geocode results.

saveRDS(dat.ppl.2, "Results/ppl2res.rds") # change the name of the file accordingly

# Combine all files with Ignacios to round out dataset

setwd( wd2 )

dat.ppl.2 <- readRDS( "Results/ppl2res.rds" ) 
dat.ppl.2 <- dat.ppl.2[,-c(1,2)]
names(dat.ppl.2) <- c("input_address", "lon_ggl", "lat_ggl", "address_ggl")

dat.ppl.1 <- readRDS( "Results/ppl1res.rds" ) 
dat.ppl.1 <- dat.ppl.1[,-c(1,2)]
names(dat.ppl.1) <- c("input_address", "lon_ggl", "lat_ggl", "address_ggl")


dat.npo.1 <- readRDS( "Results/npo1res.rds" ) 
dat.npo.1 <- dat.npo.1[,-c(1,2)]
names(dat.npo.1) <- c("input_address", "lon_ggl", "lat_ggl", "address_ggl")


# Ignacio data
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


ppl.out <- bind_rows( ppl.res, dat.ppl.1, dat.ppl.2 )
npo.out <- bind_rows( npo.res, dat.npo.1 )

## PPL SAVE ##
# loading mains
ppl.main <- readRDS("PEOPLE-2014-2021v3.rds")
ppl.main <- left_join(ppl.main, ppl.out, by = "input_address")
#Adding a geocode_type variable to all Addresses geocoded by google
x <- which(!is.na(ppl.main$address_ggl))
ppl.main$geocode_type[x] <- "google"
saveRDS(ppl.main, "PEOPLE-2014-2021v4.rds")

## NPO SAVE ##
# loading mains
npo.main <- readRDS("NONPROFITS-2014-2021v3.rds")
npo.main <- left_join(npo.main, npo.out, by = "input_address")
#Adding a geocode_type variable to all Addresses geocoded by google
x <- which(!is.na(npo.main$address_ggl))
npo.main$geocode_type[x] <- "google"
saveRDS(npo.main, "NONPROFITS-2014-2021v4.rds")



### EXPLORE DATA ###

## NPO's ##

x <- table(npo.main$geocode_type, useNA = "always")
y <- prop.table(table(npo.main$geocode_type, useNA = "always"))

summary <- as.data.frame(t(rbind(x,y)))
colnames(summary) <- c("frequency", "percent")
summary$percent <- summary$percent*100
summary[nrow(summary)+1,] <- c(sum(summary$frequency), 100)
rownames(summary)[nrow(summary)] <- "TOTAL"
summary

# frequency   percent
# census    259181  68.96432
# google     64141  17.06699
# NA.        52497  13.96869
# TOTAL     375819 100.00000

x <- round(prop.table(table(npo.main$pob, useNA = "ifany"))*100,1)
names(x) <- c("Non-POB", "POB")
x
# Non-POB     POB 
# 88.8        11.2 

# Summary of geocoding process excluding POBs:
  
x <- which(npo.main$pob == 0)
npo.main1 <- npo.main[x,]

# Summary
x <- table(npo.main1$geocode_type, useNA = "always")
y <- prop.table(table(npo.main1$geocode_type, useNA = "always"))

summary <- as.data.frame(t(rbind(x,y)))
colnames(summary) <- c("frequency", "percent")
summary$percent <- summary$percent*100
summary[nrow(summary)+1,] <- c(sum(summary$frequency), 100)
rownames(summary)[nrow(summary)] <- "TOTAL"
summary

# frequency    percent
# census    257702  77.223800
# google     64141  19.220696
# NA.        11865   3.555504
# TOTAL     333708 100.000000

# Summary of geocoding process for only POBs:
  
x <- which(npo.main$pob == 1)
npo.main2 <- npo.main[x,]

#Summary
x <- table(npo.main2$geocode_type, useNA = "always")
y <- prop.table(table(npo.main2$geocode_type, useNA = "always"))

summary <- as.data.frame(t(rbind(x,y)))
colnames(summary) <- c("frequency", "percent")
summary$percent <- summary$percent*100
summary[nrow(summary)+1,] <- c(sum(summary$frequency), 100)
rownames(summary)[nrow(summary)] <- "TOTAL"
summary

# frequency    percent
# census      1479   3.512146
# NA.        40632  96.487854
# TOTAL      42111 100.000000


## PPL ##

#Summary
x <- table(ppl.main$geocode_type, useNA = "always")
y <- prop.table(table(ppl.main$geocode_type, useNA = "always"))

summary <- as.data.frame(t(rbind(x,y)))
colnames(summary) <- c("frequency", "percent")
summary$percent <- summary$percent*100
summary[nrow(summary)+1,] <- c(sum(summary$frequency), 100)
rownames(summary)[nrow(summary)] <- "TOTAL"
summary

# frequency    percent
# census    954305  72.049283
# google    247585  18.692474
# NA.       122627   9.258243
# TOTAL    1324517 100.000000


#The following numbers of POBs

x <- round(prop.table(table(ppl.main$pob, useNA = "ifany"))*100,1)
names(x) <- c("Non-POB", "POB")
x

# Non-POB     POB 
# 94.6     5.4 

#Summary of geocoding process excluding POBs:
  
x <- which(ppl.main$pob == 0)
ppl.main1 <- ppl.main[x,]

#Summary
x <- table(ppl.main1$geocode_type, useNA = "always")
y <- prop.table(table(ppl.main1$geocode_type, useNA = "always"))

summary <- as.data.frame(t(rbind(x,y)))
colnames(summary) <- c("frequency", "percent")
summary$percent <- summary$percent*100
summary[nrow(summary)+1,] <- c(sum(summary$frequency), 100)
rownames(summary)[nrow(summary)] <- "TOTAL"
summary


#Summary of geocoding process for only POBs:
  
x <- which(ppl.main$pob == 1)
ppl.main2 <- ppl.main[x,]

#Summary
x <- table(ppl.main2$geocode_type, useNA = "always")
y <- prop.table(table(ppl.main2$geocode_type, useNA = "always"))

summary <- as.data.frame(t(rbind(x,y)))
colnames(summary) <- c("frequency", "percent")
summary$percent <- summary$percent*100
summary[nrow(summary)+1,] <- c(sum(summary$frequency), 100)
rownames(summary)[nrow(summary)] <- "TOTAL"
summary

# frequency    percent
# census      2031   2.824599
# NA.        69873  97.175401
# TOTAL      71904 100.000000
