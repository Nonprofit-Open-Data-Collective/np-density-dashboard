###---------------------------------------------------
###   05-GEOCODING ZIPS AND POB'S
###---------------------------------------------------

# In this script we will geocode POBs and failed addresses using zip and city centroids. The new dataset will be saved as a new version of the main files:
#   
#   NONPROFITS-2014-2019v5.rds
# PEOPLE-2014-2019v5.rds
# 
# STEPS
# 
# Geocoding with zip code centroids: we will add zip code centroids to the NPO and PPL files.
# Geocoding with city centroids: city centroids will be added.
# Generating a final lat lon variable.
# Exploring the results


#setting up the environment
library( tidyverse )

# update the path with your working directory:
lf <- "/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Geospatial Dashboard/Large-Files-Bank"
setwd( lf )

setwd( paste0( lf, "/Ignacio-data-bank/Open_data_ignacio/Data/5_ZipandCity"))

### read in Zip and City centroid data ###
zips1 <- readRDS("zips1.rds")
names(zips1) <- c("Zip", "lat_zip1", "lon_zip1")

# 
# Loading a secondary zip-code-centroid data to use in throughout the script
# 
# The zips are from this webpage: https://public.opendatasoft.com/explore/dataset/us-zip-code-latitude-and-longitude/export/
##### NOTE: reading the data in from the above link did not work. Will need to update this to make it reproducible
##### For the following sections, I am using Ignacio's data
zips2 <- readRDS( "zips2.rds" )
names(zips2) <- c( "Zip", "City_zip2", "State_zip2", "lat_zip2", "lon_zip2" )


## NPO Data read in ##
setwd( lf )
npo.main <- readRDS( "NONPROFITS-2014-2021v4.rds" )

#zip1
npo.main <- left_join(npo.main, zips1, by = "Zip")
x <- which(is.na(npo.main$geocode_type)==TRUE & is.na(npo.main$lat_zip1)==FALSE)
npo.main$geocode_type[x] <- "zip1"

#zip2
npo.main <- left_join(npo.main, zips2, by= "Zip")
x <- which(is.na(npo.main$geocode_type)==TRUE & is.na(npo.main$lat_zip2)==FALSE)
npo.main$geocode_type[x] <- "zip2"

table(npo.main$geocode_type, useNA = "ifany") 
# census google   zip1   zip2   <NA> 
# 259181  64141  39862  12326    309 

#Only 309 addresses have no geocode.

#Saving file

saveRDS(npo.main, "NONPROFITS-2014-2021v5.rds")



## PPL Data read in ##
setwd( lf )
ppl.main <- readRDS("PEOPLE-2014-2021v4.rds")

#zip1
ppl.main <- left_join(ppl.main, zips1, by = "Zip")
x <- which(is.na(ppl.main$geocode_type)==TRUE & is.na(ppl.main$lat_zip1)==FALSE)
ppl.main$geocode_type[x] <- "zip1"

#zip2
ppl.main <- left_join(ppl.main, zips2, by= "Zip")
x <- which(is.na(ppl.main$geocode_type)==TRUE & is.na(ppl.main$lat_zip2)==FALSE)
ppl.main$geocode_type[x] <- "zip2"

table(ppl.main$geocode_type, useNA = "ifany")

# census google   zip1   zip2   <NA> 
# 954305 247585  98900  20124   3603 

# Only 3603 addresses have no geocode.

# Saving file

saveRDS(ppl.main, "PEOPLE-2014-2021v5.rds")


###### Adding City centroids to the remaining addresses #######
# 
# Getting City Centroids from: https://public.opendatasoft.com/explore/dataset/1000-largest-us-cities-by-population-with-geographic-coordinates/table/?sort=-rank
# 
# Note: City names may be repeated across the US, so we need to look at the state to be certain we are matching the precise one.

# getting city data from source
city <- read.csv("https://public.opendatasoft.com/explore/dataset/1000-largest-us-cities-by-population-with-geographic-coordinates/download/?format=csv&timezone=America/New_York&use_labels_for_header=true", sep=';', stringsAsFactors = F)
# NOTE: Link not working as well
# will read in Ignacio's stored data

setwd( paste0( lf, "/Ignacio-data-bank/Open_data_ignacio/Data/5_ZipandCity"))

city <- readRDS( "cities-geo.rds" )

# Cities repeat unless we use the city+state:
#   
# 75 duplicated cities.
# 0 duplicated city_st
# 
# Preparing the file for merge, removing unwanted vars

city <- city[,c(5,3,4)]

## NPO Data ##
# Creating a city_st variable in the NPO dataset

npo.main$city_st <- paste(npo.main$City, npo.main$State, sep= ", ")

# Now merging with main npo data

npo.main <- left_join(npo.main, city, by= "city_st")

# Adding a geocode_type value

x <- which(is.na(npo.main$geocode_type)==TRUE & is.na(npo.main$lat_cty)==FALSE)
npo.main$geocode_type[x] <- "city"

table(npo.main$geocode_type, useNA = "ifany") 

#Only 167 addresses have no geocode.

#Saving file
setwd( lf )
saveRDS(npo.main, "NONPROFITS-2014-2021v5.rds")


## PPL Data ## 

# Creating a city_st variable in the PPL dataset

ppl.main$city_st <- paste(ppl.main$City, ppl.main$State, sep= ", ")

# Now merging with main npo data

ppl.main <- left_join(ppl.main, city, by= "city_st")

# Adding a geocode_type value

x <- which(is.na(ppl.main$geocode_type)==TRUE & is.na(ppl.main$lat_cty)==FALSE)
ppl.main$geocode_type[x] <- "city"
table(ppl.main$geocode_type, useNA = "ifany") 

# census   city google   zip1   zip2   <NA> 
# 954305    677 247585  98900  20124   2926 

# Only 2926 addresses have no geocode.

# Saving file
setwd( lf )
saveRDS(ppl.main, "PEOPLE-2014-2021v5.rds")


###### Generating a Final lat and lon value ###### 
# 
# NOTE: this priotization list should be backed by a comparison. why is Google better? why is Zip1 better than Zip2? How much is the difference between Google and Census? etc.
# 
# Geocode information has comes from different sources. As the database evolves over time, geocodes might be updated. Which geocode source we use when available can be summarized in the geocode prioritization list below:
#   
#   Google
# Census
# Zip (Zip1 > Zip2)
# City
# 
# Note: for more detail on this priority list see the Research Note.
# 
# Following this list, we will generate Lat and Lon variables in each data set.


## NPO ##
setwd( lf )
npo.main <- readRDS("NONPROFITS-2014-2021v5.rds")

npo.main$lat <- NA
npo.main$lon <- NA

npo.main$geocode_type <- NA

# city
x <- which(is.na(npo.main$lat_cty) == FALSE)
npo.main$lat[x] <- npo.main$lat_cty[x]
npo.main$lon[x] <- npo.main$lon_cty[x]
npo.main$geocode_type[x] <- "city"

# zip2
x <- which(is.na(npo.main$lat_zip2) == FALSE)
npo.main$lat[x] <- npo.main$lat_zip2[x]
npo.main$lon[x] <- npo.main$lon_zip2[x]
npo.main$geocode_type[x] <- "zip2"

# zip1
x <- which(is.na(npo.main$lat_zip1) == FALSE)
npo.main$lat[x] <- npo.main$lat_zip1[x]
npo.main$lon[x] <- npo.main$lon_zip1[x]
npo.main$geocode_type[x] <- "zip1"

# census
x <- which(is.na(npo.main$lat_cen) == FALSE)
npo.main$lat[x] <- npo.main$lat_cen[x]
npo.main$lon[x] <- npo.main$lon_cen[x]
npo.main$geocode_type[x] <- "census"

# google
x <- which(is.na(npo.main$lat_ggl) == FALSE)
npo.main$lat[x] <- npo.main$lat_ggl[x]
npo.main$lon[x] <- npo.main$lon_ggl[x]
npo.main$geocode_type[x] <- "google"


# checking the number of NAs
x <- is.na(npo.main$lat_cty) &
  is.na(npo.main$lat_zip2) &
  is.na(npo.main$lat_zip1) &
  is.na(npo.main$lat_cen) &
  is.na(npo.main$lat_ggl)
x %>% table()

# FALSE   TRUE 
# 375652    167

table(is.na(npo.main$lat))
# 
# FALSE   TRUE 
# 375652    167 

# checking using geocode_type
table(npo.main$geocode_type, useNA = "ifany")
# census   city google   zip1   zip2   <NA> 
# 259181    142  64141  39862  12326    167 

# google
x <- which(npo.main$geocode_type == "google")
x <- npo.main$lat[x] == npo.main$lat_ggl[x]
table(x, useNA = "ifany")
# TRUE 
# 64141 

# census
x <- which(npo.main$geocode_type == "census")
x <- npo.main$lat[x] == npo.main$lat_cen[x]
table(x, useNA = "ifany")
# TRUE 
# 259181 

# zip1
x <- which(npo.main$geocode_type == "zip1")
x <- npo.main$lat[x] == npo.main$lat_zip1[x]
table(x, useNA = "ifany")
# TRUE 
# 39862 

# zip2
x <- which(npo.main$geocode_type == "zip2")
x <- npo.main$lat[x] == npo.main$lat_zip2[x]
table(x, useNA = "ifany")
# TRUE 
# 12326 


# cty
x <- which(npo.main$geocode_type == "city")
x <- npo.main$lat[x] == npo.main$lat_cty[x]
table(x, useNA = "ifany")
# TRUE 
# 142 

setwd( lf )
saveRDS(npo.main, "NONPROFITS-2014-2021v5.rds")


## PPL ##
setwd( lf )
ppl.main <- readRDS("PEOPLE-2014-2021v5.rds")

# Creating the new variables

ppl.main$lat <- NA
ppl.main$lon <- NA

# Now adding the prioritized lat/lon data by overwritting the values in the priority order.

ppl.main$geocode_type <- NA

# city
x <- which(is.na(ppl.main$lat_cty) == FALSE)
ppl.main$lat[x] <- ppl.main$lat_cty[x]
ppl.main$lon[x] <- ppl.main$lon_cty[x]
ppl.main$geocode_type[x] <- "city"

# zip2
x <- which(is.na(ppl.main$lat_zip2) == FALSE)
ppl.main$lat[x] <- ppl.main$lat_zip2[x]
ppl.main$lon[x] <- ppl.main$lon_zip2[x]
ppl.main$geocode_type[x] <- "zip2"

# zip1
x <- which(is.na(ppl.main$lat_zip1) == FALSE)
ppl.main$lat[x] <- ppl.main$lat_zip1[x]
ppl.main$lon[x] <- ppl.main$lon_zip1[x]
ppl.main$geocode_type[x] <- "zip1"

# census
x <- which(is.na(ppl.main$lat_cen) == FALSE)
ppl.main$lat[x] <- ppl.main$lat_cen[x]
ppl.main$lon[x] <- ppl.main$lon_cen[x]
ppl.main$geocode_type[x] <- "census"

# google
x <- which(is.na(ppl.main$lat_ggl) == FALSE)
ppl.main$lat[x] <- ppl.main$lat_ggl[x]
ppl.main$lon[x] <- ppl.main$lon_ggl[x]
ppl.main$geocode_type[x] <- "google"

# checking the number of NAs
x <- is.na(ppl.main$lat_cty) &
  is.na(ppl.main$lat_zip2) &
  is.na(ppl.main$lat_zip1) &
  is.na(ppl.main$lat_cen) &
  is.na(ppl.main$lat_ggl)
x %>% table()

# FALSE    TRUE 
# 1321591    2926 
table(is.na(ppl.main$lat))
# FALSE    TRUE 
# 1321591    2926 

# checking using geocode_type
table(ppl.main$geocode_type, useNA = "ifany")

# census   city google   zip1   zip2   <NA> 
# 954305    677 247585  98900  20124   2926 

# google
x <- which(ppl.main$geocode_type == "google")
x <- ppl.main$lat[x] == ppl.main$lat_ggl[x]
table(x, useNA = "ifany")
# TRUE 
# 247585 

# census
x <- which(ppl.main$geocode_type == "census")
x <- ppl.main$lat[x] == ppl.main$lat_cen[x]
table(x, useNA = "ifany")
# TRUE 
# 954305 

# zip1
x <- which(ppl.main$geocode_type == "zip1")
x <- ppl.main$lat[x] == ppl.main$lat_zip1[x]
table(x, useNA = "ifany")
# TRUE 
# 98900 

# zip2
x <- which(ppl.main$geocode_type == "zip2")
x <- ppl.main$lat[x] == ppl.main$lat_zip2[x]
table(x, useNA = "ifany")
# TRUE 
# 20124 

# cty
x <- which(ppl.main$geocode_type == "city")
x <- ppl.main$lat[x] == ppl.main$lat_cty[x]
table(x, useNA = "ifany")
# TRUE 
# 677 

setwd( lf )
saveRDS(ppl.main, "PEOPLE-2014-2021v5.rds")


##### EXPLORING THE DATA #####
#4. Exploring data

setwd( lf )
npo.main <- readRDS("NONPROFITS-2014-2021v5.rds")
ppl.main <- readRDS("PEOPLE-2014-2021v5.rds")

#4.1 Summary of geocoding types

#For the NPO dataset

x <- table(npo.main$geocode_type, useNA = "ifany")
y <- prop.table(x)
summary <- as.data.frame(t(rbind(x,y)))
colnames(summary) <- c("frequency", "percent")
summary[nrow(summary)+1,] <- c(sum(summary$frequency), 1)
summary$percent <- paste0(round(summary$percent*100,2)," %")
rownames(summary)[nrow(summary)] <- "TOTAL"
summary <- summary[c(3,1,4,5,2,6,7),]
summary

# frequency percent
# google     64141 17.07 %
# census    259181 68.96 %
# zip1       39862 10.61 %
# zip2       12326  3.28 %
# city         142  0.04 %
# NA.          167  0.04 %
# TOTAL     375819   100 %

#For the PPL dataset

x <- table(ppl.main$geocode_type, useNA = "ifany")
y <- prop.table(x)
summary <- as.data.frame(t(rbind(x,y)))
colnames(summary) <- c("frequency", "percent")
summary[nrow(summary)+1,] <- c(sum(summary$frequency), 1)
summary$percent <- paste0(round(summary$percent*100,2)," %")
rownames(summary)[nrow(summary)] <- "TOTAL"
summary <- summary[c(3,1,4,5,2,6,7),]
summary

# frequency percent
# google    247585 18.69 %
# census    954305 72.05 %
# zip1       98900  7.47 %
# zip2       20124  1.52 %
# city         677  0.05 %
# NA.         2926  0.22 %
# TOTAL    1324517   100 %
#
