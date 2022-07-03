###---------------------------------------------------
###   06-GEOCODING ZIPS AND POB'S
###---------------------------------------------------

# 
# In this script we will augment the dataset with selected census variables using the geolocations.
# 
# We will use multiple methods to include census data…
# 
# We are using the IPUMS GeoMaker Tool, which attaches contextual data to your point data by determining the census geographic unit in which each point lies and attaching characteristics of that unit to the point record. The initial release of GeoMarker attaches data from the 2017 American Community Survey 5-year data at the census tract level.
# 
# Steven Manson, Jonathan Schroeder, David Van Riper, and Steven Ruggles. IPUMS National Historical Geographic Information System: Version 14.0 [Database]. Minneapolis, MN: IPUMS. 2019. http://doi.org/10.18128/D050.V14.0
# 
# NOTE: using this data needs a request, see https://nhgis.org/research/citation
# 

library( tidyverse )

########## %notIn% operator #########
`%notin%` <- Negate(`%in%`)

lf <- "/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Geospatial Dashboard/Large-Files-Bank/"

# read in NPO data
setwd( lf )
npo <- readRDS("NONPROFITS-2014-2021v5.rds")


npo.ipums <- select( npo, key, lat, lon )
npo.ipums <- na.omit( npo.ipums )

write.csv( npo.ipums, "6_CensusData/NPO-ipums.csv", row.names=F )


## Manually querying IPUMS Geomaker to get census data ##
 
#Loading NPO census data results


npo.ipumsGEO <- read.csv("/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Geospatial Dashboard/Large-Files-Bank/6_CensusData/NPO-ipumsGEO.csv", stringsAsFactors = F, row.names = NULL)


# The output of the census merge has a duplicated data point.

# input data
x <- npo.ipums$key
length(x)
length(unique(x))
# 375652

# results from IPUMS Geomaker
x <- npo.ipumsGEO$key
length(x)

length(unique(x))

# identifying the duplicates
x <- npo.ipumsGEO$key
npo.dup <- npo.ipumsGEO$key[which(duplicated(x))]

# subsetting the duplicated values to take a look
x <- which(npo.ipumsGEO$key %in% npo.dup)
npo.dat <- npo.ipumsGEO[x,]

npo.dat[,]

x <- which(npo.ipums$key %in% npo.dup)
dup <- npo.ipums[x,]

setwd( paste0( lf, "/6_CensusData/"))
write.csv(dup, "NPOdup-again.csv", row.names = F)
### run these through IPUMS again


dupGEO <- read.csv("NPOdup-againGEO.csv", row.names = NULL, stringsAsFactors = F)
dupGEO

# We get the same issue, double results.
# 

# getting the actual address in the npo main.
x <- which(npo$key %in% npo.dup)
npo$input_address[x] # [1] "240 N 1ST STREET, CHOWCHILLA, CA, 93610"
# [2] "1508 HARDING AVE, CHOWCHILLA, CA, 93610"

# we use google maps to get the lat/lon 
x <- rbind( c(37.125284, -120.260293), c(37.11033233813192, -120.2679625449784) )
dup <- data.frame(key=c(1,2), lat=x[,1], lon = x[,2], row.names = NULL)


setwd( paste0( lf, "/6_CensusData/"))
write.csv(dup, "NPOdup-ggl-latlon.csv", row.names = F)
# run through IPUMS
dupGEO <- read.csv("NPOdup-ggl-latlonGEO.csv", row.names = NULL, stringsAsFactors = F)
dupGEO

# Despite using different lat/lons, we get the same issue, double results.
# 
# Manually identifying the census tract of the address
# 
# Using this website and google maps, we were able to determine that the location is within tract 300.
# 
# 
# Updating the IPUMS results to exclude the duplicate case that is not in tract 300

x <- which(npo.ipumsGEO$key == npo.dup & npo.ipumsGEO$TRACTA == 202)
npo.ipumsGEO <- npo.ipumsGEO[-x,]
setwd( paste0( lf, "/6_CensusData/"))
write.csv( npo.ipumsGEO, "NPO-ipumsGEOv2.csv", row.names=F )

# removing lat lons
npo.ipumsGEO <- npo.ipumsGEO[,-c(2,3)]

# Changing the names of the variables using the Data Dictionary - from the GM codebook

# Geographic Unit Identifiers:
#   GISJOIN: GIS Join Match Code
#   STATE: State Name
#   STATEA: State Code
#   COUNTY: County Name
#   COUNTYA: County Code
#   TRACTA: Census Tract Code

# Contextual variables: (Your file will include only those variables you requested)
#   GM001_2017: Proportion unemployed
#   GM002_2017: Proportion population in poverty
#   GM003_2017: Median household income
#   GM004_2017: Income inequality
#   GM005_2017: Proportion family households headed by single woman
#   GM006_2017: Proportion occupied housing units that are owner occupied
#   GM007_2017: Proportion African American
#   GM008_2017: Proportion of adults who completed high school
#   GM009_2017: Persons per square kilometer
#   GM010_2017: Housing units per square kilometer

npo.ipumsGEO <- rename( npo.ipumsGEO, 
                        STATEFIPS = STATEA, 
                        COUNTYFIPS = COUNTYA,
                        TRACTFIPS = TRACTA,
                        unemp = GM001_2019,
                        poverty = GM002_2019,
                        medinc = GM003_2019, 
                        inequality = GM004_2019, 
                        single=GM005_2019, 
                        ownerocc = GM006_2019, 
                        black = GM007_2019, 
                        hs = GM008_2019, 
                        p.density = GM009_2019, 
                        h.density = GM010_2019 )

npo.cen <- left_join(npo, npo.ipumsGEO, by = "key")
setwd( lf )
saveRDS(npo.cen, "NONPROFITS-2014-2021v6.rds")

# 1.5 Exploring the amount of cases with Census data
# 
# Cases with census data

x <- is.na(npo.cen$poverty) %>% table()
x <- as.data.frame(x)
x <- cbind(x, paste0(round(prop.table(x$Freq) * 100,1),"%"))
names(x) <- c("No Census data", "Freq", "%")
x


x <- table(npo.cen$geocode_type, useNA = "ifany")
x <- as.data.frame(x)
x <- cbind(x, paste0(round(prop.table(x$Freq) * 100,1),"%"))
names(x) <- c("geocode_type", "Freq", "%")
x <- x[c(3,1,4,5,2,6),]
row.names(x) <- NULL
x

x <- table(is.na(npo.cen$lat), useNA = "ifany")
x <- as.data.frame(x)
x <- cbind(x, paste0(round(prop.table(x$Freq) * 100,1),"%"))
names(x) <- c("No lat/lon", "Freq", "%")
x



# 2. Getting Board Member Census Data for board members
# 2.1 Subsetting data

# Loading file
setwd( lf )
ppl <- readRDS("PEOPLE-2014-2021v5.rds")

# Preparing input file for the IPUMS Geomarker. In this case, we need to divide the addresses in chunks

ppl.ipums <- select( ppl, key, lat, lon )
ppl.ipums <- na.omit( ppl.ipums )
write.csv( ppl.ipums, "6_CensusData/PPL-ipums.csv", row.names=F )


ppl.ipums1 <- ppl.ipums[1:235000,]
ppl.ipums2 <- ppl.ipums[235001:470000,]
ppl.ipums3 <- ppl.ipums[470001:705000,]
ppl.ipums4 <- ppl.ipums[705001:940000,]
ppl.ipums5 <- ppl.ipums[940001:1175000,]
ppl.ipums6 <- ppl.ipums[1175001:nrow(ppl.ipums),]


write.csv( ppl.ipums1, "6_CensusData/PPL-ipums1.csv", row.names=F )
write.csv( ppl.ipums2, "6_CensusData/PPL-ipums2.csv", row.names=F )
write.csv( ppl.ipums3, "6_CensusData/PPL-ipums3.csv", row.names=F )
write.csv( ppl.ipums4, "6_CensusData/PPL-ipums4.csv", row.names=F )
write.csv( ppl.ipums5, "6_CensusData/PPL-ipums5.csv", row.names=F )
write.csv( ppl.ipums6, "6_CensusData/PPL-ipums6.csv", row.names=F )

### NOW READ THROUGH IPUMS ###

ppl.ipums1GEO <- read.csv("6_CensusData/PPL-ipums1GEO.csv", stringsAsFactors = F, row.names = NULL)
ppl.ipums2GEO <- read.csv("6_CensusData/PPL-ipums2GEO.csv", stringsAsFactors = F, row.names = NULL)
ppl.ipums3GEO <- read.csv("6_CensusData/PPL-ipums3GEO.csv", stringsAsFactors = F, row.names = NULL)
ppl.ipums4GEO <- read.csv("6_CensusData/PPL-ipums4GEO.csv", stringsAsFactors = F, row.names = NULL)
ppl.ipums5GEO <- read.csv("6_CensusData/PPL-ipums5GEO.csv", stringsAsFactors = F, row.names = NULL)
ppl.ipums6GEO <- read.csv("6_CensusData/PPL-ipums6GEO.csv", stringsAsFactors = F, row.names = NULL)

# binding all
ppl.ipumsGEO <- rbind( ppl.ipums1GEO, ppl.ipums2GEO, ppl.ipums3GEO, ppl.ipums4GEO,
                      ppl.ipums5GEO, ppl.ipums6GEO )

# writting a compiled results rds
write.csv( ppl.ipumsGEO, "6_CensusData/PPL-ipumsGEO.csv", row.names = FALSE )

# ppl.ipumsGEO <- read.csv("Data/6_CensusData/PPL-ipumsGEO.csv", row.names = NULL, stringsAsFactors = F)

# input file
x <- ppl.ipums$key
length(x)

length(unique(x))

# results from IPUMS
x <- ppl.ipumsGEO$key
length(x)
length(unique(x)) # 11 duplicates

# identifying the duplicate IDs
x <- ppl.ipumsGEO$key
ppl.dup <- ppl.ipumsGEO$key[which(duplicated(x))]

# subsetting the duplicated values to take a look
x <- which(ppl.ipumsGEO$key %in% ppl.dup)
ppl.dat <- ppl.ipumsGEO[x,]
ppl.dat

# subsetting the duplicated values to take a look
x <- which(ppl.ipums$key %in% ppl.dup)
dup <- ppl.ipums[x,]


#subsetting the duplicate addresses
x <- which(ppl$key %in% ppl.dup)
dup <- ppl[x,]
dup <- select(dup, key, input_address)

# using google geocoding service to get the lat lons.
library( ggmap )
api <- "AIzaSyCjEs0y-pgqTaVcsI_cCbtH5LjWkzZoDzw" # reading my personal API key from a local file

register_google(key = api) #The register_google function stores the API key.
getOption("ggmap") #summarises the Google credentials to check how you are connected.
dup <- mutate_geocode(dup, input_address, output = "latlona", source = "google", messaging = T) #generates an object where the original dataset is binded with the geocode results.

setwd( paste0( lf, "6_CensusData/" ) )
dir.create("dups")
setwd( paste0( lf, "6_CensusData/dups" ) )
saveRDS(dup, "GoogleResults1.rds")

# formatting the new lat/lons for submitting to the Geomaker
dup <- dup[,c(1,3,4)]
write.csv(dup, "PPLdup-ggl-latlon.csv", row.names = F)

dupGEO <- read.csv("PPLdup-ggl-latlonGEO.csv", row.names = NULL, stringsAsFactors = F)

left_join(dupGEO, ppl[c('key', 'input_address')])%>%
  select(key, TRACTA, input_address)
# all duplicates are in Madera county with duplicate tract #'s: 300 and 202
# Using Ignacio's previous documentation, we confirm that they are all in tract 300
# so we remove those with TRACTA==202

x <- which(dupGEO$TRACTA == 202)
dupGEO <- dupGEO[-x,]

setwd( paste0( lf, "6_CensusData/dups" ) )

write.csv(dupGEO, "PPLdup-ggl-latlonGEOfinal.csv", row.names = F)


# removing lat lons
ppl.ipumsGEO <- ppl.ipumsGEO[,-c(2,3)]%>%
  ppl.ipumsGEO%>%
  filter(!(key %in% dupGEO$key & TRACTA==202 ) )# fix duplicates by removing redundant rows

x <- ppl.ipumsGEO$key
length(x)
length(unique(x)) # no more duplicates

# Changing the names of the variables using the Data Dictionary - from the GM codebook

# Geographic Unit Identifiers:
#   GISJOIN: GIS Join Match Code
#   STATE: State Name
#   STATEA: State Code
#   COUNTY: County Name
#   COUNTYA: County Code
#   TRACTA: Census Tract Code

# Contextual variables: (Your file will include only those variables you requested)
#   GM001_2017: Proportion unemployed
#   GM002_2017: Proportion population in poverty
#   GM003_2017: Median household income
#   GM004_2017: Income inequality
#   GM005_2017: Proportion family households headed by single woman
#   GM006_2017: Proportion occupied housing units that are owner occupied
#   GM007_2017: Proportion African American
#   GM008_2017: Proportion of adults who completed high school
#   GM009_2017: Persons per square kilometer
#   GM010_2017: Housing units per square kilometer

ppl.ipumsGEO <- rename( ppl.ipumsGEO, 
                        STATEFIPS = STATEA, 
                        COUNTYFIPS = COUNTYA,
                        TRACTFIPS = TRACTA,
                        unemp = GM001_2019,
                        poverty = GM002_2019,
                        medinc = GM003_2019, 
                        inequality = GM004_2019, 
                        single=GM005_2019, 
                        ownerocc = GM006_2019, 
                        black = GM007_2019, 
                        hs = GM008_2019, 
                        p.density = GM009_2019, 
                        h.density = GM010_2019 )
  
# merging
ppl.cen <- left_join(ppl, ppl.ipumsGEO, by = "key")

#Finally, we add the the google geocode information we got for the 7 duplicated cases and update their geocode_type from census to google.

# google geo results
ggl.ppl <- readRDS("GoogleResults1.rds")
names(ggl.ppl)

# identifying the cases to edit
x <- which(ppl.cen$key %in% ggl.ppl$key)

# test to check the reference is OK
ppl.cen$key[x] == ggl.ppl$key

ppl.cen[x,c(2,34,35,36)]
ppl.cen[x,c(34,35,36)] <- ggl.ppl[,-c(1,2)]

# making "google" as geocode_type and updating the final lat lons with the google geocodes.
ppl.cen$geocode_type[x] <- "google"
ppl.cen$lat[x] <- ppl.cen$lat_ggl[x]
ppl.cen$lon[x] <- ppl.cen$lon_ggl[x]

setwd( paste0( lf ) )

saveRDS(ppl.cen, "PEOPLE-2014-2021v6.rds")

#2.5 Exploring the amount of cases with Census data

#Cases with census data

x <- is.na(ppl.cen$poverty) %>% table()
x <- as.data.frame(x)
x <- cbind(x, paste0(round(prop.table(x$Freq) * 100,1),"%"))
names(x) <- c("No Census data", "Freq", "%")
(x)

x <- table(ppl.cen$geocode_type, useNA = "ifany")
x <- as.data.frame(x)
x <- cbind(x, paste0(round(prop.table(x$Freq) * 100,1),"%"))
names(x) <- c("geocode_type", "Freq", "%")
x <- x[c(3,1,4,5,2,6),]
row.names(x) <- NULL
(x)


#Cases with final latitude/longitude

x <- table(is.na(ppl.cen$lat), useNA = "ifany")
x <- as.data.frame(x)
x <- cbind(x, paste0(round(prop.table(x$Freq) * 100,1),"%"))
names(x) <- c("No lat/lon", "Freq", "%")
x
