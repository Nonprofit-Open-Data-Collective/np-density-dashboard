wd<-("/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Geospatial Dashboard/Large-Files-Bank/Ignacio-data-bank/Open_data_ignacio/Data")

setwd(paste0(wd,"/4_GeoGoogle/Results"))



#### NPO Addresses ####
# loading results file
npo.res <- readRDS("NPOAddresses_googleGEO.rds")
npo.res <- npo.res[,-c(1,2)]
names(npo.res) <- c("input_address", "lon_ggl", "lat_ggl", "address_ggl")

# loading main
lf <- "/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Geospatial Dashboard/Large-Files-Bank"
setwd( lf )


# Adding a geocode_type variable to all Addresses geocoded by google
# loading main files
npo.main.b <- readRDS("NONPROFITS-2014-2021v3.rds") 


# join files
npo.main.b <- left_join(npo.main.b, npo.res, by = "input_address")

x <- which(!is.na(npo.main.b$address_ggl))
npo.main.b$geocode_type[x] <- "google"

#Saving the new version of the ppl.main file
saveRDS(npo.main.b, "NONPROFITS-2014-2021v4.rds")


#### PPL Addresses ####
# Loading addresses to geocode
setwd(paste0(wd,"/4_GeoGoogle/Results"))

# loading results file
ppl.res <- readRDS("PPLAddresses_googleGEO.rds")
ppl.res <- ppl.res[,-c(1,2)]
names(ppl.res) <- c("input_address", "lon_ggl", "lat_ggl", "address_ggl")

# loading main
setwd( lf )
ppl.main <- readRDS("PEOPLE-2014-2021v3.rds")

#Joining files

ppl.main.b <- left_join(ppl.main, ppl.res, by = "input_address")

#Adding a geocode_type variable to all Addresses geocoded by google

x <- which(!is.na(ppl.main.b$address_ggl))
ppl.main.b$geocode_type[x] <- "google"

#Saving the new version of the ppl.main file

saveRDS(ppl.main.b, "PEOPLE-2014-2021v4.rds")

> nrow(ppl.main)
[1] 1324517
> nrow(npo.main)
[1] 375819


## Check who needs to be geocoded from newer data

lf <- "/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Geospatial Dashboard/Large-Files-Bank"
setwd( lf )

########## %notin% operator #########
`%notin%` <- Negate(`%in%`)

# loading main files
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
saveRDS(ppl, "LO-NPO-addresses-google.rds")



######## In total we need 74544+25274 = 99,818 geocodes which puts us well over the monthly limit of 40,000 calls ######## 




# reseting ID and removing duplicate addresses

dir.create("4_GeoGoogle")
wd2 <- paste0(lf, "/4_GeoGoogle")

npo$ID <- 0
npo <- unique(npo)



# check old records
ig<-readRDS("/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Geospatial Dashboard/Large-Files-Bank/Ignacio-data-bank/Open_data_ignacio/Data/2_InputData/PEOPLE-2014-2019.rds")


my<-readRDS("/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Geospatial Dashboard/np-density-dashboard/Data-Wrangled/PEOPLE-2014-2021.rds")


nrow(my)-nrow(ig)
 
nrow(my[which(my$Address %notin% ig$Address),])
#353431 more ppl in my data than ignacios

ig<-readRDS("/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Geospatial Dashboard/Large-Files-Bank/Ignacio-data-bank/Open_data_ignacio/Data/2_InputData/NONPROFITS-2014-2019.rds")


my<-readRDS("/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Geospatial Dashboard/np-density-dashboard/Data-Wrangled/NONPROFITS-2014-2021.rds")


nrow(my)-nrow(ig)

#112547 more nonprofits in my data than ignacios

setwd( '/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Geospatial Dashboard/np-density-dashboard/Data-Raw' )

 dd1 <- readRDS( 'f1023ez_approvals_2014.rds' ) 
 dd2 <- readRDS( 'f1023ez_approvals_2015.rds' ) 
 dd3 <- readRDS( 'f1023ez_approvals_2016.rds' ) 
 dd4 <- readRDS( 'f1023ez_approvals_2017.rds' ) 
 dd5 <- readRDS( 'f1023ez_approvals_2018.rds' ) 
 dd6 <- readRDS( 'f1023ez_approvals_2019.rds' ) 
 
 nrow(rbind(dd1,dd2,dd3,dd4,dd5,dd6)
 