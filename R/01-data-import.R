###---------------------------------------------------
###   01-DATA IMPORT AND WORKING DATASET CREATION
###---------------------------------------------------

library( tidyverse ) 
library( rio )  # to import xlsx files
library( gender )  # for estimating gender from data (first names)

# reference: https://nonprofit-open-data-collective.github.io/open-1023-ez-dataset/Step-01-ProcessRawData.html

## Part 1: Import and Merge into Working Dataset

url <- c( 'https://www.irs.gov/pub/irs-tege/f1023ez_approvals_2014.xlsx',
         'https://www.irs.gov/pub/irs-tege/f1023ez_approvals_2015.xlsx',
         'https://www.irs.gov/pub/irs-tege/f1023ez_approvals_2016.xlsx',
         'https://www.irs.gov/pub/irs-tege/f1023EZ_approvals_2017.xlsx',
         'https://www.irs.gov/pub/irs-tege/f1023ez_approvals_2018.xlsx',
         'https://www.irs.gov/pub/irs-tege/f1023ez_approvals_2019.xlsx',
         'https://www.irs.gov/pub/irs-tege/f1023ez-approvals-2020.xlsx',
         'https://www.irs.gov/pub/irs-tege/f1023ez-approvals-2021.xlsx' )

# import data using lapply and store in list
df <- lapply( url, function( x )  rio::import( file = x ) )

# assign elements of list as individual dataframes, names dd1:dd8
for ( i in 1:length ( url ) ) {
  dds <- paste0( 'dd', 1:length ( url ) )
  assign( dds[i], df[[i]] )
}

# now, replicating code provided in: https://nonprofit-open-data-collective.github.io/open-1023-ez-dataset/Step-01-ProcessRawData.html
setwd('/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Geospatial Dashboard/R code for now/Data-Raw' )

# save raw 2014 dataset
dd1$EIN <- gsub( "-", "", dd1$EIN )
dd1$ID <- paste0( "ID-", 2014, "-", dd1$EIN )
saveRDS( dd1, "f1023ez_approvals_2014.rds" ) 

# save raw 2015 dataset
dd2$EIN <- gsub( "-", "", dd2$EIN )
dd2$ID <- paste0( "ID-", 2015, "-", dd2$EIN )
saveRDS( dd2, "f1023ez_approvals_2015.rds" ) 

# save raw 2016 dataset
dd3$EIN <- gsub( "-", "", dd3$EIN )
dd3$ID <- paste0( "ID-", 2016, "-", dd3$EIN )
saveRDS( dd3, "f1023ez_approvals_2016.rds" ) 

# save raw 2017 dataset
dd4$EIN <- gsub( "-", "", dd4$EIN )
dd4$ID <- paste0( "ID-", 2017, "-", dd4$EIN )
saveRDS( dd4, "f1023ez_approvals_2017.rds" ) 

# save raw 2018 dataset
dd5$EIN <- gsub( "-", "", dd5$EIN )
dd5$ID <- paste0( "ID-", 2018, "-", dd5$EIN )
saveRDS( dd5, "f1023ez_approvals_2018.rds" ) 

# save raw 2019 dataset
colnames( dd6 ) [1] <- "EIN" # NOTE that this file has "Ein" -> fixing it to "EIN" 
dd6$EIN <- gsub( "-", "", dd6$EIN )
dd6$ID <- paste0( "ID-", 2019, "-", dd6$EIN )
saveRDS( dd6, "f1023ez_approvals_2019.rds" ) 

# save raw 2020 dataset
colnames( dd7 ) [1] <- "EIN" # NOTE that this file has "Ein" -> fixing it to "EIN" 
dd7$EIN <- gsub( "-", "", dd7$EIN )
dd7$ID <- paste0( "ID-", 2020, "-", dd7$EIN )
saveRDS( dd7, "f1023ez_approvals_2020.rds" ) 

# save raw 2021 dataset
colnames( dd8 ) [1] <- "EIN" # NOTE that this file has "Ein" -> fixing it to "EIN" 
dd8$EIN <- gsub( "-", "", dd8$EIN )
dd8$ID <- paste0( "ID-", 2021, "-", dd8$EIN )
saveRDS( dd8, "f1023ez_approvals_2021.rds" ) 


# in case we need to repeat steps below but want to import data stored locally on machine:
# dd1 <- readRDS('f1023ez_approvals_2014.rds' ) 
# dd2 <- readRDS('f1023ez_approvals_2015.rds' ) 
# dd3 <- readRDS('f1023ez_approvals_2016.rds' ) 
# dd4 <- readRDS('f1023ez_approvals_2017.rds' ) 
# dd5 <- readRDS('f1023ez_approvals_2018.rds' ) 
# dd6 <- readRDS('f1023ez_approvals_2019.rds' ) 
# dd7 <- readRDS('f1023ez_approvals_2020.rds' ) 
# dd8 <- readRDS('f1023ez_approvals_2021.rds' ) 


# Binding all rows into single dataset

# 2021 vs 2017, The elements of setdiff( x,y )  are those elements in x but not in y
setdiff( names( dd8 ), names( dd4 ) ) # after 2017, we see differences in the column names

setdiff( names( dd4 ), names( dd5 ) ) # 2018 datafile has duplicated “Gamingactyno” and “Gamingactyyes” columns

identical( dd8$Gamingactyno...103, dd8$Gamingactyno...91 )  # “Gamingactyno” and “Gamingactyyes” in 2018 datafile both have identical data.

# Change column names for 2018-2021
# For binding with the data of previous years, the name of one of the duplicated columns will be set 
# back to its original name ( removing the column #). We will leave the duplicated variables present in 
# 2018 and 2019 data and will rename them with a “.1” at the end.
colnames( dd5 ) [91] <- "Gamingactyno"
colnames( dd5 ) [92] <- "Gamingactyyes"
colnames( dd5 ) [103] <- "Gamingactyno.1"
colnames( dd5 ) [104] <- "Gamingactyyes.1"

colnames( dd6 ) [91] <- "Gamingactyno"
colnames( dd6 ) [92] <- "Gamingactyyes"
colnames( dd6 ) [103] <- "Gamingactyno.1"
colnames( dd6 ) [104] <- "Gamingactyyes.1"

colnames( dd7 ) [91] <- "Gamingactyno"
colnames( dd7 ) [92] <- "Gamingactyyes"
colnames( dd7 ) [103] <- "Gamingactyno.1"
colnames( dd7 ) [104] <- "Gamingactyyes.1"

colnames( dd8 ) [91] <- "Gamingactyno"
colnames( dd8 ) [92] <- "Gamingactyyes"
colnames( dd8 ) [103] <- "Gamingactyno.1"
colnames( dd8 ) [104] <- "Gamingactyyes.1"

# final row bind
dat <- bind_rows( dd1, dd2, dd3, dd4, dd5, dd6, dd7, dd8 )

# check for missings in EIN and case number
sum( is.na( dat$EIN ) )
sum( is.na( dat$`Case Number` ) )

# Current dataset has two variables for ORGNAME and they seem to be a single name split into the two vars.
head( dat[,c("Orgname1", "Orgname2" ) ],5 ) 

x <- dat[,c("Orgname1", "Orgname2" ) ] # subsetting only orgnames to compare
x <- x[!is.na( x$Orgname2 ) ,] # removing NAs from Orgname2
x$Org2len <- nchar( as.character( x$Orgname2 ) )
x <- x[order( x$Org2len, decreasing = T ) ,]
head( x, 10 ) 

# Merging Orgname 1 and 2 to create variable ORGNAME
dat$ORGNAME <- dat$Orgname1
x <- is.na( dat$Orgname2 ) 
x <- dat[!x,c("Orgname1","Orgname2" ) ]
x[1:10,]


x <- is.na( dat$Orgname2 ) 
dat$ORGNAME[!x] <- paste0( dat$ORGNAME[!x], dat$Orgname2[!x])
x <- dat[!x,"ORGNAME"]
x[1:10,]


colnames( dat ) [2] <- "Case.Number"
dat <- unique( dat ) 
dat <- as_tibble( dat ) 

# save
setwd('/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Geospatial Dashboard/R code for now/Data-Wrangled' )
saveRDS( dat, "NONPROFIT-ADDRESSES-2014-2021.rds" ) 

head( dat, 10 )  # Check


## Part 2: Build Non-profit Addresses Dataset

# Using the NONPROFIT-ADDRESSES-2014-2019.rds dataset to make a new file with only the NPO data necessary.
keep.these <- 
  c( "ORGNAME","ID","Mission",
     "EIN","Orgname1", "Orgname2",
     "Case.Number", "Formrevision", "Eligibilityworksheet", 
     "Address", "City", "State", "Zip", "Zippl4", 
     "Accountingperiodend", "Userfeesubmitted",
     "Orgurl",
     "Orgtypecorp", "Orgtypeunincorp", "Orgtypetrust", 
     "Necessaryorgdocs", "Incorporateddate", "Incorporatedstate", 
     "Containslimitation", "Doesnotexpresslyempower", "Containsdissolution", 
     "Nteecode", "Orgpurposecharitable", "Orgpurposereligious", 
     "Orgpurposeeducational", "Orgpurposescientific", 
     "Orgpurposeliterary", "Orgpurposepublicsafety", 
     "Orgpurposeamateursports", "Orgpurposecrueltyprevention", 
     "Qualifyforexemption","Leginflno", "Leginflyes", 
     "Compofcrdirtrustno", "Compofcrdirtrustyes", 
     "Donatefundsno", "Donatefundsyes", "Conductactyoutsideusno", 
     "Conductactyoutsideusyes", "Financialtransofcrsno", 
     "Financialtransofcrsyes", "Unrelgrossincm1000moreno", 
     "Unrelgrossincm1000moreyes", "Gamingactyno", 
     "Gamingactyyes", "Disasterreliefno", "Disasterreliefyes", 
     "Onethirdsupportpublic", "Onethirdsupportgifts", 
     "Benefitofcollege", "Privatefoundation508e", 
     "Seekingretroreinstatement", "Seekingsec7reinstatement", 
     "Gamingactyno.1", "Gamingactyyes.1", 
     "HospitalOrChurchNo", "HospitalOrChurchYes", 
     "Correctnessdeclaration", "Signaturename", 
     "Signaturetitle", "Signaturedate",    
     "EZVersionNumber" )

# these will be dropped
setdiff( names( dat ) , keep.these )

npo <- dplyr::select( dat, keep.these )

# adding YR var
x <- npo$ID
npo$YR <- substr( x, start = 4, stop = 7 ) 

# ordering variables
npo <- npo[,c( 2,1,4,68,3,5:67 ) ]

# removing duplicates
npo <- unique( npo )  # from 265,220 to 263,272
rownames( npo )  <- NULL

# explore duplicates
id.count <- as.data.frame( table( npo$ID ) )
id.count <- id.count[order( id.count$Freq, decreasing = T ) ,]
id.count$Var1 <- as.character( id.count$Var1 ) 
rownames( id.count )  <- NULL
names( id.count )  <- c("ID", "IDdup" ) 
head( id.count )  # some IDs are repeated


# joining IDdup to the dataset
npo <- left_join( npo, id.count, by = "ID" ) 

# subsetting those with duplicates
x <- npo$IDdup > 1
dups <- npo[x,] 
dups <- dups[order( dups$IDdup, decreasing = T ) ,c( 1,69,2:68 ) ]

dups[1:6,c( 1,3,7,8 ) ]

# Adding a key variable
npo <- npo[order( npo$ID ) ,]
npo$key <- 1:nrow( npo ) 

# rearranging columns
npo <- npo[,c( 1,70,2:69 ) ]

# saving
npo <- as_tibble( npo ) 
saveRDS( npo, "NONPROFITS-2014-2021.rds" ) 


## Part 3: Board Member Addresses Dataset

# first member listed in the npos
d1 <- 
  dat %>%
  select( ID, ORGNAME, EIN, Signaturedate, Case.Number,  
          Ofcrdirtrust1firstname, Ofcrdirtrust1lastname, 
          Ofcrdirtrust1title, Ofcrdirtrust1streetaddr, 
          Ofcrdirtrust1city, Ofcrdirtrust1state, 
          Ofcrdirtrust1zip, Ofcrdirtrust1zippl4 ) 

# standardizing the variable names for binding
nmz <- names( d1 ) 
nmz <- gsub( "Ofcrdirtrust[1-9]", "Ofcrdirtrust", nmz )
names( d1 ) <- nmz

# adding board member #
d1$ID <- paste0( d1$ID, "-01" )

# second member listed in the npos
d2 <- 
  dat %>% 
  select( ID, ORGNAME, EIN, Signaturedate, Case.Number,
          Ofcrdirtrust2firstname, Ofcrdirtrust2lastname, 
          Ofcrdirtrust2title, Ofcrdirtrust2streetaddr, 
          Ofcrdirtrust2city, Ofcrdirtrust2state, 
          Ofcrdirtrust2zip, Ofcrdirtrust2zippl4 )
nmz <- names( d2 ) 
nmz <- gsub( "Ofcrdirtrust[1-9]", "Ofcrdirtrust", nmz )
names( d2 ) <- nmz
d2$ID <- paste0( d2$ID, "-02" )

# third member listed in the npos
d3 <- 
  dat %>% 
  select( ID, ORGNAME, EIN, Signaturedate, Case.Number,
          Ofcrdirtrust3firstname, Ofcrdirtrust3lastname, 
          Ofcrdirtrust3title, Ofcrdirtrust3streetaddr, 
          Ofcrdirtrust3city, Ofcrdirtrust3state,
          Ofcrdirtrust3zip, Ofcrdirtrust3zippl4 )
nmz <- names( d3 ) 
nmz <- gsub( "Ofcrdirtrust[1-9]", "Ofcrdirtrust", nmz )
names( d3 ) <- nmz
d3$ID <- paste0( d3$ID, "-03" )

# fourth member listed in the npos
d4 <- 
  dat %>% 
  select( ID, ORGNAME, EIN, Signaturedate, Case.Number, 
          Ofcrdirtrust4firstname, Ofcrdirtrust4lastname, 
          Ofcrdirtrust4title, Ofcrdirtrust4streetaddr, 
          Ofcrdirtrust4city, Ofcrdirtrust4state, 
          Ofcrdirtrust4zip, Ofcrdirtrust4zippl4 )
nmz <- names( d4 ) 
nmz <- gsub( "Ofcrdirtrust[1-9]", "Ofcrdirtrust", nmz )
names( d4 ) <- nmz
d4$ID <- paste0( d4$ID, "-04" )

# fifth member listed in the npos
d5 <- 
  dat %>% 
  select( ID, ORGNAME, EIN, Signaturedate, Case.Number, 
          Ofcrdirtrust5firstname, Ofcrdirtrust5lastname, 
          Ofcrdirtrust5title, Ofcrdirtrust5streetaddr, 
          Ofcrdirtrust5city, Ofcrdirtrust5state, 
          Ofcrdirtrust5zip, Ofcrdirtrust5zippl4 )
nmz <- names( d5 ) 
nmz <- gsub( "Ofcrdirtrust[1-9]", "Ofcrdirtrust", nmz )
names( d5 ) <- nmz
d5$ID <- paste0( d5$ID, "-05" )

# binding all data
ppl <- bind_rows( d1, d2, d3, d4, d5 ) # all people data as individual cases.

# identifying empty cases ( NAs in all fields ) 
x <- is.na( ppl$Ofcrdirtrustfirstname )  & 
  is.na( ppl$Ofcrdirtrustlastname )  & 
  is.na( ppl$Ofcrdirtrusttitle )  & 
  is.na( ppl$Ofcrdirtruststreetaddr )  & 
  is.na( ppl$Ofcrdirtrustcity )  & 
  is.na( ppl$Ofcrdirtruststate ) 

# removing empty cases
ppl <- ppl[!x, ] 

# removing duplicated data
ppl <- unique( ppl ) 
rownames( ppl )  <- NULL

# adding YR var
x <- ppl$ID
ppl$YR <- substr( x, start = 4, stop = 7 ) 
pander( table( ppl$YR ) )

# Renaming variables to more friendly names
names( ppl ) 

x <- c("Firstname", "Lastname", "Title", "Address", "City", "State", "Zip", "Zippl4" ) 
names( ppl ) [6:13] <- x
names( ppl ) 

# arranging order
ppl <- arrange( ppl, ID ) # ordering by ID ( all board members of the same org together ) 


# creating a unique list of first names
first.names <- unique( ppl$Firstname )
gender.codes <- gender( first.names ) 

# selecting relevant variables
gender.codes <- select( gender.codes, name, gender, proportion_male )
head( gender.codes ) 

ppl <- left_join( ppl, gender.codes, by = c("Firstname" = "name" ) )

x <- round( prop.table( table( ppl$gender, useNA ="ifany" )),2 ) 
table( x )

# ordering variables
nmz <- c("ID",
         "ORGNAME",
         "EIN",
         "YR",
         "Signaturedate",
         "Case.Number",
         "Firstname",
         "Lastname",
         "Title",
         "Address",
         "City",
         "State",
         "Zip",
         "Zippl4",
         "gender",
         "proportion_male" )  

ppl <- ppl[,nmz]

id.count <- as.data.frame( table( ppl$ID ) )
id.count <- id.count[order( id.count$Freq, decreasing = T ) ,]
head( id.count )  # some IDs are repeated

# making key variable =
names( id.count )  <- c("ID", "IDdup" ) 
id.count$ID <- as.character( id.count$ID ) 

# joining count
ppl <- left_join( ppl, id.count, by = "ID" ) 

# subsetting those with duplicates
x <- ppl$IDdup > 1
dups <- ppl[x,] 
names( dups ) 

dups <- dups[order( dups$IDdup, decreasing = T ) ,c( 1,17,2:16 ) ]

# Given ID is not unique, we will create a key variable that is unique
# Adding a key variable
ppl <- ppl[order( ppl$ID ) ,]
ppl$key <- 1:nrow( ppl ) 

names( ppl ) 

# rearranging columns
ppl <- ppl[,c( 1,18,2:17 ) ]

ppl <- as_tibble( ppl ) 

setwd('/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Geospatial Dashboard/R code for now/Data-Wrangled' )

saveRDS( ppl, "PEOPLE-2014-2021.rds" )
