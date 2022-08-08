
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


ppl <- readRDS( 'PEOPLE-2014-2021v6.rds' )



### Obtain/Wrangle NPO addresses ###
# use Case.Number as the row-identifier since there are duplicates in EIN

npo.keep <- npo %>%
  select( Case.Number,
          YR,
          GEOID,
          ORGNAME,
          Nteecode,
          lat.npo = lat, 
          lon.npo = lon )


length( unique( npo.keep$Case.Number ) )
nrow( npo.keep )
# 55 duplicates


# let's look at them

dups <- npo.keep %>%
  filter( duplicated( Case.Number ) ) %>%
  select( Case.Number )

# print the duplicates in the dataset
npo.keep %>% 
  filter( Case.Number %in% dups$Case.Number ) %>%
  arrange( Case.Number ) 

# after careful inspection of the above it shows that all the duplicates have the same
# lat/lon coordinates as their counterparts other than 2 organizations. It is likely that the 
# duplicates represent organizations that had to submit updated paperwork later on. However, 
# for the 2 organizations with slightly different coordinates as their duplicates, we will,
# for the time being, select one of the two duplicates randomly.


npo.distinct <- npo.keep %>%
  distinct( )

length( unique( npo.keep$Case.Number ) )
nrow( npo.keep )


### Board Member Data Wrangling ###

ppl.keep <- ppl %>%
  select( Case.Number, lat, lon ) %>%
  group_by( Case.Number ) %>%
  mutate( bm = paste0( "BM.", row_number() ) ) %>% # create board member identifier within an NPO
  ungroup() %>%
  arrange( Case.Number )

# now pivot wider to have all BM coordinates in single row for easy merge with NPO data
ppl.wide <- ppl.keep %>%
  pivot_wider( names_from = bm, 
               values_from = c( lat, lon ),
               names_sep = "." )


# Merge with NPO coordinate data
npo.ppl <- left_join( npo.distinct, ppl.wide )



### Use data to calculate distances and make a sample spatial grid ###





