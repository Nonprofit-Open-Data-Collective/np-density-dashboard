
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



# obtain NPO addresses

npo %>%
  distinct( Case.Number )
