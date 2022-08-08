###--------------------------------------------------------------
###   09-SHAPEFILES FOR BOARDMEMBER(BM) DATA AND SPATIAL GRIDS
###--------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------------------------------------------------
# 
# In this script, we will generate some general shapefiles (to be stored as .rds) that will be used for
# generating the spatial grids in the dashboard. The idea will be to get all the NPO and PPL data
# into a single row so that we can then use the `distHaversine` function in the `geosphere` package
# to compute Haversine distances between the NPO and BM addresses. 
#
# Input data:  'NONPROFITS-2014-2021v7.rds' , 'PEOPLE-2014-2021v6.rds'
# Out data: 
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

library( tidyverse )
library( sf )               # simple features framework     
library( geosphere )
library( data.table )       # for manipulating data for plotting BM to NPO
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
  mutate( bm = row_number() ) %>% # create board member identifier within an NPO
  mutate( bm = ifelse( bm < 10, paste0( "BM.0", bm ), paste0( "BM.", bm ) ) ) %>%  # add leading zeros
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

# test the function using NPO location and BM 1 coordinates
mypts.npo <- data.frame( npo.ppl$lon.npo,npo.ppl$lat.npo )    # lon goes first and lat goes second in the data.frame
mypts.bm1<- data.frame( npo.ppl$lon.BM.01, npo.ppl$lat.BM.01 )


dis.miles <- distHaversine( p1 = mypts.npo,
               p2 = mypts.bm1 )*0.0006213711 # convert default meters to miles

dis.meters <- distHaversine( p1 = mypts.npo,
                          p2 = mypts.bm1 )


## Loop for all board members

# ensure this is run before loop:
mypts.npo <- data.frame( npo.ppl$lon.npo,npo.ppl$lat.npo )    

bm.tails <- c( paste0( "0", 1:9 ), 10 )
for( i in 1:10){
  
  
  start.time <- Sys.time()
  
  
  # select correct columns for respective board member
  mypts.bm <- data.frame( eval( parse( text = ( paste0( "npo.ppl$lon.BM.", bm.tails[i] ) ) ) ), 
                          eval( parse( text = ( paste0( "npo.ppl$lat.BM.", bm.tails[i] ) ) ) ) )
  
  npo.ppl[[ paste0( "dist.BM.", bm.tails[i], ".miles" ) ]] <- distHaversine( p1 = mypts.npo,
                              p2 = mypts.bm1 )*0.0006213711 # convert default meters to miles
  
  npo.ppl[[ paste0( "dist.BM.", bm.tails[i], ".meters" ) ]] <- distHaversine( p1 = mypts.npo,
                                                                            p2 = mypts.bm1 )
  
  
  end.time <- Sys.time()
  
  print( end.time - start.time)
  print( paste0( "Iteration ", i, "/", 10, " complete" ) ) 
  
  
  
}


sum(is.na(npo.ppl$lon.npo)) # 167 missing coordinates
sum(is.na(npo.ppl$lat.npo))

# remove NAs otherwise st_as_sf can't generate sf file
# put data back into long format for sf (geometries will be lines for each row
# representing a line between the NPO and the BM coordinates)
no.na <- npo.ppl%>%
  filter( !is.na( lon.npo ) ) %>%
  mutate( bm = paste0( "BM.", bm.tails[i] ),
          id = row_number() ) %>%
  select( GEOID,
          Case.Number,
          id,
          bm,
          ORGNAME,
          Nteecode,
          YR,
          contains( paste0( "lon.BM.", bm.tails[i] ) ), 
          contains( paste0( "lat.BM.", bm.tails[i] ) ),
          lon.npo,
          lat.npo )

# generate sf file


# `data.table` way: This code was modified from the reproducible example online:
# https://newbedev.com/r-create-linestring-from-two-points-in-same-row-in-dataframe

dt <- as.data.table(no.na)

dt1 <- dt[, .(id, lon = lon.BM.01, lat = lat.BM.01)]
dt2 <- dt[, .(id, lon = lon.npo, lat = lat.npo)]

## Add on a 'sequence' variable so we know which one comes first
dt1[, seq := 1L ]
dt2[, seq := 2L ]

## put back together
dt <- rbindlist(list(dt1, dt2), use.names = TRUE)
setorder(dt, id, seq)

sf <- sfheaders::sf_linestring(
  obj = dt
  , x = "lon"
  , y = "lat"
  , linestring_id = "id"
)


out.line.sf <- st_as_sf( left_join( bind.dt, sf, by = "id" ) )

cn.lev <- levels( as.factor(out.line.sf$Case.Number ) )
(d <- out.line.sf %>% filter( Case.Number == cn.lev[109] ))
ggplot( )+
  geom_sf( data = st_geometry(d), lwd = 1, lineend = "round" ) 

plot(st_geometry(d), lwd = 1, lineend = "round")

