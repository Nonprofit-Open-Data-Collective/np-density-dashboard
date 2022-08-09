###--------------------------------------------------------------
###   09-SHAPEFILES FOR BOARDMEMBER(BM) DATA AND SPATIAL GRIDS
###--------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------------------------------------------------
# 
# In this script, we will generate some general shapefiles (to be stored as .rds) that will be used for
# generating the spatial grids in the dashboard. The idea will be to get all the NPO and PPL data
# into a single row so that we can then use the `distHaversine` function in the `geosphere` package
# to compute Haversine distances between the NPO and BM addresses. The final dataset consists of a row corresponding to
# single board member with a stringline geometry that represents the line between the boardmember's home and their
# respective NPO.
#
# Input data:  'NONPROFITS-2014-2021v7.rds' , 'PEOPLE-2014-2021v6.rds'
# Out data: "BM-NPO-Spatial-Grid.rds"
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

library( tidyverse )
library( sf )               # simple features framework     
library( geosphere )
library( data.table )       # for manipulating data for plotting BM to NPO
library( rlang )

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
                              p2 = mypts.bm )*0.0006213711 # convert default meters to miles
  
  npo.ppl[[ paste0( "dist.BM.", bm.tails[i], ".meters" ) ]] <- distHaversine( p1 = mypts.npo,
                                                                            p2 = mypts.bm )
  
  
  end.time <- Sys.time()
  
  print( end.time - start.time)
  print( paste0( "Iteration ", i, "/", 10, " complete" ) ) 
  
  
  
}


sum(is.na(npo.ppl$lon.npo)) # 167 missing coordinates
sum(is.na(npo.ppl$lat.npo))


out.npo.ppl <- list()
# the following loop will take some depending on your machine's specifications
for ( i in 1:10 ){
  
  start.time <- Sys.time()
  
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

dt.a <- as.data.table( no.na )
  
  dt1 <- dt.a[, .(id, lon = get( paste0( "lon.BM.", bm.tails[i] ) ), lat =  get( paste0( "lat.BM.", bm.tails[i] ) ) )]
  dt2 <- dt.a[, .(id, lon = get( "lon.npo" ), lat = get( "lat.npo" ) )]

  ## Add on a 'sequence' variable so we know which one comes first
  dt1[, seq := 1L ]
  dt2[, seq := 2L ]

  ## put back together
  dt.b <- rbindlist( list( dt1, dt2 ), use.names = TRUE )
  setorder( dt.b, id, seq )

  sf <- sfheaders::sf_linestring(
    obj = dt.b,
    x = "lon",
    y = "lat",
    linestring_id = "id" )


  out.line.sf <- left_join( as.data.frame( dt.a ), sf, by = "id" )

  out.npo.ppl[[i]] <- out.line.sf %>% 
    arrange( Case.Number ) %>%                                            # arrange data by NPO
    distinct( Case.Number, 
              bm, 
              lon.bm = !!rlang::parse_expr( paste0( "lon.BM.", bm.tails[i] ) ), 
              lat.bm = !!rlang::parse_expr( paste0( "lat.BM.", bm.tails[i] ) ),
              lon.npo,
              lat.npo,
              geometry ) %>%                                              # delete duplicates
    filter( !is.na( !!rlang::parse_expr( "lon.bm" ) ) )                   # remove those with missing values which would indicate they don't have a BM at that position


  end.time <- Sys.time()

  print( end.time - start.time)
  print( paste0( "Iteration ", i, "/", 10, " complete" ) ) 

}


# convert dataset to sf
d <- st_as_sf( do.call( "rbind", out.npo.ppl ) )

# merge distance data to NPO data
d.f <- npo.ppl %>%
  pivot_longer( cols = contains( "miles" ), names_to = "bm", values_to = "miles" ) %>% # pivot longer
  select( miles, bm, Case.Number, ORGNAME ) %>%
  mutate( bm = str_remove_all( str_remove_all( bm, "\\.miles" ), "dist\\." ) ) %>%     # text process `bm` column
  left_join(d, ., by = c("bm", "Case.Number" ) ) %>%  # join to BM-NPO data
  group_by( Case.Number ) %>%
  mutate( avg.miles = mean( miles, na.rm = T ) )          # compute average miles to NPO within organizations

# save
setwd( paste0( lf, "/10-Spatial-Grid-Data" ) )
saveRDS( d.f, "BM-NPO-Spatial-Grid.rds")



# Example Spatial grid
cn.lev <- levels( as.factor(d$Case.Number ) )

(d.plot <- d.f %>% filter( Case.Number == cn.lev[110] ) %>%
    mutate( miles = round( miles, 2 ) ) )

# alter boundary coordinates 
# get current bounding box
current.bbox <- st_bbox( d.plot )

xrange <- current.bbox$xmax - current.bbox$xmin # range of x values
yrange <- current.bbox$ymax - current.bbox$ymin # range of y values

current.bbox[1] <- current.bbox[1] - (0.4 * xrange) # xmin - left
current.bbox[3] <- current.bbox[3] + (0.4 * xrange) # xmax - right
current.bbox[2] <- current.bbox[2] - (0.4 * yrange) # ymin - bottom
current.bbox[4] <- current.bbox[4] + (0.4 * yrange) # ymax - top


# average distance label
avg.label <- paste0( "Mean Distance: ", unique( round( d.plot$avg.miles, 2) ), " miles" )
org.title <- paste0( "Org Name: ", unique( d.plot$ORGNAME ) )

# plot
ggplot( d.plot ) +
  geom_sf( lwd = 0.3, lineend = "round" ) +
  theme_classic()+
  ggtitle( org.title ) +
  annotate( "text", -Inf, Inf, label = avg.label, hjust = -0.08, vjust = 1 ) +
  annotate( "text", -83.65505, 40.66292, label = "8.06 miles", size = 2.5, vjust = 2 ) +
  annotate( "text", -83.51111, 40.53398, label = "18.6 miles", size = 2.5, vjust = 2 ) +
  annotate( "text", -83.65196, 40.77841, label = "0.26 miles", size = 2.5, vjust = 2 ) +
  coord_sf( xlim = c( current.bbox[1], current.bbox[3] ), ylim = c( current.bbox[2], current.bbox[4] ) ) # boundaries

plot(st_geometry(d), lwd = 1, lineend = "round")

