library(tigris)
library(sf)
library(tidyverse)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

orwa <- rbind_tigris(
  tracts("OR", cb = TRUE), 
  tracts("WA", cb = TRUE)
)
ggplot(orwa) + geom_sf()

cb <- core_based_statistical_areas(cb = TRUE)
sea <- filter(cb, grepl("Seattle", NAME)) # match 
ggplot(pdx) + geom_sf()

p1 <- orwa[sea,]
ggplot() + 
  geom_sf(data = p1) + 
  geom_sf(data = sea, fill = NA, color = "red")


w1 <- st_within(orwa, pdx)


w2 <- map_lgl(w1, function(x) {
  if (length(x) == 1) {
    return(TRUE)
  } else {
    return(FALSE)
  }
})

p2 <- orwa[w2,]
ggplot() + 
  geom_sf(data = p2) + 
  geom_sf(data = sea, fill = NA, color = "red")+
  theme_minimal()

head(p2)
names(p2)
np.sf$state_fips
np.sf %>%
  dplyr::filter( state_fips == 53 & county_fip) %>%
  select( State )


# subset 1023 data sf files based on those having geometries within the MSA
w1 <- st_within(orwa, pdx)

w2 <- map_lgl( w1, function( x ) {
  if ( length( x ) == 1) {
    return( TRUE )
  } else {
    return( FALSE )
  }
})

w3<-st_within( orwa, np.sf )
np.sf




library(tigris)
library(sf)
library(tidyverse)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

wa <- tracts("WA", cb = TRUE) # Washington shapefiles

cb <- core_based_statistical_areas(cb = TRUE) # fetches cartographic boundary files for MSA
sea <- filter(cb, grepl("Seattle", NAME)) # match "Seattle" and return boundary files

p1 <- wa[sea,] # subset tracts from within (or touching) MSA boundary

ggplot() + 
  geom_sf(data = p1)
  geom_sf(data = sea, fill = NA, color = "red") + # red lines show cartographic boundaries of MSA
  theme_minimal() 



s <- st_within( wa, sea )

these <- map_lgl( s, function( x ) {
  if (length( x ) == 1) {
    return(TRUE)
  } else {
    return(FALSE)
  }
})

d <- wa[ these, ]
ggplot() + 
  geom_sf(data = d)  +
  geom_sf(data = sea, fill = NA, color = "red") +
  theme_minimal()



# subset 1023 data sf files based on those having geometries within the MSA
s2 <- st_within(np.sf, sea)

these.2 <- map_lgl( s2, function( x ) {
  if ( length( x ) == 1) {
    return( TRUE )
  } else {
    return( FALSE )
  }
})


np.sf.2 <- np.sf[these.2,]

ggplot() + 
  geom_sf(data = d) +
  geom_sf(data = np.sf.2) + 
  geom_sf(data = sea, fill = NA, color = "red")



length(unique(d$NAMELSAD))

#####try

lf <- "/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Geospatial Dashboard/Large-Files-Bank"

setwd( lf )
np <- readRDS( "NONPROFITS-2014-2021v6.rds" )

# Convert lat/long to a sf
np.sf <- np %>%
  filter( is.na( lat ) ==F ) %>% # st_as_sf does not allow missing values in coordinate columns; n=167 missing
  st_as_sf(coords = c("lon", "lat"), crs="NAD83")

# subset
s2 <- st_within(np.sf, sea)

these.2 <- map_lgl( s2, function( x ) {
  if ( length( x ) == 1) {
    return( TRUE )
  } else {
    return( FALSE )
  }
})


np.sf.2 <- np.sf[these.2]

ggplot() + 
  geom_sf(data = np.sf.2) + 
  geom_sf(data = sea, fill = NA, color = "red")



View(np.sf.2)

s2 <- st_within(np.sf, d[500,])

s2 <- st_within(np.sf, sea)
length(unique(sea$GEOID))

ct <- urbnmapr::get_urbn_map( map = "ccdf", sf = TRUE )

# transform coordinate system
ct <- st_transform( ct, crs = "NAD83")

sea.c <- st_within(sea, ct)
nrow(sea.c)


##import shapefile from census

w<-read.table('/Users/Chris/Downloads/tl_2021_53_tract/tl_2021_53_tract.shp')


library(sf)

wascen <- readShapeSpatial('/Users/Chris/Downloads/tl_2021_53_tract/tl_2021_53_tract.shp')

wascen <- system.file("/Users/Chris/Downloads/tl_2021_53_tract/tl_2021_53_tract.shp", package="sf")

st_layers(wascen, do_count = TRUE)


proj4string(wascen) <- "+proj=longlat +datum=NAD83" # specify projection
wascen<-sf::st_read('/Users/Chris/Downloads/tl_2021_53_tract/tl_2021_53_tract.shp',
            stringsAsFactors=F, options = "ENCODING=UTF8")
head(sf)

sea.c <- st_within( sea, wascen )
head(wascen)
names(wascen)


names(d)






##tract org density

# compound fips code
np.sf.2$fips<- paste0( np.sf.2$STATEFIPS, np.sf.2$COUNTYFIPS, np.sf.2$TRACTFIPS )

dat.geo <- st_join(d, np.sf.2, join = st_nearest_feature, left = T)
names(np.sf.3)

np.sf.3$geometry

# count rows by FIPS, create density measure
c.dens <- dat.geo %>%
  group_by(fips) %>%
  mutate( pop = ceiling( p.density * (ALAND*1e-06) ), # convert sq. meters to sq. km
          n = n(), 
          o.density = ( n / pop ) * 1000)%>%
  distinct( fips, pop , o.density , geometry )


# bin for plotting
c.dens$o.den.bin = factor(quant.cut( var = 'o.density', x = 5 ,df=c.dens ) )


## plotting

ggplot() + 
  geom_sf( data = d ) +
  geom_sf( data = sea, fill = NA, color = "red" )+
  geom_sf(c.dens,
          mapping = aes(fill = o.den.bin),
          color = NA, size = 0.5) +
scale_fill_manual('Quintile', values = c("#DCE319FF", "#55C667FF", "#238A8DFF",
                             "#39568CFF","#440154FF")) +
  theme_minimal() +
  theme( text = element_text( family = "Avenir" ) )
  
  sum( is.na( as.data.frame(c.dens$o.den.bin)))

# landing page plot of USA

# aggregate up to level of counties
  
cb <- tracts(cb = TRUE)

cb$fips_ct <- paste0( cb$STATEFP, cb$COUNTYFP )
cb$fips_tract <- paste0( cb$STATEFP, cb$COUNTYFP )

  
# county map
ct.sf <- get_urbn_map( map = "counties", sf = TRUE )

ct.sf <- st_transform( ct.sf, crs="NAD83" ) 

# compound fips code to county level

# no. of NPOs by county FIPS
n.ct <- as.data.frame( np.sf %>%
  group_by( fips_ct ) %>%
  mutate( n = n() ) %>% # count of NPO's within county FIPS
  distinct( fips_ct, n ) )

length(unique(p.dens.ct$fips_ct))
View(p.dens.ct[which(duplicated(p.dens.ct$fips_ct)),]%>%
       arrange(fips_ct))
sum(is.na(n.ct$fips_ct))
View(n.ct)

# cumulative county land area by county FIPS
cb.ct <- cb%>%
  mutate( fips_ct = as.numeric( fips_ct ) ) %>%
  distinct( fips_ct, ALAND ) %>%
  group_by( fips_ct )%>%
  mutate( ALAND.ct = sum(ALAND )) # cumulative county land area


# county population 
library(tidycensus)
census_api_key("eeaa303439c03596e636a76abb061c86cb315032", install =TRUE,
               overwrite=TRUE)

pop.ct <- as.data.frame( get_acs(geography = "county", 
                       variables = c( pop = "B07401_001" ), 
                       year = 2020)%>%
  select(fips_ct=GEOID, estimate) )


v17 <- load_variables(2020, "sf1", cache = TRUE)

View(v17[which(v17$geography=='county'),])
  

## merge
n.pop.merge <- pop.ct%>%
  mutate(fips_ct = as.numeric( fips_ct ) )%>%
  left_join(n.ct, .)


pop.ct[which(pop.ct$fips_ct==4211),]


# still isue with fips codes in n.ct dataset

library(fipio)
fips.sf<-coords_to_fips(x=trim.sf, coords=c(2,1))
names(np)
trim.sf<-np[1:5, c('lat','lon')]
sum(is.na(cb.ct))
  
library(eicompare)

latlong_sf <- trim.sf %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = "NAD83")

census_block <- list()

num_catch <- rep(NA, nrow(nom_geo))

for (i in 1:nrow(trim.sf)) {

  census_block[[i]] <- coords_to_fips(x=trim.sf$lon[i], y=trim.sf$lat[i])
}

np<-np%>%
  mutate(COUNTYFIPS=ifelse(str_count( COUNTYFIPS, '\\d' )==1, paste0('00', COUNTYFIPS),
                           ifelse(str_count( COUNTYFIPS, '\\d' )==2,paste0('0', COUNTYFIPS),
                                  ifelse( str_count( COUNTYFIPS, '\\d' )==3, paste0( COUNTYFIPS), NA ) ) ),
         fips_ct = ifelse( is.na(STATEFIPS)==F & is.na(COUNTYFIPS)==F, paste0(STATEFIPS, COUNTYFIPS),NA ) )


 # obtain FIPS for those still missing FIPS code but having coordinates present                         
trim.sf <- np[which(is.na(np$COUNTYFIPS) & is.na( np$lat) ==F & is.na( np$lon )==F), c('lat','lon','key')]

s<-list()
for (i in 1:475) {
  fips_c <- coords_to_fips(x=trim.sf$lon[i], y = trim.sf$lat[i])
  s[[i]] <- data.frame( f = ifelse( length(fips_c)==0, NA, fips_c),
                        key=trim.sf$key[i])
  
  print(paste0("iteration # " ,i, " complete") )
}   

length(keys)
s <- do.call('rbind', s)    

np.b <- left_join( np, s, by='key') %>%
  mutate( fips_ct = ifelse( is.na(fips_ct), f, fips_ct ) ) %>%
  select( -f )

sum(is.na(np$fips_ct)) # previously had 642 missing county FIPS
sum(is.na(np.b$fips_ct)) # now reduced to 278

                    
View(np[which(is.na(np$COUNTYFIPS) & is.na( np$lat) ==F & is.na( np$lon )==F),])

View(np[1:5,])
  View(cb.ct)
sum(is.na(np.sf$COUNTYFIPS))# 475 missing county and state fips...needs more investigating
sum(is.na(np.sf$STATEFIPS))

head(p.dens.ct$fips_ct)

ct.geo <- st_join( ct.sf, np.sf.2, join = st_nearest_feature, left = T) 

np.sf.3$geometry

# count rows by FIPS, create density measure
c.dens <- dat.geo %>%
  group_by(fips) %>%
  mutate( pop = ceiling( p.density * (ALAND*1e-06) ), # convert sq. meters to sq. km
          n = n(), 
          o.density = ( n / pop ) * 1000) %>%
  distinct( fips, pop , o.density , geometry )

head ()

