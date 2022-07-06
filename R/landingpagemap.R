np <- readRDS( "NONPROFITS-2014-2021v6.rds" )


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


# Convert lat/long to a sf
np.sf <- np.b %>%
  filter( is.na( lat ) ==F ) %>% # st_as_sf does not allow missing values in coordinate columns; n=167 missing
  st_as_sf(coords = c("lon", "lat"), crs="NAD83")


# no. of NPOs by county FIPS
n.ct <- as.data.frame( np.sf %>%
                         group_by( fips_ct ) %>%
                         mutate( n = n(),
                                 fips_ct = as.numeric( fips_ct ) ) %>% # count of NPO's within county FIPS
                         distinct( fips_ct, n ) )


# county population 
library(tidycensus)
census_api_key("eeaa303439c03596e636a76abb061c86cb315032", install =TRUE,
               overwrite=TRUE)

pop.ct <- as.data.frame( get_acs(geography = "county", 
                                 variables = c( pop = "B07401_001" ), 
                                 year = 2020)%>%
                           select(fips_ct=GEOID, estimate) )


## merge
n.pop.merge <- pop.ct%>%
  mutate(fips_ct = as.numeric( fips_ct ) )%>%
  left_join(n.ct, .) %>%
  mutate( dens = n / estimate) %>%
  filter ( is.na( dens )==F)%>%
  mutate( dens.q = factor(quant.cut( var = 'dens', x = 5 ,df=n.pop.merge ) ) )


ct <- st_transform( urbnmapr::get_urbn_map( map = "counties", sf = TRUE ), crs='NAD83' ) %>%
  mutate( county_fips = as.numeric( county_fips)) %>%
            left_join( ., n.pop.merge, by=c('county_fips'='fips_ct'))




ggplot() + geom_sf(ct,
                   mapping = aes(fill = dens.q),
                   color = NA, size = 0.5)+
  scale_fill_manual( 'Quintile', values = c("#DCE319FF", "#55C667FF", 
                                            "#238A8DFF", "#39568CFF","#440154FF")) +
  theme_minimal( ) 

