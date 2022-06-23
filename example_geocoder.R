library(tidygeo)

# create a dataframe with addresses
some_addresses <- tibble::tribble(
  ~name,                  ~addr,
  "White House",          "1600 Pennsylvania Ave NW, Washington, DC",
  "Transamerica Pyramid", "600 Montgomery St, San Francisco, CA 94111",     
  "Willis Tower",         "233 S Wacker Dr, Chicago, IL 60606"                                  
)

# geocode the addresses
lat_longs <- some_addresses %>%
  geocode(addr, method = 'osm', lat = latitude , long = longitude)
#> Passing 3 addresses to the Nominatim single address geocoder
#> Query completed in: 3 seconds
#> 

gc<-tibble(read.csv("/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Geospatial Dashboard/R code for now/Data-Wrangled/demo/TestAddresses.csv"))%>%
  geocode(., street = Address, city=City,state=State,postalcode=Zip,method='census',full_results=T)



for (i in 1:length(dir(wd2))-1){ 
  
  
  tibble(read.csv("/Volumes/My Passport for Mac/Urban Institute/Summer Projects/Geospatial Dashboard/R code for now/Data-Wrangled/demo/TestAddresses.csv"))%>%
    geocode(., street = Address, city=City,state=State,postalcode=Zip,method='census',full_results=T)
}
