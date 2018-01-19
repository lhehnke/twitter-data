##------------------------------------------------------------------------------------------##
##                          TWITTER USER GEOGRAPHICAL ANALYSIS                              ##
##------------------------------------------------------------------------------------------##


## R version 3.3.1 (2016-06-21)

## Adapted from: http://lucaspuente.github.io/notes/2016/04/05/Mapping-Twitter-Followers


#-------#
# Setup #
#-------#

# Install and load pacman if not already installed
if (!require("pacman")) install.packages("pacman")
library(pacman)

# Load packages
p_load(data.table, dplyr, geosphere, ggmap, ggplot2, leaflet, maptools, maps, twitteR)


#---------------------#
# Scrape twitter data #
#---------------------#

# Consumer_key, consumer_secret, access_token & access_secret 
consumer_key <- "[INSERT OWN KEY HERE]"
consumer_secret <- "[INSERT OWN KEY HERE]"
access_token <- "[INSERT OWN KEY HERE]"
access_secret <- "[INSERT OWN KEY HERE]"

# Twitter authentication
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
1

# Scrape hashtag
searchResults <- searchTwitter("#MeToo", n = 5000)  
tweetFrame <- twListToDF(searchResults) 

# Scrape user info
userInfo <- lookupUsers(tweetFrame$screenName)  
userFrame <- twListToDF(userInfo)

# Remove anything other than English letters or space in location and subset 
userFrame$location <- gsub("[^[:alpha:][:space:]]*", "", userFrame$location) 
userFrame <- subset(userFrame, location != "")


#------------------------#
# Geocode user locations #
#------------------------#

# Google Maps API
api_key <- "[INSERT OWN KEY HERE]"

# Install key package helpers and modified version of geocode function
## Source: hhttp://lucaspuente.github.io/notes/2016/04/05/Mapping-Twitter-Followers
source("https://raw.githubusercontent.com/LucasPuente/geocoding/master/geocode_helpers.R")
source("https://raw.githubusercontent.com/LucasPuente/geocoding/master/modified_geocode.R")

# Specify query
geocode_apply <- function(x){
  geocode(x, source = "google", output = "all", api_key = api_key)
}

# Get geocoded locations
geocode_results <- sapply(userFrame$location, geocode_apply, simplify = F)

# Clean geocoded data
condition_a <- sapply(geocode_results, function(x) x["status"]=="OK")
geocode_results <- geocode_results[condition_a]

condition_b <- lapply(geocode_results, lapply, length)
condition_b2 <- sapply(condition_b, function(x) x["results"]=="1")
geocode_results <- geocode_results[condition_b2]

# Transform cleaned results to df and extract relevant columns
source("https://raw.githubusercontent.com/LucasPuente/geocoding/master/cleaning_geocoded_results.R")
results_b <- lapply(geocode_results, as.data.frame)
results_c <- lapply(results_b,function(x) subset(x, select = c("results.formatted_address", "results.geometry.location")))

results_d <- lapply(results_c,function(x) data.frame(Location = x[1, "results.formatted_address"],
                                                     lat = x[1, "results.geometry.location"],
                                                     lng = x[2, "results.geometry.location"]))

# Make data.table from lists
results_e <- rbindlist(results_d)

# Add original user-specified location strings that were successfully geocoded
results_final <- results_e[, location:=names(results_d)]

# Subset user data for keeping geocoded users only
loc <- results_final$location
userFrame_final <- subset(userFrame, location %in% loc)

# Check results 
userFrame_final$location == results_final$location

# Add geocoded locations to user data
userFrame_final <- cbind(userFrame_final, results_final[, 1:3])

# Add original tweet to user data
## Note: "Duplicates" result from multiple tweets by single user
tweet_userFrame_final <- left_join(userFrame_final, tweetFrame, by = "screenName")


#------------------------#
# Visualization: leaflet #
#------------------------#

# Set colours
pal <- colorNumeric(palette = c("#ffe6e6", "#ff8080", "#ff0000"), domain = c(0, 100))

# Plot user locations with color of locations reflecting number of followers
leaflet(userFrame_final, width = "100%") %>% 
  addProviderTiles("CartoDB.DarkMatter") %>% 
  addCircleMarkers(~lng,
                   ~lat,
                   color = ~pal(followersCount),
                   radius = 1.5)


#------------------------#
# Visualization: ggplot2 #
#------------------------#

# Download, unzip and import shapefiles from Natural Earth webpage
temp <- tempfile()
download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip", temp, mode = "w")
unzip(temp)
world_shp <- readShapeSpatial("ne_50m_admin_0_countries.shp", proj4string = CRS("+proj=longlat +ellps=WGS84"))
unlink(temp)

# Remove Antarctica
world_shp <- subset(world_shp, NAME != "Antarctica")

# Plot user locations
ggplot() + 
  geom_polygon(data = world_shp, aes(x = long, y = lat, group = group)) +
  geom_point(data = userFrame_final, aes(x = lng, y = lat), size = 1.2, color = "red") +
  labs(x = "", y = "") +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
  coord_equal()