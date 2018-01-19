# twitter-data
Scripts for scraping, wrangling, geocoding, analyzing, and mapping Twitter data

## Description: twitter_analysis_trump
The script covers

* scraping Twitter data from user timeline
* text cleaning of tweets using *tm* (e.g., removing URLs, RTs, handles and emojis from text) 
* mining them by  
    * extracting and plotting word frequencies
    * visualizing word clouds
    * finding word associations
    * visualizing popularity statistics (number of favourites and retweets)
    * extracting and visualizing publication dates 
    * conducting sentiment analyses
    
using the example of Donald Trump's tweets. 

## Description: twitter_user_location
The script covers

* scraping Twitter data by hashtag
* scraping corresponding user infos of tweet authors
* geocoding user locations using Google Maps API
* cleaning geocoded user locations and merging them with corresponding tweets 
* mapping user locations with *leaflet* and *ggplot2*
    
using the example of #MeToo. 
