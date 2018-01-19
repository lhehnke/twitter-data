##------------------------------------------------------------------------------------------##
##                   TEXT AND SENTIMENT ANALYSIS OF DONALD TRUMPS TWEETS                    ##
##------------------------------------------------------------------------------------------##


## R version 3.3.1 (2016-06-21)


#-------#
# Setup #
#-------#

# Install and load pacman if not already installed
if (!require("pacman")) install.packages("pacman")
library(pacman)

# Load packages
p_load(dplyr, ggplot2, lubridate, reshape2, syuzhet, tm, twitteR, wordcloud)


#---------------#
# Twitter setup #
#---------------#

# Change to own consumer_key, consumer_secret, access_token & access_secret 
consumer_key <- "[INSERT OWN KEY HERE]"
consumer_secret <- "[INSERT OWN KEY HERE]"
access_token <- "[INSERT OWN KEY HERE]"
access_secret <- "[INSERT OWN KEY HERE]"

# Twitter authentication
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
1


#---------------#
# Scrape tweets #
#---------------#

# Extract list of tweets from user timeline and turn list into data.frame
user_tweets <- userTimeline("realDonaldTrump", n = 3200) # includeRts = TRUE for retweets
user_tweets_df <- twListToDF(user_tweets)


#---------------#
# Text cleaning #
#---------------#

# Build a corpus
tweets_corpus <- Corpus(VectorSource(user_tweets_df$text))

# Remove RTs and handles
removeTwitter <- function(x) gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", x) 
tweets_corpus <- tm_map(tweets_corpus, content_transformer(removeTwitter))

# Remove URLs
removeURL <- function(x) gsub("http[[:alnum:][:punct:]]*", "", x) 
tweets_corpus <- tm_map(tweets_corpus, content_transformer(removeURL))

# Remove emojis from text
#removeEmoji <- function(x) iconv(x, to = 'UTF-8-MAC', sub = 'byte')
#tweets_corpus <- tm_map(tweets_corpus, content_transformer(removeEmoji))

# Remove anything other than English letters or space 
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x) 
tweets_corpus <- tm_map(tweets_corpus, content_transformer(removeNumPunct))

# Convert to lower case
tweets_corpus <- tm_map(tweets_corpus, tolower)

# Remove stopwords
tweets_corpus <- tm_map(tweets_corpus, removeWords, stopwords("english"))

# Remove specified words
tweets_corpus <- tm_map(tweets_corpus, removeWords, c("emoji", "htt*", "tweet", "amp", "i", "etc", "also", "the", "this", "these", "very", "rt"))

# Strip whitespace
tweets_corpus <- tm_map(tweets_corpus, stripWhitespace)

# Stem words, i.e., convert words to their stem (e.g., learning -> learn)
#library(SnowballC)
#tweets_corpus <- tm_map(tweets_corpus, stemDocument)

# Remove characters with only 1-2 letters
#removeSingle <- function(x) gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ", x) 
#tweets_corpus <- tm_map(tweets_corpus, content_transformer(removeSingle))

# Create term document matrix from corpus
tdm <- TermDocumentMatrix(tweets_corpus)


#------------------#
# Word frequencies #
#------------------#

# Find frequent terms
findFreqTerms(tdm, lowfreq = 20)

# Calculate word frequencies and sort by frequency
mat <- as.matrix(tdm)
words_freq <- sort(rowSums(mat), decreasing = T) 

# Subset and convert to data.frame
words_freq_subset <- subset(words_freq, words_freq >= 10) # min. frequency = 10
words_df <- data.frame(term = names(words_freq_subset), freq = words_freq_subset)

# Plot most common words
ggplot(words_df[1:20, ], aes(x = term, y = freq)) + geom_bar(stat = "identity") +
  xlab("Words") + ylab("Count") + 
  ggtitle("Word frequencies using Twitter data", subtitle = "User: realDonaldTrump") + 
  ylim(0, 100) + coord_flip()


#-----------#
# Wordcloud #
#-----------#

# Set color
palette <- brewer.pal(9, "Reds")[-(1:4)] 

# Plot word cloud
wordcloud(words_df$term, words_df$freq, min.freq = 10, colors = palette, 
          max.words = 80, random.order = F)


#-------------------#
# Word associations #
#-------------------#

# Words associated with term "great" (r >= 0.2)
findAssocs(tdm, "great", 0.2)


#------------------#
# Popularity stats #
#------------------#

# Plot number of favorites for user
ggplot(user_tweets_df) + geom_histogram(aes(x = favoriteCount), bins = 25) + 
  ggtitle("Number of favorites for tweets", subtitle = "User: realDonaldTrump") +
  xlab("Favorites") + ylab("Count") + ylim(0, 150) 

# Plot number of retweets for user
ggplot(user_tweets_df) + geom_histogram(aes(x = retweetCount), bins = 25) + 
  ggtitle("Number of retweets for tweets", subtitle = "User: realDonaldTrump") +
  xlab("Retweets") + ylab("Count") + ylim(0, 200) 


#------------------#
# Publication date #
#------------------#

# Convert timestamps of tweets to same time zone
user_tweets_df$created <- ymd_hms(user_tweets_df$created)
user_tweets_df$created <- with_tz(user_tweets_df$created, "America/New_York")

# Tweets by month
ggplot(user_tweets_df, aes(x = month(created, label = TRUE))) +
  geom_bar() +
  ggtitle("Number of tweets by month", subtitle = "User: realDonaldTrump") +
  xlab("Month") + ylab("Count") +
  theme(legend.position = "none") + ylim(0, 150)

# Tweets by weekday
ggplot(user_tweets_df, aes(x = wday(created, label = TRUE))) + 
  geom_bar() + xlab("Day") + ylab("Count") + 
  ggtitle("Number of tweets by weekday", subtitle = "User: realDonaldTrump") +
  theme(legend.position = "none") + ylim(0, 100)

# Tweets by hour
ggplot(user_tweets_df, aes(x = hour(created))) + 
  geom_bar() + xlab("Hour") + ylab("Count") + 
  ggtitle("Number of tweets by hour", subtitle = "User: realDonaldTrump") +
  theme(legend.position = "none") + ylim(0, 50)


#--------------------#
# Sentiment analysis #
#--------------------#

## Adapted from: https://juliasilge.com/blog/joy-to-the-world/ 

# Convert corpus to character vector and get sentiments (based on http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm)
sentiment <- get_nrc_sentiment(as.character(tweets_corpus$content))

# Add sentiments to user_tweets_df
user_tweets_df <- cbind(user_tweets_df, sentiment)

# Calculate sentiment frequencies and subset
sentiment_tot <- data.frame(colSums(user_tweets_df[, c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust", "negative", "positive")]))

# Rename count variable and create variable with sentiment names
names(sentiment_tot) <- "count"
sentiment_tot <- cbind("sentiment" = rownames(sentiment_tot), sentiment_tot)

# Plot total sentiment scores
ggplot(data = sentiment_tot, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  xlab("Sentiment") + ylab("Count") + 
  ggtitle("Sentiment scores for tweets", subtitle = "User: realDonaldTrump") +
  theme(legend.position = "none") + ylim(0, 500)

# Calculate sentiments during week
user_tweets_df$weekday <- wday(user_tweets_df$created, label = TRUE)
weeklysentiment <- user_tweets_df %>% group_by(weekday) %>% 
  summarise(anger = mean(anger), 
            anticipation = mean(anticipation), 
            disgust = mean(disgust), 
            fear = mean(fear), 
            joy = mean(joy), 
            sadness = mean(sadness), 
            surprise = mean(surprise), 
            trust = mean(trust)) %>% melt
names(weeklysentiment) <- c("weekday", "sentiment", "meanvalue")

# Plot weekly sentiments
ggplot(data = weeklysentiment, aes(x = weekday, y = meanvalue, group = sentiment)) +
  geom_line(size = 2.5, alpha = 0.7, aes(color = sentiment)) +
  geom_point(size = 0.6) +
  ylim(0, 1.0) +
  ylab("Average sentiment score") + xlab("Weekday") +
  ggtitle("Sentiments during the week", subtitle = "User: realDonaldTrump") +
  theme(legend.title = element_blank(), axis.title.x = element_blank()) 