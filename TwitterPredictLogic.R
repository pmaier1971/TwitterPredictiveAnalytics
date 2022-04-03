setwd("~/Documents/R/TwitterPredictiveAnalytics/")

pkgTest <- function(x){
  suppressMessages(
    suppressWarnings(
      if (!require(x,character.only = TRUE)) {
        install.packages(x,dep=TRUE)
        if(!require(x,character.only = TRUE)) stop("Package not found")
      }
    )
  )
}

pkgTest("fredr")
pkgTest("RColorBrewer")
pkgTest("reshape")
pkgTest("quantmod")
pkgTest("zoo")
pkgTest("dplyr")
pkgTest("forecast")
pkgTest("lubridate")
pkgTest("httr")
pkgTest("jsonlite")
pkgTest("rtweet")
pkgTest("httr")
pkgTest("tis")
pkgTest("tm")
pkgTest("gmailr")
pkgTest("COVID19")
pkgTest("stringr")

# Setup / Load data

chart.col   = brewer.pal(6, "Paired")

# Fix / Enrich Data

twitterData = read.csv("tweet_activity_metrics_EconomicsShiny_20220201_20220301_en.csv")

twitterData$date = as.Date(strtrim(twitterData$time, 10))
twitterData = twitterData[order(twitterData$date), ]

twitterData$weekday = weekdays(twitterData$date)
twitterData$hour = hour(strptime( str_split_fixed(twitterData$time, " ", 5)[,2], format = "%H:%M"))
twitterData$minute = minute(strptime( str_split_fixed(twitterData$time, " ", 5)[,2], format = "%H:%M"))

# Need some Vlad magic here
list_hashtags = list()

for (idx in 1:nrow(twitterData)) {
  list_hashtags = append(list_hashtags, str_extract_all(twitterData[idx, "Tweet.text"], '#\\w+', simplify = T))
}

list_hashtags = unique(tolower(unlist(list_hashtags)))

table_hashtag_popularity = data.frame(Hashtag = list_hashtags, Popularity = 0, Impressions = 0)

for (idx in 1:nrow(table_hashtag_popularity)) {
  for (idx2 in 1:nrow(twitterData)) {
    if (table_hashtag_popularity[idx, "Hashtag"] %in% tolower( str_extract_all(twitterData[idx2, "Tweet.text"], '#\\w+', simplify = T)) ){
      table_hashtag_popularity[idx, "Popularity"] = table_hashtag_popularity[idx, "Popularity"] +1
      table_hashtag_popularity[idx, "Impressions"] = table_hashtag_popularity[idx, "Impressions"] +
        twitterData[idx2, "impressions"]
    }
  }
}

table_hashtag_popularity = table_hashtag_popularity[order(table_hashtag_popularity$Popularity,decreasing = TRUE),]

hashtag_plot = barplot(table_hashtag_popularity[5:1, 2], horiz = TRUE, col = chart.col[1:5] )
axis(2, at = hashtag_plot, labels = table_hashtag_popularity[5:1,1], las = 2)


twitterData$NoHashTag = 0
twitterData$ContainsHashTagRstats = 0
twitterData$ContainsMultipleHashTags = 0

for (idx in 1:nrow(twitterData)) {
  if (is.logical( tolower( str_extract_all(twitterData[idx2, "Tweet.text"], '#\\w+', simplify = T)) ) ) {
    # Tweet has no hashtags
    
    twitterData[idx, "NoHashTag"] = 1
    
  } else {
    if ("#rstats" %in% tolower( str_extract_all(twitterData[idx2, "Tweet.text"], '#\\w+', simplify = T)) ) {
      twitterData[idx, "ContainsHashTagRstats"] = 1 }  
    if (length (tolower( str_extract_all(twitterData[idx2, "Tweet.text"], '#\\w+', simplify = T)) )>1 ) { 
      twitterData[idx, "ContainsMultipleHashTags"] = 1
    }
  }  
}


  