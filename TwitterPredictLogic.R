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
twitterData = twitterData[order(twitterData$date), twitterData]

twitterData$weekday = weekdays(twitterData$date)
twitterData$hour = hour(strptime( str_split_fixed(twitterData$time, " ", 5)[,2], format = "%H:%M"))
twitterData$minute = minute(strptime( str_split_fixed(twitterData$time, " ", 5)[,2], format = "%H:%M"))

# Need some Vlad magic here
list_hashtags = list()

for (idx in 1:nrow(twitterData)) {
  list_hashtags = append(list_hashtags, str_extract_all(twitterData[idx, "Tweet.text"], '#\\w+', simplify = T))
}

list_hashtags = tolower(unique(unlist(list_hashtags)))


