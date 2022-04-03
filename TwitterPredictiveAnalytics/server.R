#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


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

options(shiny.maxRequestSize=30*1024^2)

pkgTest("shiny")
pkgTest("readxl")
pkgTest("dplyr")
pkgTest("DT")
pkgTest("stringr")
pkgTest("lubridate")

options(scipen=999)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  observe({
    file1 = input$fileTwitterData
    
    if (is.null(file1)) {
      return(NULL)
    }
    
    
    
    twitterData = reactive({
      
      twitterData = read.csv(file1$datapath)
      
      twitterData$date =  as.Date(strtrim(twitterData$time, 10))
      twitterData = twitterData[order(twitterData$date), ]
      twitterData$weekday = weekdays(twitterData$date)
      
      twitterData$hour = hour(strptime( str_split_fixed(twitterData$time, " ", 5)[,2], format = "%H:%M"))
      twitterData$minute = minute(strptime( str_split_fixed(twitterData$time, " ", 5)[,2], format = "%H:%M"))
      
      return(twitterData)
    })
    
    
    # Controls
    
    output$selector_hashags = renderUI({
      # Need some Vlad magic here
      
      twitterData = twitterData()
      
      list_hashtags = list()
      
      for (idx in 1:nrow(twitterData)) {
        list_hashtags = append(list_hashtags, str_extract_all(twitterData[idx, "Tweet.text"], '#\\w+', simplify = T))
      }
      
      list_hashtags = sort( tolower(unique(unlist(list_hashtags))) )
      
      selectInput("uiSelection_hashtag", label = "Hashtags", list_hashtags)
      
    })
    
    # Analysis
    output$dataCheck = renderDataTable({
      results = twitterData()
      results = results[,3:6]
      return(results)
    })
      
    output$ImpressionsByDay = renderPlot({
     
      data = twitterData()
      dataElements = c("impressions", "engagements", "retweets", "replies")
      
      tweetPerDay = aggregate(data[,dataElements], by=list(Day = data$weekday), sum)
      tweetPerDay$Day = factor(tweetPerDay$Day, levels= c("Monday", 
                                                          "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
      tweetPerDay = tweetPerDay[order(tweetPerDay$Day), ]
      
      
      plot(tweetPerDay$impressions, type = "l", main = paste0("On Average You Earned ", 
                                                              round( mean(tweetPerDay$impressions), 0),
                                                              " Impressions / Day"), 
           xaxt = "n", xlab="", ylab="",
           col = chart.col[1], lwd = 2)
      axis(1, at=1:7, label = tweetPerDay$Day,
           col.axis="black", cex.axis=0.9)
      
    })
    
    output$ImpressionsByHour = renderPlot({
      
      data = twitterData()
      dataElements = c("impressions", "engagements", "retweets", "replies")
      
      tweetPerHour = aggregate(data[,dataElements], by=list(Hour = data$hour), sum)
      
      tweetPerHour = left_join( data.frame(Hour = 1:24), tweetPerHour)
      tweetPerHour[is.na(tweetPerHour)] = 0
      
      plot(tweetPerHour$impressions, type = "l", main = "Impressions by Hour", 
           xaxt = "n", 
           xlab="", ylab="",
           col = chart.col[1], lwd = 2)
      axis(1, at=1:nrow(tweetPerHour), label = tweetPerHour$Hour,
            col.axis="black", cex.axis=0.9)
      
    })
    
    output$ImpressionsByMinute = renderPlot({
      
      data = twitterData()
      dataElements = c("impressions", "engagements", "retweets", "replies")
      
      tweetPerMinute = aggregate(data[,dataElements], by=list(Minute = data$minute), sum)
      tweetPerMinute = left_join( data.frame(Minute = 1:60), tweetPerMinute)
      tweetPerMinute[is.na(tweetPerMinute)] = 0
      
      plot(tweetPerMinute$impressions, type = "l", main = "Impressions by Minute", 
           xaxt = "n", 
           xlab="", ylab="",
           col = chart.col[1], lwd = 2)
      axis(1, at=1:nrow(tweetPerMinute), label = tweetPerMinute$Hour,
           col.axis="black", cex.axis=0.9)
      
    })
    
    output$plotHashtags = renderPlot({
      
      twitterData = twitterData()
      emptyFrame = data.frame(Date = seq(as.Date(twitterData()[1, "date"]), 
                                         as.Date(twitterData()[nrow(twitterData()), "date"]),'days'))
      twitterData$include = FALSE
      
      for (idx in 1:nrow(twitterData)) {
        list_hashtags = str_extract_all(twitterData[idx, "Tweet.text"], '#\\w+', simplify = T)
        if (input$uiSelection_hashtag %in% tolower( list_hashtags )) {
          twitterData[idx, "include"] = TRUE
        }
      }
      
      dataElements = c("impressions", "engagements", "retweets", "replies")
      twitterData = twitterData[twitterData$include == TRUE, ]
      twitterData = aggregate(twitterData[,dataElements], by=list(Date = twitterData$date), sum)
      
      
      
      twitterData = left_join( emptyFrame, twitterData)
      twitterData[is.na(twitterData)] = 0
      
      plot(twitterData$impressions, type = "l", main = paste0("Impressions of Tweets containing ",  
                                                              input$uiSelection_hashtag ), 
           xaxt = "n", xlab="", ylab="",
           col = chart.col[1], lwd = 2)
      axis(1, at=1:nrow(twitterData), label = twitterData$Date,
           col.axis="black", cex.axis=0.9)
      
    })
    
  })
})
