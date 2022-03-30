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
      
      twitterData$weekday = weekdays(twitterData$date)
      # twitterData$hour = hour(tweet.DB$created_at)
      # twitterData$minute = minute(tweet.DB$created_at)
      # twitterData$URL = is.null(tweet.DB$media_url)
      # twitterData$isCOVID = "COVID19" %in% tweet.DB$hashtags[[1]]
      
      return(twitterData)
    })
    
    output$dataCheck = renderDataTable({
      results = twitterData()
      results = results[1:10,3:6]
      return(results)
    })
      
    output$BasicStats = renderPlot({
     
      data = twitterData()
      dataElements = c("impressions", "engagements", "retweets", "replies")
      
      tweetPerDay = aggregate(data[,dataElements], by=list(Day = data$weekday), sum)
      
      tweetPerDay$Day = factor(tweetPerDay$Day, levels= c("Monday", 
                                                          "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
      tweetPerDay = tweetPerDay[order(tweetPerDay$Day), ]
      
      plot(tweetPerDay$impressions, type = "l", main = "Impressions by Day", 
           xaxt = "n", xlab="", ylab="",
           col = chart.col[1], lwd = 2)
      axis(1, at=1:7, label = tweetPerDay$Day,
           col.axis="black", cex.axis=0.9)
      
      # plot(tweetPerDay$retweet_count, type = "l", main = "Retweet Count by Day", 
      #      xaxt = "n", xlab="", ylab="", 
      #      col = chart.col[1], lwd = 2)
      # axis(1, at=1:7, label = tweetPerDay$Day,
      #      col.axis="black", cex.axis=0.9)
      
      
    })
    
    
  })
})
