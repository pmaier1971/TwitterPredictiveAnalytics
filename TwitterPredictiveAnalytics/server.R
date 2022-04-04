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
pkgTest("RColorBrewer")
pkgTest("shinydashboard")

options(scipen=999)

quartzFonts(avenir = c("Avenir Book", "Avenir Black", "Avenir Book Oblique", 
                       "Avenir Black Oblique"))

chart.col   = brewer.pal(6, "Paired")

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
      
      
      list_hashtags = sort( unique(tolower(unlist(list_hashtags))) )
      
      list_hashtags = sort( tolower(unique(unlist(list_hashtags))) )
      
      
      selectInput("uiSelection_hashtag", label = "Hashtags", list_hashtags)
      
    })
    
    
    # Boxes
    
    output$valuebox_no_tweets <- renderValueBox({
      valueBox(
        nrow(twitterData()) , "Tweets Analyzed", #icon = icon("list"),
        icon = icon("thumbs-up", lib = "glyphicon"),
        color = "purple"
      )
    })
    
    
    output$valuebox_daterange <- renderValueBox({
      
      twitterData = twitterData()
      
      valueBox(
        as.character( abs( as.numeric( difftime( twitterData$date[1], twitterData$date[nrow(twitterData)], units = "days" ) ))), 
        "Days Of Data", icon = icon("list"),
        color = "blue"
      )
    })
    
    output$valuebox_impressionsPerday <- renderValueBox({
      
      twitterData = twitterData()
      
      valueBox(
        as.character( 
          round( mean(twitterData$impressions), 0)
        ), 
        "Impressions / Tweet", icon = icon("list"),
        color = "red"
      )
    })
    
    # Analysis
    
    output$HashtagPopularity = renderPlot({
      twitterData = twitterData()
      
      list_hashtags = list()
      for (idx in 1:nrow(twitterData)) {
        list_hashtags = append(list_hashtags, str_extract_all(twitterData[idx, "Tweet.text"], '#\\w+', simplify = T))
      }
      
      list_hashtags = unique(tolower(unlist(list_hashtags)))
      
      table_hashtag_popularity = data.frame(Hashtag = list_hashtags, Popularity = 0)
      
      for (idx in 1:nrow(table_hashtag_popularity)) {
        for (idx2 in 1:nrow(twitterData)) {
          if (table_hashtag_popularity[idx, "Hashtag"] %in% tolower( str_extract_all(twitterData[idx2, "Tweet.text"], '#\\w+', simplify = T)) ){
            table_hashtag_popularity[idx, "Popularity"] = table_hashtag_popularity[idx, "Popularity"] +1
          }
        }
      }
      
      table_hashtag_popularity = table_hashtag_popularity[order(table_hashtag_popularity$Popularity,decreasing = TRUE),]
      
      par(mar=c(4,10,4,4))
      hashtag_plot = barplot(table_hashtag_popularity[5:1, 2], horiz = TRUE, 
                             col = chart.col[1:5], main = "Top-5 Most Used Hashtags" )
      axis(2, at = hashtag_plot, labels = table_hashtag_popularity[5:1,1], las = 2)
      
    })
    
    output$HashtagImpact = renderPlot({
      twitterData = twitterData()
      
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
      
      table_hashtag_popularity$Impressions = table_hashtag_popularity$Impressions / table_hashtag_popularity$Popularity
      table_hashtag_popularity = table_hashtag_popularity[order(table_hashtag_popularity$Impressions,decreasing = TRUE),]
      
      par(mar=c(4,10,4,4))
      
      hashtag_plot = barplot(table_hashtag_popularity[5:1, 3], horiz = TRUE, col = chart.col[1:5], 
                             main = "Top-5 Hashtags Generating Most Impressions" )
      axis(2, at = hashtag_plot, labels = table_hashtag_popularity[5:1,1], las = 2)
      
    })
    
    
    output$HashtagMakeDifferences = renderPlot({
      twitterData = twitterData()
      
      twitterData$NoHashTag = 0
      twitterData$ContainsHashTagRstats = 0
      twitterData$ContainsMultipleHashTags = 0
      twitterData$ContainsChosenHashTag = 0
      
      
      for (idx in 1:nrow(twitterData)) {
        if (is.logical( tolower( str_extract_all(twitterData[idx, "Tweet.text"], '#\\w+', simplify = T)) ) ) {
          # Tweet has no hashtags
          
          twitterData[idx, "NoHashTag"] = 1
          
        } else {
          if ("#rstats" %in% tolower( str_extract_all(twitterData[idx, "Tweet.text"], '#\\w+', simplify = T)) ) {
            twitterData[idx, "ContainsHashTagRstats"] = 1 }  
          if (length (tolower( str_extract_all(twitterData[idx, "Tweet.text"], '#\\w+', simplify = T)) )>1 ) { 
            twitterData[idx, "ContainsMultipleHashTags"] = 1
          }
          if (input$uiSelection_hashtag %in% tolower( str_extract_all(twitterData[idx, "Tweet.text"], '#\\w+', simplify = T)) ) {
            twitterData[idx, "ContainsChosenHashTag"] = 1 } 
        }  
      }
      
      results = data.frame(
        NoHashTag = 0,
        ContainsHashTagRstats = 0,
        ContainsMultipleHashTags = 0,
        ContainsChosenHashTag = 0
      )
      
      results$NoHashTag = mean( twitterData[ twitterData$NoHashTag == 1, "impressions"] ) 
      results$ContainsHashTagRstats = mean( twitterData[ twitterData$ContainsHashTagRstats == 1, "impressions"] )
      results$ContainsMultipleHashTags = mean( twitterData[ twitterData$ContainsMultipleHashTags == 1, "impressions"] )
      results$ContainsChosenHashTag = mean( twitterData[ twitterData$ContainsChosenHashTag == 1, "impressions"] )
      
      if (is.nan(results$NoHashTag)) results$NoHashTag = 0
      
      par(mar=c(4,10,4,4))
      
      hashtag_plot = barplot(as.matrix(results), horiz = TRUE, col = chart.col[1:3], yaxt = "n",
                             main = "Pick The Right Hashtag" )
      axis(2, at = hashtag_plot, labels = c("No Hashtag", "#rstats", "Multiple", input$uiSelection_hashtag), las = 2)
      
    })
    
    
    
    # Analysis
    
    output$dataCheck = renderDataTable({
      results = twitterData()
      results = results[,3:6]
      return(results)
    })
    
    output$ImpressionsByDay = renderPlot({
      
      data = twitterData()
      data$NoOfTweets = 1
      dataElements = c("impressions", "engagements", "retweets", "replies", "NoOfTweets")
      
      tweetPerDay = aggregate(data[,dataElements], by=list(Day = data$weekday), sum)
      tweetPerDay$Day = factor(tweetPerDay$Day, levels= c("Monday", 
                                                          "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
      tweetPerDay = tweetPerDay[order(tweetPerDay$Day), ]
      tweetPerDay$impressions = tweetPerDay$impressions / tweetPerDay$NoOfTweets
      
      
      plot(tweetPerDay$impressions, type = "l", main = paste0("Which Day of the Week Generates \nThe Most Impressions?"), 
                xaxt = "n", xlab="", ylab="",
                col = chart.col[1], lwd = 2)
           axis(1, at=1:7, label = tweetPerDay$Day,
                col.axis="black", cex.axis=0.9)
           
    })
      
      output$ImpressionsByHour = renderPlot({
        
        data = twitterData()
        
        data$NoOfTweets = 1
        dataElements = c("impressions", "engagements", "retweets", "replies", "NoOfTweets")
        
        
        tweetPerHour = aggregate(data[,dataElements], by=list(Hour = data$hour), sum)
        tweetPerHour = left_join( data.frame(Hour = 1:24), tweetPerHour)
        tweetPerHour$impressions = tweetPerHour$impressions / tweetPerHour$NoOfTweets
        tweetPerHour[is.na(tweetPerHour)] = 0
        
        plot(tweetPerHour$impressions, type = "l", main = "What Hour of the Day \nGenerates The Most Impressions?", 
             xaxt = "n", 
             xlab="", ylab="",
             col = chart.col[1], lwd = 2)
        axis(1, at=1:nrow(tweetPerHour), label = paste0(tweetPerHour$Hour, ":00") )
             
      })
        
        output$ImpressionsByMinute = renderPlot({
          
          data = twitterData()
          
          data$NoOfTweets = 1
          dataElements = c("impressions", "engagements", "retweets", "replies", "NoOfTweets")
          
          tweetPerMinute = aggregate(data[,dataElements], by=list(Minute = data$minute), sum)
          tweetPerMinute = left_join( data.frame(Minute = 1:60), tweetPerMinute)
          tweetPerMinute$impressions = tweetPerMinute$impressions / tweetPerMinute$NoOfTweets
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
      