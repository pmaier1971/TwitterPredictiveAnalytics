#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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
pkgTest("shinydashboard")


options(scipen=999)
quartzFonts(avenir = c("Avenir Book", "Avenir Black", "Avenir Book Oblique", 
                       "Avenir Black Oblique"))


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Twitter: Predictive Analytics"),
  
  dashboardBody(
    fluidRow(
      column(3, fileInput("fileTwitterData", "Upload Twitter Data")),
      column(6, 
             valueBoxOutput("valuebox_no_tweets"),
             valueBoxOutput("valuebox_daterange"),
             valueBoxOutput("valuebox_impressionsPerday"))
    ),
    
    
    tabsetPanel(type = "tabs",
                tabPanel("Basic Stats", 
                         fluidRow(
                           column(4, plotOutput("ImpressionsByDay")), 
                           column(4, plotOutput("ImpressionsByHour")),
                           column(4, plotOutput("ImpressionsByMinute"))
                         )),
                tabPanel("HashTag Analysis", 
                         fluidRow(
                           column(6, plotOutput("HashtagPopularity") ),
                           column(6, plotOutput("HashtagImpact") )),
                         uiOutput("selector_hashags"),
                         fluidRow(
                           column(6, plotOutput("HashtagMakeDifferences")),
                           column(6, plotOutput("plotHashtags"))) 
                ),
                tabPanel("Explore", dataTableOutput("dataCheck"))
                
    )
  
  )
  
  
  
))

