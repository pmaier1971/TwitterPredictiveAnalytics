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

options(scipen=999)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Twitter: Predictive Analytics"),

    fluidRow(
      column(3, fileInput("fileTwitterData", "Upload Twitter Data")
             )
    ),
    
    # fluidRow(
    #   column(3, 
    #          uiOutput("selector_hashags")
    #   )
    # ),
    
    tabsetPanel(type = "tabs",
                tabPanel("Basic Stats", plotOutput("ImpressionsByDay"), 
                         plotOutput("ImpressionsByHour"), plotOutput("ImpressionsByMinute")),
                tabPanel("HashTags", uiOutput("selector_hashags"), plotOutput("plotHashtags")),
                tabPanel("Data", dataTableOutput("dataCheck"))
    )
    
    #plotOutput("BasicStats"),
    
    #dataTableOutput("dataCheck")

    ))
