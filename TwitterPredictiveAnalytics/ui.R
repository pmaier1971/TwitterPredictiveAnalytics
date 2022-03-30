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
    
    plotOutput("BasicStats"),
    
    dataTableOutput("dataCheck")

    ))
