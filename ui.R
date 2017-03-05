library(httr)
library(jsonlite)
library(knitr)
library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)
library(maps)
library(mapdata)
library(devtools)
library(leaflet)

ui <- fluidPage(
  titlePanel("Title"),
  h3("By Kelsey Kua, Casey Lum, and Devin Reich"),
  h5("This report is about blah blah blah"),
  img(src="flag.jpg", height=245), 
  img(src="capitolbuilding.jpg", height=245), 
  img(src="congress.jpg", height=245), hr(),
  
  sidebarLayout(
    
    sidebarPanel(
      h3("Parameters"),
      textInput('zip', "Zip code", value="90210"),
      h5(em("Enter a zipcode to view legislators from the district(s) within that zipcode.")) 
      ),
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Your Representatives",
                           h3("Your Representatives"),
                           leafletOutput('leaflet', height = 800),
                           #plotOutput('alaska', click ='my.click'),
                          # splitLayout(
                           #  plotOutput('hawaii', click ='my.click'),
                            # plotOutput("map", click ='my.click')), 
                         # verbatimTextOutput('info'),
                          tableOutput('clickleg'),
                           ("Below are the members of Congress that represent the zipcode"),
                           textOutput('zipcode', inline=TRUE),
                          uiOutput('photosclick'),
                           tableOutput('reps'),
                           uiOutput('photos')),
                  
                  tabPanel("Compare Representatives",
                           verbatimTextOutput('senate')),
                  
                  tabPanel("Voting Record"),
                  
                  tabPanel("View a Vote"),
                  
                  tabPanel("Vote Breakdown"),
                  
                  tabPanel("Gender Makeup"),
                  
                  tabPanel("Party Makeup"),
                  
                  tabPanel("Voting Reliability")
                  ))
      ),
  hr(),
  ("Image credits for header photos (L to R):"), 
  tags$a(href="http://feelgrafix.com/group/american-flag.html", "feelgrafix", target = "_blank"), 
  ("|"),
  tags$a(href="https://en.wikipedia.org/wiki/United_States_Congress", "Wikipedia", target = "_blank"), 
  ("|"),
  tags$a(href="https://www.brookings.edu/multi-chapter-report/vital-statistics-on-congress/", "Brookings", target = "_blank"), br()
  
)
   