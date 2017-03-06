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
      radioButtons('format', label = "Find representatives by...", choices = c("zipcode", "map"), selected = NULL)),     
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Your Representatives",
                           h3("Your Representatives"),
                           conditionalPanel(
                             condition = "input.format == 'map'",
                              leafletOutput('leaflet', height = 500),
                              tableOutput('clickleg'),
                              uiOutput('photosclick')
                           ),
                           conditionalPanel(
                             condition = "input.format == 'zipcode'",
                             uiOutput('choice'),
                           tableOutput('reps'),
                           uiOutput('photos'))
                           ),
                  
                  tabPanel("Compare Representatives"),
                  
                  tabPanel("Voting Record",
                           verbatimTextOutput('districts')),
                  
                  tabPanel("View a Vote"),
                  
                  tabPanel("Vote Breakdown"),
                  
                  tabPanel("Gender Makeup"),
                  
                  tabPanel("Party Makeup"),
                  
                  tabPanel("Voting Reliability")
                  )
      )),
  hr(),
  ("Image credits for header photos (L to R):"), 
  tags$a(href="http://feelgrafix.com/group/american-flag.html", "feelgrafix", target = "_blank"), 
  ("|"),
  tags$a(href="https://en.wikipedia.org/wiki/United_States_Congress", "Wikipedia", target = "_blank"), 
  ("|"),
  tags$a(href="https://www.brookings.edu/multi-chapter-report/vital-statistics-on-congress/", "Brookings", target = "_blank"), br()
  
)
   