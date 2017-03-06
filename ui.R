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
  
#  sidebarLayout(
    
#    sidebarPanel(
 #     h3("Parameters"),
 #     radioButtons('format', label = "Find representatives by...", choices = c("zipcode", "map"), selected = NULL)),     
 #   mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Your Representatives",
                           h2("Your Representatives"),
                           radioButtons('format', label = "Find representatives by...", choices = c("zipcode", "map"), selected = character(0)),
                           conditionalPanel(
                             condition = "input.format == 'map'", 
                             actionButton('reset', "Reset View"), br(), br(), 
                             leafletOutput('leaf.let', height = 650),
                             uiOutput('explanation'),
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
                  
                  tabPanel("Voting Record"),
                  
                  tabPanel("View a Vote"),
                  

                  tabPanel("Gender Makeup"),
                  
                  tabPanel("Party Makeup", 
                           h3("House"),
                           plotlyOutput("house.area"), br(),
                           plotlyOutput("house.line"), br(),
                           plotOutput("house.pie"),
                           h3("Senate"),
                           plotlyOutput("senate.area"), br(),
                           plotlyOutput("senate.line"), br(),
                           plotOutput("senate.pie")),
                  
                  tabPanel("Voting Reliability",
                           h2("Voting Reliability: Missed Votes and Party Loyalty"), br(),
                           radioButtons('party', "View by party:", 
                                                    choices = c("all", "Democrat", "Republican", "Independent"), selected = "all"),
                            radioButtons('congress', "Congress:",
                                                    choices = c("114th", "115th"), selected = character(0)),
                           conditionalPanel(
                              condition = "input.congress == '115th'", 
                              strong("Note:"), ("all percentages are inflated by 0.5% so that members with 0 missed
                                             votes are still shown on the graph"),
                              plotlyOutput('house.missed'),
                              strong("Note:"), ("all percentages are inflated by 0.2% so that members with 0 missed
                                             votes are still shown on the graph"),
                              plotlyOutput('senate.missed'),
                              plotlyOutput('house.with'),
                              plotlyOutput('senate.with')
                             
                           ),
                           
                           conditionalPanel(
                             condition = "input.congress == '114th'",
                             strong("Note:"), ("all percentages are inflated by 0.5% so that members with 0 missed
                                             votes are still shown on the graph"),         
                          plotlyOutput('house.missed.114'),
                          strong("Note:"), ("all percentages are inflated by 0.2% so that members with 0 missed
                                             votes are still shown on the graph"),
                          plotlyOutput('senate.missed.114'),
                          plotlyOutput('house.with.114'),
                          plotlyOutput('senate.with.114')
)
)
     ),
#    )),
  hr(),
  ("Image credits for header photos (L to R):"), 
  tags$a(href="http://feelgrafix.com/group/american-flag.html", "feelgrafix", target = "_blank"), 
  ("|"),
  tags$a(href="https://en.wikipedia.org/wiki/United_States_Congress", "Wikipedia", target = "_blank"), 
  ("|"),
  tags$a(href="https://www.brookings.edu/multi-chapter-report/vital-statistics-on-congress/", "Brookings", target = "_blank"), br()
  
)