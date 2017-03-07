library(httr)
library(jsonlite)
library(knitr)
library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)
library(devtools)
library(leaflet)

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Congress App"),
  h3("Info 201 Final Project"),
  h3("By Kelsey Kua, Casey Lum, and Devin Reich"),
  img(src = "header.jpg", height = 255),
   hr(),
  

      tabsetPanel(type="tabs",
                  tabPanel("Welcome", icon = icon("hand-spock-o", lib = "font-awesome"),
                           h1("Welcome"),
                           h3("This app will allow you to explore congress data"),
                           actionButton('welcome', "Awesome!"),
                           textOutput("hi")),
                  tabPanel("Your Representatives", icon = icon("handshake-o", lib = "font-awesome"),
                           h2("Your Representatives"),
                           radioButtons('format', label = "Find representatives by...", choices = c("zipcode", "map"), selected = character(0)),
                           conditionalPanel(
                             condition = "input.format == 'map'", 
                             actionButton('reset', "Reset View", icon = icon("undo", lib = "font-awesome")), br(), br(), 
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
                  
                  tabPanel("Compare Two Reps", icon = icon("balance-scale", lib ="font-awesome")),
                  
                  tabPanel("Voting Record", icon = icon("history", lib = "font-awesome")),
                  
                  tabPanel("View a Vote", icon = icon("eye", lib = "font-awesome")),
                  

                  tabPanel("Gender Makeup", icon = icon("venus-mars", lib = "font-awesome"),
                           h3("Gender Makeup"),
                           "This page shows how the gender makeup has changed from 2009 to 2017 for both the house and the senate.",
                           br(), br(),
                           plotlyOutput("genderHouseArea"),
                           br(), br(),
                           plotlyOutput("genderHouseLine"),
                           br(), br(),
                           plotOutput("genderHousePie"),
                           br(), br(),
                           plotlyOutput("genderSenateArea"),
                           br(), br(),
                           plotlyOutput("genderSenateLine"),
                           br(), br(),
                           plotOutput("genderSenatePie")),
                  
                  tabPanel("Party Makeup", icon = icon("birthday-cake", lib = "font-awesome"),
                           h3("House"),
                           plotlyOutput("house.area"), br(),
                           plotlyOutput("house.line"), br(),
                           plotOutput("house.pie"),
                           h3("Senate"),
                           plotlyOutput("senate.area"), br(),
                           plotlyOutput("senate.line"), br(),
                           plotOutput("senate.pie")),
                  

      
                  tabPanel("Voting Reliability", icon = icon("check-square-o", lib = "font-awesome"),
                           h2("Voting Reliability: Missed Votes and Party Loyalty"), br(),
                          fluidRow(
                            column(3,
                                   radioButtons('congress', "Congress:",
                                                choices = c("114th", "115th"))),
                            column(3,
                                   radioButtons('party', "Party:", 
                                                    choices = c("all", "Democrat", "Republican", "Independent"), selected = "all")),
                            column(3,
                           selectInput('order', "Show Members:", choices = c("alphabetically", "decreasing", "increasing")))),
                          actionButton('table.button', "Show Table", icon = icon("table", lib = "font-awesome")),
                          hidden(uiOutput('graph.button')),
                           conditionalPanel(
                              condition = "input.congress == '115th'", 
                         #     h3("Percent of Votes Missed"),

                              plotlyOutput('house.missed'),

                              plotlyOutput('senate.missed'),
                      #        h3("Percent of Votes With Party"),
                              plotlyOutput('house.with'),
                              plotlyOutput('senate.with'),
                              fluidRow(
                                column(6,
                              hidden(dataTableOutput('house.115'))),
                              column(6,
                              hidden(dataTableOutput('senate.115'))))

                             
                           ),
                           
                           conditionalPanel(
                             condition = "input.congress == '114th'",
                     #        h3("Percent of Votes Missed"),
       
                          plotlyOutput('house.missed.114'),

                          plotlyOutput('senate.missed.114'),
                     #     h3("Percent of Votes With Party"),
                          plotlyOutput('house.with.114'),
                          plotlyOutput('senate.with.114')),
                          fluidRow(
                            column(6,
                                   hidden(dataTableOutput('house.114'))),
                            column(6,
                          hidden(dataTableOutput('senate.114'))))
)
),

  hr(),
  ("Image credits for header photos (L to R):"), 
  tags$a(href="http://feelgrafix.com/group/american-flag.html", "feelgrafix", target = "_blank"), 
  ("|"),
  tags$a(href="https://en.wikipedia.org/wiki/United_States_Congress", "Wikipedia", target = "_blank"), 
  ("|"),
  tags$a(href="https://www.brookings.edu/multi-chapter-report/vital-statistics-on-congress/", "Brookings", target = "_blank"), br(),

theme = "creative.css"
  
)