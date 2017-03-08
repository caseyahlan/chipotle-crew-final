library(httr)
library(jsonlite)
library(knitr)
library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)
library(devtools)
library(leaflet)
library(shinyjs)
library(xtable)
library(shinyLP)

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Congress App"),
  h3("INFO 201 Final Project"),
  h3("By Kelsey Kua, Casey Lum, and Devin Reich"),
  img(src = "header.jpg", height = 255),
   hr(),
  

      tabsetPanel(type="tabs",
                  tabPanel("Welcome", icon = icon("hand-spock-o", lib = "font-awesome"),
                           h1("Welcome"),
                           "This app will allow you to explore Congress data taken from the ",
                           tags$a(href="https://sunlightlabs.github.io/congress/", "Sunlight Congress API", target = "_blank"),
                           "and the ",
                           tags$a(href="https://www.propublica.org/datastore/api/propublica-congress-api", "ProPublica Congress API", target = "_blank"),
                           ". You can view information about the representatives for a selected area, see how a representative voted, and view gender and party makeup and overall voting reliability in Congress. 
                           Have fun exploring our app!",
                           br(), br(),
                           actionButton('welcome', "Let's get started!"),
                           textOutput("hi")),
                  
                  tabPanel("Your Representatives", icon = icon("handshake-o", lib = "font-awesome"),
                           h2("Your Representatives"),
                           "Click on one of the radio buttons below to find representatives from a selected area. 
                           You can find representatives by entering a zip code or clicking on a location on the map.",
                           br(), br(),
                           radioButtons('format', label = "Find representatives by...", choices = c("zip code", "map"), selected = character(0)),
                           conditionalPanel(
                             condition = "input.format == 'map'", 
                             actionButton('reset', "Reset View", icon = icon("undo", lib = "font-awesome")), br(), 
                             fluidRow(
                               column(6,
                                      em("Click a point on the map to view its representatives"), br(), br(),
                                      leafletOutput('leaf.let')),
                               column(6,
                             tableOutput('clickleg'),
                             uiOutput('photosclick'))              
                             )),

                           conditionalPanel(
                             condition = "input.format == 'zip code'",
                             uiOutput('choice'),
                             em(strong("Note:"), "Zip codes have varying numbers of representatives because some 
                                zip codes span multiple congressional districts, meaning multiple members of the House are shown. 
                                Finding your location on the map will show the representatives for that area."),
                             br(), br(),
                             tableOutput('reps'),
                             uiOutput('photos'))
                  ),
                  

                  tabPanel("Recent Bills", icon = icon("inbox", lib = "font-awesome"),
                           h3("Recent Bills"),
                           fluidRow(
                             column(6, offset = 3,
                           iframe(width = "672", height = "378", url_link="https://www.youtube.com/embed/tyeJ55o3El0"),
                           ("via"), (em(tags$a(href = "http://www.schoolhouserock.tv/Bill.html", "Schoolhouse Rock!", target = "_blank"))))), hr(),
                           "Click one of the buttons below to look for a bill.",
                           br(), br(),
                           actionButton("choose.topic", "Choose Topic"),
                           actionButton("search.text", "Search Text of Bills"),
                           hidden(selectInput("topic", label = NULL, choices=c("education", "health", "guns", "veterans", "budget", 
                                                                     "law", "welfare", "taxes", "diplomacy", "defense", "immigration"))),
                           hidden(textInput('search', "Search text of bills", placeholder = "e.g. immigration, Washington, taxes")),
                           hidden(dataTableOutput('bills.topic')),
                           hidden(dataTableOutput('bills.search'))
                           ),
                  
                  tabPanel("View a Vote", icon = icon("eye", lib = "font-awesome")),
                  
                  
                  tabPanel("Gender Makeup", icon = icon("venus-mars", lib = "font-awesome"),
                           h3("Gender Makeup"),
                           "This page shows how the gender makeup has changed from 2009 to 2017 for both the 
                           House of Representatives and the Senate. Click on one of the radio buttons below to see
                           gender information about the House or the Senate.",
                           br(), br(), br(),
                           fluidRow(
                             column(12, 
                                    radioButtons('chamber',
                                                 label = "Chamber",
                                                 choices = c("House of Representatives", "Senate")
                                                 )
                                    )
                           ),
                           conditionalPanel(
                             condition = "input.chamber == 'House of Representatives'",
                             h4(em("Gender Makeup in the House of Representatives"), align = "center"),
                             br(), br(),
                             tableOutput("genderHouseTable"),
                             br(), br(),
                             plotlyOutput("genderHouseArea"),
                             br(), br(),
                             plotlyOutput("genderHouseLine"),
                             br(), br(), br(),
                             plotOutput("genderHousePie"),
                             align = "center"
                           ),
                           conditionalPanel(
                             condition = "input.chamber == 'Senate'",
                             h4(em("Gender Makeup in the Senate"), align = "center"),
                             br(), br(),
                             tableOutput("genderSenateTable"),
                             br(), br(),
                             plotlyOutput("genderSenateArea"),
                             br(), br(),
                             plotlyOutput("genderSenateLine"),
                             br(), br(), br(),
                             plotOutput("genderSenatePie"),
                             align = "center"
                           ),
                           br(), br(),
                           "As seen in the data table and plots, the ratio of females to males has experienced little change from 2009 to 2017
                           for both the House and the Senate. Even though the public is becoming more aware of the gender diversity (or lack thereof) 
                           in predominantly male fields (e.g. STEM fields, politics, military), this data shows that there has been 
                           negligible change in the number of women in both the House and the Senate. What would this mean in terms of how 
                           effective the push for more diversity is in these predominantly male fields? As ",
                           tags$a(href="https://www.gillibrand.senate.gov/", "Senator Kirsten Gillibrand", target = "_blank"),
                           " points out, ",
                           tags$a(href="http://www.dailykos.com/story/2012/09/27/1137167/-Women-Are-The-Key-To-Holding-Onto-The-Senate",
                                  "women make up 51% of the United States population but only 17% of Congress.", target = "_blank"),
                           br(), br(),
                           "To learn more about women's involvement in politics, visit the ",
                           tags$a(href="http://www.cawp.rutgers.edu/facts", "Center for American Women and Politics website.", target = "_blank")
                  ),
                  
                  tabPanel("Party Makeup", icon = icon("birthday-cake", lib = "font-awesome"),
                           h3("House"),
                           plotlyOutput("house.area"), br(),
                           plotlyOutput("house.line"), br(),
                           plotOutput("house.pie"),
                           h3("Senate"),
                           actionButton("senate.q", "What? I thought the senate has 100 members!"),
                           hidden(textOutput("senate.ex")),
                           plotlyOutput("senate.area"), br(),
                           plotlyOutput("senate.line"), br(),
                           plotOutput("senate.pie")),
                  
                  tabPanel("Voting Reliability", 
                           icon = icon("check-square-o", lib = "font-awesome"),
                           h2("Voting Reliability: Missed Votes and Party Loyalty"), br(),
                          fluidRow(
                            column(2,
                                   radioButtons('congress', "Congress:",
                                                choices = c("114th", "115th"))),
                            column(2,
                                   radioButtons('party', "Party:", 
                                                    choices = c("all", "Democrat", "Republican", "Independent"), selected = "all")),
                            column(3,
                                    selectInput('order', "Show Members:", 
                                                    choices = c("alphabetically", "decreasing", "increasing"))),
                            column(3,
                                   selectInput('state', "State", choices = c("all", "AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", 
                                                                                    "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", 
                                                                                    "SC",  "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV",  "WY"),
                                                      selected = "all"))),
                          actionButton('table.button', "Show Table", icon = icon("table", lib = "font-awesome")),
                          hidden(actionButton('graph.button', "Return to Graph", icon = icon("bar-chart", lib = "font-awesome"))),

                           conditionalPanel(
                              condition = "input.congress == '115th'", 
                              plotlyOutput('house.missed'), 
                              plotlyOutput('senate.missed'), 
                              plotlyOutput('house.with'), 
                              plotlyOutput('senate.with'), 
                              fluidRow(
                                column(6,
                              hidden(tableOutput('house.115'))),
                              column(6,
                              hidden(tableOutput('senate.115'))))
                           ),
                           conditionalPanel(
                             condition = "input.congress == '114th'",
                          plotlyOutput('house.missed.114'), hr(),
                          plotlyOutput('senate.missed.114'), 
                          plotlyOutput('house.with.114'), 
                          plotlyOutput('senate.with.114'),
                          fluidRow(
                            column(6,
                                   hidden(tableOutput('house.114'))),
                            column(6,
                          hidden(tableOutput('senate.114'))))
))
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