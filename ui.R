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
  # Titles the report
  titlePanel("Congress to You"),
  h3("INFO 201 Final Project"),
  h3("By Kelsey Kua, Casey Lum, and Devin Reich"),
  # Puts three patriotric images as the header
  img(src = "header.jpg", height = 255),
   hr(),
  
      # Creates navigation tabs
      tabsetPanel(type="tabs",
                  # Creates a welcome tab
                  tabPanel("Welcome", icon = icon("hand-spock-o", lib = "font-awesome"),
                           # Creates a header title
                           h1("Welcome to Congress to You"),
                           h2("Explore U.S. congressional data and use our tools to:"),
                           # Creates a description
                          h1(tags$li("Be informed:")),
                          fluidRow(column(6, offset = 1, h3("Look up recent bills & view the breakdown on votes"))),
                          h1(tags$li("Be involved:")),
                          fluidRow(column(6, offset = 1, h3("Find your representatives and how to contact them"))),
                          h1(tags$li("Be critical:")),
                          fluidRow(column(11, offset = 1, h3("Examine the demographic makeup of Congress over time & inspect representative's voting habits"))), br(),
                          h2("...it's now more important than ever."),
                           br(), 
                           
                           # Creates a cute button that doesn't do much but is still fun
                           actionButton('welcome', "Awesome!"),
                           textOutput("hi")
                           ),
                  
                  # Creates a tab called "Your Representatives"
                  tabPanel("Your Representatives", icon = icon("handshake-o", lib = "font-awesome"),
                           # Creates a header title
                           h2("Your Representatives"),
                           h3("Discover who is representing YOU in Congress."), br(),
                           
                           
                           # Creates radio buttons to find representatives by zip code or by clicking on a map
                           h4("Find representatives by:"),
                           radioButtons('format', label = NULL, choices = c("zip code", "map"), selected = character(0)),
                           
                           # Creates a conditional panel for when the user selects the "map" radio button
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
                             )
                           ),

                           # Creates a conditional panel for when the user selects the "zip code" radio button
                           conditionalPanel(
                             condition = "input.format == 'zip code'",
                             uiOutput('choice'),
                             em(strong("Note:"), "Some zip codes have more than representatives because they cover multiple congressional districts, which means they have 
                              more than one member of the House of Representatives.
                                Finding your location on the map will show your specific representatives."),
                             br(), br(),
                             tableOutput('reps'),
                             uiOutput('photos')
                           )
                           ),
                  

                  # Creates a tab called "Recent Bills"
                  tabPanel("Recent Bills", icon = icon("inbox", lib = "font-awesome"),
                           # Creates a header title
                           h2("Recent Bills"),
                           h3("See what Congress is working on."),
                           
                           # Displays the "I'm Just A Bill" video
                           fluidRow(
                             column(6, offset = 3,
                                    iframe(width = "672", height = "378",
                                           url_link="https://www.youtube.com/embed/tyeJ55o3El0"),
                                    ("via"), (em(tags$a(href = "http://www.schoolhouserock.tv/Bill.html", "Schoolhouse Rock!", target = "_blank")))
                             )
                           ),
                           hr(),
                           h5("Click one of the buttons below to look for recent bills."),

                           # Creates two action buttons where the user can choose a topic from a dropdown menu or input a search
                           actionButton("choose.topic", "Choose Topic"),
                           actionButton("search.text", "Search Text of Bills"), br(), br(),
                           
                           # Creates dropdown menu choices
                           hidden(selectInput("topic", label = NULL, 
                                              choices=c("budget", "defense", "diplomacy", "education", "guns",
                                                        "health", "immigration", "law", "taxes", "veterans", "welfare"))),
                           
                           # Creates the search input box
                           fluidRow(
                             column(3,
                                    hidden(textInput('search', label = NULL, placeholder = "e.g. Obama, women, China"))),
                             column(3,
                                    hidden(actionButton('go.table', "search bills", icon = icon("search", lib = "font-awesome"))))),
                           
                           # Outputs tables based on what the user enters
                           hidden(dataTableOutput('bills.topic')),
                           hidden(dataTableOutput('bills.search'))
                           ),
                  
                  # Creates a tab called "View a Vote"
                  tabPanel("View a Vote", icon = icon("eye", lib = "font-awesome"),
                           # Creates a header title
                           h2("View a Vote"),
                           h3("See how Congress voted on a roll call vote"),
                           
                           # Creates a description
                           p(("View the vote breakdown of any vote that took place during or after 2009 by entering the vote's roll id.
                            The roll id of a vote is made up of the chamber (h or s), the vote number, and the year the vote took place.
                              To find a vote's roll id, look it up on the"), tags$a(href="https://www.govtrack.us/congress/votes", "GovTrack", target = "_blank"), ("database.")),
                           p(("Some interesting votes to view include:"), br(),
                           ("h2-2017: the 2017 Election of the Speaker of the House"), br(),
                           ("h65-2017: the No Taxpayer Funding for Abortion and 
                            Abortion Insurance Full Disclosure Act"), br(),
                           ("s59-2017: Senate confirmation of Jeff Sessions for attorney general"), br(),
                           ("s334-2015: Every Child Achieves Act of 2015"), br(),
                           ("h768-2009: the Patient Protection and Affordable Care Act")), br(), 
                           # Creates action buttons that let the user either select a bill from a dropdown menu or enter a roll id

                           

                           
                           # Creates an input textbox and an enter button
                           fluidRow(
                             column(3, textInput('own.roll.id', label = NULL, placeholder = "enter roll.id")),
                             column(3, actionButton('go.vote', "look up vote", icon = icon("search", lib = "font-awesome")))
                           ),
                           fluidRow(
                             column(4,
                                    hidden(tableOutput('vote.own'))),
                             column(4,
                                    hr(), hidden(uiOutput("vote.title")),
                                    hidden(tableOutput('vote.own.table')), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), 
                                    hr(), hidden(uiOutput("gender.title")),
                                    hidden(tableOutput('gender.table.own')), br(), br(), br(), br(), br(), br(), br(), 
                                    hr(), hidden(uiOutput("party.title")),
                                    hidden(tableOutput("party.voting"))),
                             column(4,
                                    hidden(plotOutput('own.pie.chart')), br(), br(),
                                    hidden(plotOutput('gender.voting.own'))))),
              
                  # Creates a tab called "Gender Makeup"
                  tabPanel("Gender Makeup", icon = icon("venus-mars", lib = "font-awesome"),
                           # Creates a header title
                           h2("Gender Makeup"),
                           
                           # Creates a description
                           h3("Examine the gender makeup of the House and Senate from 2009-2017."),
                           br(), 
                           
                           # Creates radio buttons to view plots for the House and Senate
                           fluidRow(
                             column(12, radioButtons('chamber', label = "Chamber",
                                                     choices = c("House of Representatives", "Senate")
                                                     )
                                    )
                           ),
                           
                           # Creates a conditional panel for when the "House of Representatives" button is selected
                           conditionalPanel(
                             condition = "input.chamber == 'House of Representatives'",
                             
                             # Creates a header
                             h4(em("Gender Makeup in the House of Representatives"), align = "center"),
                             br(), br(),
                             
                             # Outputs tables showing information about the House
                             tableOutput("genderHouseTable"),
                             br(), br(),
                             plotlyOutput("genderHouseArea"),
                             br(), br(),
                             plotlyOutput("genderHouseLine"),
                             br(), br(), br(),
                             plotOutput("genderHousePie"),
                             align = "center"
                           ),
                           
                           # Creates a conditional panel for when the "Senate" button is selected
                           conditionalPanel(
                             condition = "input.chamber == 'Senate'",
                             
                             # Creates a header
                             h4(em("Gender Makeup in the Senate"), align = "center"),
                             br(), br(),
                             
                             # Outputs tables showing information about the Senate
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
                           p("As seen in the data table and plots, the ratio of females to males has experienced little change from 2009 to 2017
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
                           )),
                  
                  # Creates a tab called "Party Makeup"
                  tabPanel("Party Makeup", icon = icon("birthday-cake", lib = "font-awesome"),
                           # Creates a title
                           h2("Party Makeup"),
                           h3("Examine the party makeup of the House and Senate: since the 102nd House and the 80th Senate."),
                           p("A note about Congress numbers: each Congress spans two years, starting in every odd year - the first Congress started in 1789. The 102nd Congress began in 1991, and the
                              80th began in 1947."),
                           
                           # Outputs plots of House data
                           plotlyOutput("house.area"), br(),
                           plotlyOutput("house.line"), br(),
                           plotOutput("house.pie"), br(),
                           
                           # Creates a title
                           h3("Senate"),
                           
                           # Creates a button that explains why the plots may show more than 100 senators
                           actionButton("senate.q", "What?? Doesn't the senate have 100 members??"),
                           hidden(textOutput("senate.ex")),
                           
                           # Outputs plots of Senate data
                           plotlyOutput("senate.area"), br(),
                           plotlyOutput("senate.line"), br(),
                           plotOutput("senate.pie"),
                           
                           # Creates a description
                           p("In both the House and the Senate, the majority party fluctuates between Democrat
                              and Republican. This can be most clearly seen in the line graphs for the House and the Senate, although 
                              the majority party in the Senate seems to change more often than the House. Historically, the Senate
                              has more Independent members than the House, but both chambers have relatively few Independents.")
                           ),
                  
                  # Creates a tab called "Voting Reliability"
                  tabPanel("Voting Reliability", icon = icon("check-square-o", lib = "font-awesome"),
                           # Creates a header title
                           h2("Voting Reliability"),
                            h3("Inspect Representatives' % of Missed Votes and Party Loyalty."), 
                           
                           # Creates a description
                           p("Below are bar graphs showing representatives' percentage of missed votes and how often they vote with the
                              majority of their party. The votes with party percentage is the percentage of votes that are same as the majority vote of the party.
                              Each bar represents a member of Congress; hover over bars to see the members' information.
                              Use the widgets to display the 114th or 115th Congress, to filter by party or state, 
                              and to change the order of the bars on the graphs. It is important to note that John Boehner and Paul Ryan have high
                              percentages of votes missed in the 114th (and 115th in Ryan's case)
                              because they served as Speaker of the House. The Speaker has the right to vote on bills, but does not regularly participate.
                              As for the House of Representatives of the 114th Congress, Ryan Zinke, Tom Price, Mike Pompeo, and Mick Mulvaney are shown to have high
                              percentages of votes missed, which is likely due to the fact that were recently nominated and confirmed
                              as Secretary of the Interior, Secretary of Health and
                              Human Services, director of the CIA, and director of the Office of Management and Budget, respectively. Xavier Becerra also has a high
                              percentage of votes missed; he was confirmed as the Attorney General of California in January.
                              It is also interesting to note that in the Senate of the 114th Congress, the members with the highest percentages of votes missed were
                              Ted Cruz, Marco Rubio, and Bernie Sanders: all candidates in the presidential general election."),

                           br(),
                           
                           # Creates a set of filters like radio buttons and dropdown menus
                           fluidRow(
                             # Creates radio buttons to select the 114th or 115th Congress
                              column(2,
                                  radioButtons('congress', "Congress:", 
                                               choices = c("114th", "115th"))),
                              
                              # Creates radio buttons to select the party
                              column(2,
                                  radioButtons('party', "Party:", 
                                               choices = c("all", "Democrat", "Republican", "Independent"), selected = "all")),
                              
                              # Creates a dropdown menu of state selections
                              column(2,
                                     selectInput('state', "State",
                                                 choices = c("all", "AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", 
                                                             "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", 
                                                             "OH", "OK", "OR", "PA", "RI", "SC",  "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV",  "WY"),
                                                 
                                                 selected = "all")),
                              
                              # Creates a dropdown menu that arranges the data in the plots
                              column(3,
                                  selectInput('order', "Order:", 
                                              choices = c("alphabetical by last name", "increasing", "decreasing")))),
                              
                          
                          # Creates a button that changes the view from plots to table and vice versa
                          actionButton('table.button', "Show Table", icon = icon("table", lib = "font-awesome")),
                          hidden(actionButton('graph.button', "Return to Graph", icon = icon("bar-chart", lib = "font-awesome"))),
                          
                          # Creates a conditional panel for when the 115th Congress radio button is selected
                          conditionalPanel(
                            condition = "input.congress == '115th'", 
                            
                            # Creates plots showing information about the 115th Congress
                            plotlyOutput('house.missed'), br(), br(),
                            plotlyOutput('senate.missed'), br(), br(),
                            plotlyOutput('house.with'), br(), br(),
                            plotlyOutput('senate.with'),
                            fluidRow(
                              column(6, hidden(tableOutput('house.115'))),
                              column(6, hidden(tableOutput('senate.115')))
                            )
                          ),
                          
                          # Creates a conditional panel for when the 114th Congress radio button is selected
                          conditionalPanel(
                            condition = "input.congress == '114th'",
                            
                            # Creates plots showing information about the 114th Congress
                            plotlyOutput('house.missed.114'), br(), br(),
                            plotlyOutput('senate.missed.114'), br(), br(),
                            plotlyOutput('house.with.114'), br(), br(),
                            plotlyOutput('senate.with.114'),
                            fluidRow(
                              column(6, hidden(tableOutput('house.114'))),
                              column(6, hidden(tableOutput('senate.114')))
                            )
                          )
                          )
                  ),

      hr(),
  
      # Creates a footer that provides hyperlinks to the images from the header
      "Image credits for header photos (L to R):",
      tags$a(href="http://feelgrafix.com/group/american-flag.html", "feelgrafix", target = "_blank"), 
      ("|"),
      tags$a(href="https://en.wikipedia.org/wiki/United_States_Congress", "Wikipedia", target = "_blank"), 
      ("|"),
      tags$a(href="https://www.brookings.edu/multi-chapter-report/vital-statistics-on-congress/", "Brookings", target = "_blank"), br(),
  
      ("This app was created with"), 
      tags$a(href = "https://www.rstudio.com", "RStudio", target= "_blank"), ("and uses data from the"),
      tags$a(href="https://sunlightlabs.github.io/congress/", "Sunlight Congress API", target = "_blank"),
      ("and the"),
      tags$a(href="https://www.propublica.org/datastore/api/propublica-congress-api", "ProPublica Congress API", target = "_blank"), hr(),

      # Sets the theme of the app
      theme = "creative.css"
      
      
)