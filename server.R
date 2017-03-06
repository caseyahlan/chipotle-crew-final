library(httr)
library(jsonlite)
library(knitr)
library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)
library(maps)
library(fiftystater)
library(mapdata)
library(sp)
library(geojsonio)
library(curlconverter)
library(tidyr)
source("apikey.R")

state <- geojson_read("stateData.geojson", what = "sp")
class(state)


# Sunlight API base
sunlight.base <- "https://congress.api.sunlightfoundation.com/"
# Propublica API Base
propublica.base <- "https://api.propublica.org/congress/v1/"

house.makeup <- read.csv("house.makeup", stringsAsFactors = FALSE)


# 'GENDERS IN CONGRESS' SECTION
# Function that finds the gender composition by examining votes
GetGenderMakeup <- function(roll.id) {
  base <- ("https://congress.api.sunlightfoundation.com/")
  votes.resource <- ("votes?roll_id=")
  votes.filters <- ("&fields=voters")
  votes.response <- GET(paste0(base, votes.resource, roll.id, votes.filters))
  request.body.as.list <- content(votes.response)
  voters.list <- request.body.as.list$results[[1]]$voters
  names(voters.list) <- NULL
  voters.json <- toJSON(voters.list)
  voters.as.data.frame <- flatten(fromJSON(voters.json, flatten=TRUE))
  voters <- voters.as.data.frame %>% select(voter.party, voter.gender, vote)
  voters$voter.gender <- as.factor(unlist(voters$voter.gender))
  voters.gender <- tally(group_by(voters, voter.gender))
  return(voters.gender)
}


# Creates a data frame of gender breakdown from 2009 to 2017
resource <- "votes"
legislators.by.gender <- data.frame(c("F", "M"))
years <- c(2009 : 2017)
for (year in years) {
  query <- paste0("?chamber=", "house", "&per_page=", "all", "&year=", year)
  response <- GET(paste0(sunlight.base, resource, query))
  body <- fromJSON(content(response, "text"))
  body <- flatten(body$results)
  legislators.by.gender <- cbind(legislators.by.gender, select(GetGenderMakeup(body[1, "roll_id"]), n))
}
#colnames(legislators.by.gender) <- c("Gender", 1:9)

#voter.party <- c("D", "I","R")
#x <- select(x, -voter.party)
#x <- gather(x, key = Year,
   #         value  = Members,
   #         1:26, convert = TRUE)
#x <- data.frame(voter.party,x)
#ggplot(x, aes(x=Year, y=Members, fill=voter.party)) + 
#  geom_area()

# Server function
server <- function(input, output) {
  output$choice <- renderUI({
      textInput('zip', "Zipcode", value = "90210")
  })
  
  legislators <- reactive({
    resource <- "legislators/locate"
    query <- paste0("?zip=", 98502)
    response <- GET(paste0(sunlight.base, resource, query))
    body <- fromJSON(content(response, "text"))
    legislators <- flatten(body$results) %>% mutate(name = paste(first_name, last_name)) %>% select(name, chamber, party, state, phone, website)
    return(legislators)
  })
  
  output$reps <- renderTable({
    return(legislators())
  })
  
  congressmen <- reactive({
    resource <- ("legislators?chamber=house&per_page=all")
    response <- GET(paste0(sunlight.base, resource))
    body <- fromJSON(content(response, "text"))
    house <- flatten(body$results) %>% select(state_name)
    return(house)
  })
  
 output$districts <- renderPrint({
    body <- congressmen()
    body$state_name <- as.factor(body$state_name)
    districts <- tally(group_by(body, state_name))
    View(districts)
    })
  
  legislators.click <- reactive({
    click <- input$leaflet_shape_click
    if (is.null(click))
      return()
    resource <- ("legislators/locate?latitude=")
    resource2 <- ("&longitude=")
    longitude <- click$lng
    latitude <- click$lat
    response <- GET(paste0(sunlight.base, resource, latitude, resource2, longitude))
    body <- fromJSON(content(response, "text"))
    legislators <- flatten(body$results) %>% mutate(name = paste(first_name, last_name)) %>% select(name, chamber, party, state, phone, website)
    return(legislators)
  })
  

  resource <- ("legislators?chamber=house&per_page=all")
  response <- GET(paste0(sunlight.base, resource))
  body <- fromJSON(content(response, "text"))
  house <- flatten(body$results) %>% filter(state_name != "American Samoa") %>% 
    filter(state_name != "Northern Mariana Islands") %>% 
    filter(state_name != "Puerto Rico") %>% filter(state_name != "US Virgin Islands") %>% 
    filter(state_name != "District of Columbia") %>% 
    select(state_name) 
  house$state_name <- as.factor(house$state_name)
  districts <- tally(group_by(house, state_name))

  

  
  
  output$clickleg <- renderTable({
     return(legislators.click()) 
  })

  output$info <- renderPrint({
    if (is.null(input$leaflet_click))
      return()
    return(input$leaflet_click)
      })
    
    


  
  
  output$leaflet <- renderLeaflet({
    leaflet(data = state) %>% addTiles() %>%
      addPolygons(fillColor = topo.colors(20, alpha = NULL), stroke= FALSE,
        highlight = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE))
  })
  
  
  output$infoo <- renderPrint({
    return(input$leafletclick)
  })
  
  output$photosclick <- renderUI({
    click <- input$leaflet_shape_click
    if (is.null(click))
      return()
    resource <- ("legislators/locate?latitude=")
    resource2 <- ("&longitude=")
    longitude <- click$lng
    latitude <- click$lat
    response <- GET(paste0(sunlight.base, resource, latitude, resource2, longitude))
    body <- fromJSON(content(response, "text"))
    bio.ids <- flatten(body$results) %>% select(bioguide_id)
    picture.base <- ("https://theunitedstates.io/images/congress/225x275/")
    picture.query <- (".jpg") 
    num <- nrow(bio.ids)
    if (num == 1) {
    picture1 <-paste0(picture.base, bio.ids[1,1], picture.query)
    tags$img(src=picture1)
    } else if (num == 2) {
      picture1 <-paste0(picture.base, bio.ids[1,1], picture.query)
      picture2 <-paste0(picture.base, bio.ids[2,1], picture.query)
      tagList(tags$img(src=picture1), 
              tags$img(src=picture2))
    } else {
    picture1 <-paste0(picture.base, bio.ids[1,1], picture.query)
    picture2 <-paste0(picture.base, bio.ids[2,1], picture.query)
    picture3 <- paste0(picture.base, bio.ids[3,1], picture.query)
    tagList(tags$img(src=picture1), 
            tags$img(src=picture2), 
            tags$img(src=picture3))
    }
  })
  
  output$senate <- renderPrint({
    response <- GET("https://api.propublica.org/congress/v1/115/senate/members.json",
                    add_headers(X-Api-Key : "ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"))
    body <- fromJSON(content(response, "text"))
    return(body)
  })
  
  output$photos <- renderUI({
    resource <- ("legislators/locate")
    query <- paste0("?zip=", input$zip)
    response <- GET(paste0(sunlight.base, resource, query))
    body <- fromJSON(content(response, "text"))
    bio.ids <- flatten(body$results) %>% select(bioguide_id)
    picture.base <- ("https://theunitedstates.io/images/congress/225x275/")
    picture.query <- (".jpg")
    num.reps <- nrow(bio.ids)
    size <- 200
    if (num.reps > 3) {
      size <- 200 - (18*num.reps)
    }
    picture1 <-paste0(picture.base, bio.ids[1,1], picture.query)
    images <- tags$img(src=picture1, width = size)
    if (num.reps > 1) {
      num.reps <- 2:num.reps
      for (val in num.reps) {
        picture <-paste0(picture.base, bio.ids[val,1], picture.query)
        images <- tagAppendChild(images, tags$img(src=picture, width = size))
      }
    }
    return(images)
  })
  
}
