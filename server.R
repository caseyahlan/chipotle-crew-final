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
source("apikey.R")
state <- geojson_read("stateData.geojson", what = "sp")

class(state)


# Sunlight API base
sunlight.base <- "https://congress.api.sunlightfoundation.com/"
# Propublica API Base
propublica.base <- "https://api.propublica.org/congress/v1/"


# Map data
usa <- data("fifty_states")
data("us.cities")
map <- map_data("world")
hawaii <- read.csv("hawaii.csv", stringsAsFactors = FALSE)
alaska <- read.csv("alaska.csv", stringsAsFactors = FALSE)
forty8states <- fifty_states %>% filter(id != "hawaii") %>% filter(id !="alaska") %>% select(long, lat, id, group)
# hawaii.world <- map %>% filter(subregion== "Hawaii")
# write.csv(hawaii.world, "hawaii.csv")
# alaska.world <- map %>% filter(subregion == "Alaska")
# write.csv(alaska.world, "alaska.csv")
alaska <- read.csv("alaska.csv", stringsAsFactors = FALSE)
hawaii <- read.csv("hawaii.csv", stringsAsFactors = FALSE)
usa <- rbind(forty8states, alaska, hawaii)


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
colnames(legislators.by.gender) <- c("Gender", 1:9)



# Server function
server <- function(input, output) {
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
  
  output$clickleg <- renderTable({
     return(legislators.click()) 
  })

  output$info <- renderPrint({
    return(input$my.click)
  })
    
    
  output$map <- renderPlot({
  ggplot(data = forty8states) +
      geom_polygon(aes(x = long, y = lat, group = group, fill = id)) +
      coord_map(xlim=c(-130, -60), ylim=c(20,50)) + 
      guides(fill = FALSE) +
      labs(x="longitude", y="latitude")+
      geom_point(data=us.cities, aes(x=long, y = lat))
    })

  output$alaska <- renderPlot({
    ggplot(data=alaska) +
      geom_polygon(aes(x=long, y = lat, group = group)) +
      coord_map(xlim=c(-180, -130), ylim= c(50, 74)) 
  })
  
  output$hawaii <- renderPlot({
    ggplot(data=hawaii) +
      geom_polygon(aes(x=long, y = lat, group = group))+
      coord_map()
  })
  
  output$leaflet <- renderLeaflet({
    leaflet(data = state) %>% addTiles() %>%
      addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE,   highlight = highlightOptions(
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
    picture1 <-paste0(picture.base, bio.ids[1,1], picture.query)
    picture2 <-paste0(picture.base, bio.ids[2,1], picture.query)
    picture3 <- paste0(picture.base, bio.ids[3,1], picture.query)
    tagList(tags$img(src=picture1), 
            tags$img(src=picture2), 
            tags$img(src=picture3))
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
    picture1 <-paste0(picture.base, bio.ids[1,1], picture.query)
    images <- tags$img(src=picture1)
    if (num.reps > 1) {
      num.reps <- 2:num.reps
      for (val in num.reps) {
        picture <-paste0(picture.base, bio.ids[val,1], picture.query)
        images <- tagAppendChild(images, tags$img(src=picture))
      }
    }
    return(images)
  })
  
}
