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

state <- geojson_read("stateData.geojson", what = "sp")

class(state)



base <- ("https://congress.api.sunlightfoundation.com/")
source("apikey.R")


# Sunlight API base
sunlight.base <- ("https://congress.api.sunlightfoundation.com/")
# Propublica API Base
propublica.base <- ("https://api.propublica.org/congress/v1/")


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

GetPartyMakeup <- function(roll.id) {
  base <- ("http://congress.api.sunlightfoundation.com/")
  votes.resource <- ("votes?roll_id=")
  votes.filters <- ("&fields=voters")
  votes.response <- GET(paste0(base, votes.resource, roll.id, votes.filters))
  request.body.as.list <- content(votes.response)
  voters.list <- request.body.as.list$results[[1]]$voters
  names(voters.list) <- NULL
  voters.json <- toJSON(voters.list)
  voters.as.data.frame <- flatten(fromJSON(voters.json, flatten=TRUE))
  voters <- voters.as.data.frame %>% select(voter.party, voter.gender, vote)
  voters$voter.party <- as.factor(unlist(voters$voter.party))
  voters.party <- tally(group_by(voters, voter.party))
  return(voters.party)
}

x1 <- GetPartyMakeup("s1-2009")
colnames(x1)[2] <- "1"
x2 <- GetPartyMakeup("s215-2009") %>% select(n)
colnames(x2)[1] <- "2"
x3 <- GetPartyMakeup("s397-2009") %>% select(n)
colnames(x3)[1] <- "3"
x4 <- GetPartyMakeup("s1-2010") %>% select(n)
colnames(x4)[1] <- "4"
x5 <- GetPartyMakeup("s105-2010") %>% select(n)
colnames(x5)[1] <- "5"
x6 <- GetPartyMakeup("s299-2010") %>% select(n)
colnames(x6)[1] <- "6"
x7 <- GetPartyMakeup("s1-2011") %>% select(n)
colnames(x7)[1] <- "7"
x8 <- GetPartyMakeup("s105-2011") %>% select(n)
colnames(x8)[1] <- "8"
x9 <- GetPartyMakeup("s235-2011") %>% select(n)
colnames(x9)[1] <- "9"
x10 <- GetPartyMakeup("s1-2012") %>% select(n)
colnames(x10)[1] <- "10"
x11 <- GetPartyMakeup("s173-2012") %>% select(n)
colnames(x11)[1] <- "11"
x12 <- GetPartyMakeup("s250-2012") %>% select(n)
colnames(x12)[1] <- "12"
x13 <- GetPartyMakeup("s1-2013") %>% select(n)
colnames(x13)[1] <- "13"
x14 <- GetPartyMakeup("s169-2013") %>% select(n)
colnames(x14)[1] <- "14"
x15 <- GetPartyMakeup("s291-2013") %>% select(n)
colnames(x15)[1] <- "15"
x16 <- GetPartyMakeup("s1-2014") %>% select(n)
colnames(x16)[1] <- "16"
x17 <- GetPartyMakeup("s217-2014") %>% select(n)
colnames(x17)[1] <- "17"
x18 <- GetPartyMakeup("s366-2014") %>% select(n)
colnames(x18)[1] <- "18"
x19 <- GetPartyMakeup("s1-2015") %>% select(n)
colnames(x19)[1] <- "19"
x20 <- GetPartyMakeup("s221-2015") %>% select(n)
colnames(x20)[1] <- "20"
x21 <- GetPartyMakeup("s339-2015") %>% select(n)
colnames(x21)[1] <- "21"
x22 <- GetPartyMakeup("s1-2016") %>% select(n)
colnames(x22)[1] <- "22"
x23 <- GetPartyMakeup("s91-2016") %>% select(n)
colnames(x23)[1] <- "23"
x24 <- GetPartyMakeup("s163-2016") %>% select(n)
colnames(x24)[1] <- "24"
x25 <- GetPartyMakeup("s1-2017") %>% select(n)
colnames(x25)[1] <- "25"
x26 <- GetPartyMakeup("s80-2017") %>% select(n)
colnames(x26)[1] <- "26"
x <- cbind(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20,
           x21, x22, x23, x24, x25, x26)
voter.party <- c("D", "I","R")
x <- select(x, -voter.party)
x <- gather(x, key = Year,
            value  = Members,
            1:26, convert = TRUE)
x <- data.frame(voter.party,x)
#ggplot(x, aes(x=Year, y=Members, fill=voter.party)) + 
#  geom_area()

server <- function(input, output) {
  output$choice <- renderUI({
      textInput('zip', "Zipcode", value = "90210")
  })
  
  
  
  legislators <- reactive({
    resource <- ("legislators/locate")
    query <- paste0("?zip=", input$zip)
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
    response <- GET(paste0(base, resource, query))
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