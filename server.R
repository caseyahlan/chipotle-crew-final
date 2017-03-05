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



server <- function(input, output) {
  
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
  
#  markers.on("click", function (event) {
 #   // Assuming the clicked feature is a shape, not a point marker.
  #  map.fitBounds(event.layer.getBounds());
#  });
  

  
  
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
    response <- GET(paste0(base, resource, query))
    body <- fromJSON(content(response, "text"))
    bio.ids <- flatten(body$results) %>% select(bioguide_id)
    picture.base <- ("https://theunitedstates.io/images/congress/225x275/")
    picture.query <- (".jpg")
    num.reps <- 2:nrow(bio.ids)
    picture1 <-paste0(picture.base, bio.ids[1,1], picture.query)
    images <- tags$img(src=picture1)
    for (val in num.reps) {
      picture <-paste0(picture.base, bio.ids[val,1], picture.query)
      images <- tagAppendChild(images, tags$img(src=picture))
    }
    return(images)
  })
  
}