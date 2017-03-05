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



base <- ("https://congress.api.sunlightfoundation.com/")
source("apikey.R")
View(usa)
# Sunlight API base
sunlight.base <- ("https://congress.api.sunlightfoundation.com/")
# Propublica API Base
propublica.base <- ("https://api.propublica.org/congress/v1/")

usa <- data("fifty_states")

usa.cities <- world.cities %>% filter(country.etc=="USA") %>% filter(pop >= 100000 && capital == 0)
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
usa <- usa %>% mutate(longitude = round(long, digits = 2)) 
usa <- usa %>% mutate(latitude = round(lat, digits = 2))
View(usa)
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
  
  legislators.click <- reactive({
    resource <- ("legislators/locate?latitude=")
    resource2 <- ("&longitude=")
    longitude <- input$my.click$x
    latitude <- input$my.click$y
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
  ggplot(data = usa) +
      geom_polygon(aes(x = long, y = lat, group = group, fill = id)) +
      coord_map(xlim=c(-180, -60)) + 
      guides(fill = FALSE) +
      labs(x="longitude", y="latitude")+
      geom_point(data=usa.cities, aes(x=long, y = lat))+
      scale_y_continuous(breaks=c(20, 30, 40, 50, 60, 70), labels = c(23, 28.9, 34, 41.9, 50, 62)) 
  })
  

  
  output$photosclick <- renderUI({
    resource <- ("legislators/locate?latitude=")
    resource2 <- ("&longitude=")
    longitude <- round(input$my.click$x, digits = 2)
    latitude <- round(input$my.click$y, digits = 2)
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
  

  
  output$photos <- renderUI({
    resource <- ("legislators/locate")
    query <- paste0("?zip=", input$zip)
    response <- GET(paste0(base, resource, query))
    body <- fromJSON(content(response, "text"))
    bio.ids <- flatten(body$results) %>% select(bioguide_id)
    picture.base <- ("https://theunitedstates.io/images/congress/225x275/")
    picture.query <- (".jpg")
    num.reps <- nrow(bio.ids)
    if (num.reps ==1 ) {
      picture1 <-paste0(picture.base, bio.ids[1,1], picture.query)
      tags$img(src=picture1)
    } else if (num.reps == 2) {
      picture1 <-paste0(picture.base, bio.ids[1,1], picture.query)
      picture2 <-paste0(picture.base, bio.ids[2,1], picture.query)
      tagList(tags$img(src=picture1), 
              tags$img(src=picture2))
    } else if (num.reps == 3) {
      picture1 <-paste0(picture.base, bio.ids[1,1], picture.query)
      picture2 <-paste0(picture.base, bio.ids[2,1], picture.query)
      picture3 <- paste0(picture.base, bio.ids[3,1], picture.query)
      tagList(tags$img(src=picture1), 
              tags$img(src=picture2), 
              tags$img(src=picture3)) }
    else if (num.reps == 4) {
      picture1 <-paste0(picture.base, bio.ids[1,1], picture.query)
      picture2 <-paste0(picture.base, bio.ids[2,1], picture.query)
      picture3 <- paste0(picture.base, bio.ids[3,1], picture.query)
      picture4 <- paste0(picture.base, bio.ids[4,1], picture.query)
      tagList(tags$img(src=picture1, width = 187.5),
              tags$img(src=picture2, width = 187.5),
              tags$img(src=picture3, width = 187.5), 
              tags$img(src=picture4, width = 187.5)) 
    } else if (num.reps == 5) {
      picture1 <-paste0(picture.base, bio.ids[1,1], picture.query)
      picture2 <-paste0(picture.base, bio.ids[2,1], picture.query)
      picture3 <- paste0(picture.base, bio.ids[3,1], picture.query)
      picture4 <- paste0(picture.base, bio.ids[4,1], picture.query)
      picture5 <- paste0(picture.base, bio.ids[5,1], picture.query)
      tagList(tags$img(src=picture1, width=150), 
              tags$img(src=picture2, width=150), 
              tags$img(src=picture3, width=150), 
              tags$img(src=picture4, width=150), 
              tags$img(src=picture5, width=150)) 
    } else if (num.reps == 6) {
      picture1 <-paste0(picture.base, bio.ids[1,1], picture.query)
      picture2 <-paste0(picture.base, bio.ids[2,1], picture.query)
      picture3 <- paste0(picture.base, bio.ids[3,1], picture.query)
      picture4 <- paste0(picture.base, bio.ids[4,1], picture.query)
      picture5 <- paste0(picture.base, bio.ids[5,1], picture.query)
      picture6 <- paste0(picture.base, bio.ids[6,1], picture.query)
      tagList(tags$img(src=picture1, width=125), 
              tags$img(src=picture2, width=125),
              tags$img(src=picture3, width=125), 
              tags$img(src=picture4, width=125), 
              tags$img(src=picture5, width=125), 
              tags$img(src=picture6, width=125))
    } else if (num.reps == 7) {
      picture1 <-paste0(picture.base, bio.ids[1,1], picture.query)
      picture2 <-paste0(picture.base, bio.ids[2,1], picture.query)
      picture3 <- paste0(picture.base, bio.ids[3,1], picture.query)
      picture4 <- paste0(picture.base, bio.ids[4,1], picture.query)
      picture5 <- paste0(picture.base, bio.ids[5,1], picture.query)
      picture6 <- paste0(picture.base, bio.ids[6,1], picture.query)
      picture7 <- paste0(picture.base, bio.ids[7,1], picture.query)
      tagList(tags$img(src=picture1, width=107),
              tags$img(src=picture2, width=107),
              tags$img(src=picture3, width=107),
              tags$img(src=picture4, width=107),
              tags$img(src=picture5, width=107), 
              tags$img(src=picture6, width=107), 
              tags$img(src=picture7, width=107))
    }
    
  })
  
  
}