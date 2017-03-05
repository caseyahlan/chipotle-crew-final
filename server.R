library(httr)
library(jsonlite)
library(knitr)
library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)
library(maps)
library(fiftystater)

base <- ("https://congress.api.sunlightfoundation.com/")

server <- function(input, output) {
  legislators <- reactive({
    resource <- ("legislators/locate")
    query <- paste0("?zip=", input$zip)
    response <- GET(paste0(base, resource, query))
    body <- fromJSON(content(response, "text"))
    legislators <- flatten(body$results) %>% mutate(name = paste(first_name, last_name)) %>% select(name, chamber, party, state, phone, website)
    return(legislators)
  })
  
  output$reps <- renderTable({
    return(legislators())
  })
  
  output$map <- renderPlot({
    ggplot(data = fifty_states) +
      geom_polygon(aes(x = long, y = lat, group = group, fill = id)) +
      coord_quickmap() + 
      guides(fill = FALSE) +
      theme(axis.text = element_blank()) +
      theme(axis.ticks = element_blank()) +
      theme(axis.title = element_blank())
  })
  
  # Handle clicks on the plot
  output$info <- renderPrint({
    return(input$my_click_key)    
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