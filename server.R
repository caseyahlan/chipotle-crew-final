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
    picture1 <-paste0(picture.base, bio.ids[1,1], picture.query)
    tags$img(src=picture1)
    if (num.reps > 1) {
      num.reps <- c(2:num.reps)
    }
   # for (val in num.reps) {
   #   picture <-paste0(picture.base, bio.ids[val,1], picture.query)
   #   tags[[length(lst)+1]] <- tags$img(src=picture1)
    #}
    return(tags)
  })
  
}