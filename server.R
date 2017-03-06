library(httr)
library(jsonlite)
library(knitr)
library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)
library(sp)
library(geojsonio)
library(curlconverter)
library(tidyr)
source("apikey.R")

state <- geojson_read("data/stateData.geojson", what = "sp")
class(state)


# Sunlight API base
sunlight.base <- "https://congress.api.sunlightfoundation.com/"
# Propublica API Base
propublica.base <- "https://api.propublica.org/congress/v1/"

house.makeup <- read.csv("data/house.makeup", stringsAsFactors = FALSE)
senate.makeup <- read.csv("data/senate.makeup", stringsAsFactors = FALSE)


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

# Server function
server <- function(input, output) {
  
  
  output$house.area <- renderPlotly({
    h.makeup <- house.makeup
    party <- c("D","I","R")
    h.makeup <- select(h.makeup, -X, -party)
    colnames(h.makeup) <- 102:115
    h.makeup  <- gather(h.makeup , key = Congress,
                        value  = Members,
                        `102`:`115`, convert = TRUE)
    h.makeup  <- data.frame(party, h.makeup)
    plot <- ggplot(h.makeup, aes(x=Congress, y=Members, fill=party)) + 
      geom_area()+
      scale_fill_manual(values = c("#002868", "#6D1FA7", "#BF0A30")) +
      scale_x_continuous(breaks = c(102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115))
    pplot <- ggplotly(plot)
    return(pplot)
  })
  
  output$house.line <- renderPlotly({
  h.makeup <- house.makeup
  party <- c("D","I","R")
  h.makeup <- select(h.makeup, -X, -party)
  colnames(h.makeup) <- 102:115
  h.makeup  <- gather(h.makeup , key = Congress,
                      value  = Members,
                      `102`:`115`, convert = TRUE)
  h.makeup  <- data.frame(party, h.makeup)
  plot <- ggplot(h.makeup, aes(x=Congress, y=Members, color=party)) + 
    geom_line()+
    scale_color_manual(values = c("#002868", "#6D1FA7", "#BF0A30"))
  pplot <- ggplotly(plot)
  return(pplot)
  })
  
  
  output$house.pie <- renderPlot({
    house.makeup$X102 <- round(((house.makeup$X102 / sum(house.makeup$X102))*100), digits = 2)
    house.makeup$X103 <- round(((house.makeup$X103 / sum(house.makeup$X103))*100), digits = 2)
    house.makeup$X104 <- round(((house.makeup$X104 / sum(house.makeup$X104))*100), digits = 2)
    house.makeup$X105 <- round(((house.makeup$X105 / sum(house.makeup$X105))*100), digits = 2)
    house.makeup$X106 <- round(((house.makeup$X106 / sum(house.makeup$X106))*100), digits = 2)
    house.makeup$X107 <- round(((house.makeup$X107 / sum(house.makeup$X107))*100), digits = 2)
    house.makeup$X108 <- round(((house.makeup$X108 / sum(house.makeup$X108))*100), digits = 2)
    house.makeup$X109 <- round(((house.makeup$X109 / sum(house.makeup$X109))*100), digits = 2)
    house.makeup$X110 <- round(((house.makeup$X110 / sum(house.makeup$X110))*100), digits = 2)
    house.makeup$X111 <- round(((house.makeup$X111 / sum(house.makeup$X111))*100), digits = 2)
    house.makeup$X112 <- round(((house.makeup$X112 / sum(house.makeup$X112))*100), digits = 2)
    house.makeup$X113 <- round(((house.makeup$X113 / sum(house.makeup$X113))*100), digits = 2)
    house.makeup$X114 <- round(((house.makeup$X114 / sum(house.makeup$X114))*100), digits = 2)
    house.makeup$X115 <- round(((house.makeup$X115 / sum(house.makeup$X115))*100), digits = 2)
    h.makeup <- house.makeup
    party <- c("D","I","R")
    h.makeup <- select(h.makeup, -X, -party)
    colnames(h.makeup) <- 102:115
    h.makeup  <- gather(h.makeup , key = Congress,
                        value  = Members,
                        `102`:`115`, convert = TRUE)
    h.makeup  <- data.frame(party, h.makeup)
    plot <- ggplot(h.makeup, aes(x=factor(1), y=Members, fill=party)) + 
      geom_bar(width = 1, stat="identity") +
      coord_polar(theta="y")+
      scale_fill_manual(values = c("#002868", "#6D1FA7", "#BF0A30"))+
      facet_wrap(~Congress, nrow = 2)+
      theme(axis.ticks = element_blank())+
      theme(axis.text = element_blank())+
      theme(axis.title = element_blank())+
      theme_void()
      
    return(plot)
  })
  
  output$senate.area <- renderPlotly({
    s.makeup <- senate.makeup
    party <- c("D","I","R")
    s.makeup <- select(s.makeup, -X, -party)
    colnames(s.makeup) <- 80:115
    s.makeup  <- gather(s.makeup , key = Congress,
                        value  = Members,
                        `80`:`115`, convert = TRUE)
    s.makeup  <- data.frame(party, s.makeup)
    plot <- ggplot(s.makeup, aes(x=Congress, y=Members, fill=party)) + 
      geom_area()+
      scale_fill_manual(values = c("#002868", "#6D1FA7", "#BF0A30"))
    pplot <- ggplotly(plot)
    return(pplot)
  })
  
  output$senate.line <- renderPlotly({
    s.makeup <- senate.makeup
    party <- c("D","I","R")
    s.makeup <- select(s.makeup, -X, -party)
    colnames(s.makeup) <- 80:115
    s.makeup  <- gather(s.makeup , key = Congress,
                        value  = Members,
                        `80`:`115`, convert = TRUE)
    s.makeup  <- data.frame(party, s.makeup)
    plot <- ggplot(s.makeup, aes(x=Congress, y=Members, color=party)) + 
      geom_line()+
      scale_color_manual(values = c("#002868", "#6D1FA7", "#BF0A30"))
    pplot <- ggplotly(plot)
    return(pplot)
  })
  
  output$senate.pie <- renderPlot({
    times <- 3:38
    for (val in times) {
      senate.makeup[,val] <- round(((senate.makeup[,val] / sum(senate.makeup[,val]))*100), digits = 2)
    }
    s.makeup <- senate.makeup
    party <- c("D","I","R")
    s.makeup <- select(s.makeup, -X, -party)
    colnames(s.makeup) <- 80:115
    s.makeup  <- gather(s.makeup , key = Congress,
                        value  = Members,
                        `80`:`115`, convert = TRUE)
    s.makeup  <- data.frame(party, s.makeup)
    plot <- ggplot(s.makeup, aes(x=factor(1), y=Members, fill=party)) + 
      geom_bar(width = 1, stat = "identity") +
      coord_polar(theta="y")+
      scale_fill_manual(values = c("#002868", "#6D1FA7", "#BF0A30")) +
      facet_wrap(~Congress, nrow = 3)+
      theme(axis.ticks = element_blank())+
      theme(axis.text = element_blank())+
      theme(axis.title = element_blank())+
      theme_void()
    return(plot)
  })
  
  output$choice <- renderUI({
      textInput('zip', "Zipcode", value = "90210")
  })
  
  legislators <- reactive({
    resource <- "legislators/locate"
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
  

  legislators.click <- reactive({
    click <- input$leaf.let_shape_click
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
    if (is.null(input$leaflet_click))
      return()
    return(input$leaflet_click)
      })
    
    
  output$explanation <- renderUI({
    if (is.null(input$leaflet_click)) 
      return(em("Click on the map to view representatives"))

    tags$em("Below are the representatives that represent this spot.") 
        })
  
output$leaf.let <- renderLeaflet({
    leaflet(data = state, options = leafletOptions(minZoom = 3)) %>% addTiles() %>%
      addPolygons(fillColor = rainbow(50, alpha = NULL), stroke= FALSE,
        highlight = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE)) %>% setView(lng=-115, lat = 52, zoom = 3.4)
  })
    
    
  observe({
    input$reset
    leafletProxy("leaf.let") %>% setView(lng = -115, lat = 52, zoom = 3.4)
  })
  
  

  
  
  
  output$photosclick <- renderUI({
    click <- input$leaf.let_shape_click
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
    if (num.reps > 6) {
      size <- 200 - (4*num.reps)
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
  

  output$house.missed <- renderPlotly({
    cmd <- 'curl "https://api.propublica.org/congress/v1/115/house/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
    parsed_cmd <- straighten(cmd)
    str(parsed_cmd)
    actual_function <- make_req(parsed_cmd)[[1]]
    request.body.list <- content(actual_function())
    members.list <- request.body.list$results[[1]]$members
    names(members.list) <- NULL
    members.json <- toJSON(members.list)
    house <- flatten(fromJSON(members.json, flatten = TRUE)) %>% 
      select(first_name, last_name, party, missed_votes_pct)
    if (input$party == "all") {
      house.members.115 <- house
    } else if (input$party == "Democrat") {
      house.members.115 <- house %>% filter(party == "D")
    } else if (input$party == "Independent") {
      house.members.115 <- house %>% filter(party == "I")
    } else if (input$party == "Republican") {
      house.members.115 <- house %>% filter(party == "R")
    }
    house.members.115 <- house.members.115 %>% mutate(name = paste(first_name, last_name))
    house.members <- house.members.115[!sapply(house.members.115$missed_votes_pct,is.null),]
    house.members$missed_votes_pct <- as.numeric(unlist(house.members$missed_votes_pct))
    house.members$name <- as.factor(unlist(house.members$name))
    house.members$last_name <- as.factor(unlist(house.members$last_name))
    house.members$party <- as.factor(unlist(house.members$party))
    house.members$percent <- (house.members$missed_votes_pct + 0.5)
    house.members$name<- factor(house.members$name, levels = house.members$name[order(house.members$last_name)])
  p <- ggplot(house.members, aes(x = name, y = percent, fill = party)) +
    geom_bar(width = 1, stat = "identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 5))+
    theme(axis.ticks.x = element_blank()) +
    scale_y_continuous(limits = c(0, 100))+
    ggtitle("House of Representatives % of Votes Missed")
  if (input$party == "all") {
    pp <- p + scale_fill_manual(values = c("#002868", "#BF0A30"), labels = c("Democrat", "Republican"))+
      theme(axis.text.x = element.blank())
  } else if (input$party == "Democrat") {
    pp <- p + scale_fill_manual(values = "#002868", labels = "Democrat")
  } else if (input$party == "Republican") {
    pp <- p + scale_fill_manual(values = "#BF0A30", labels = "Republican")
  } else if (input$party == "Independent") {
    pp <- p
  }
    ppp <- ggplotly(pp)
  return(ppp)
  
  })
  
  
  
  output$senate.missed <- renderPlotly({
    cmd <- 'curl "https://api.propublica.org/congress/v1/115/senate/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
    parsed_cmd <- straighten(cmd)
    str(parsed_cmd)
    actual_function <- make_req(parsed_cmd)[[1]]
    request.body.list <- content(actual_function())
    members.list <- request.body.list$results[[1]]$members
    names(members.list) <- NULL
    members.json <- toJSON(members.list)
    senate <- flatten(fromJSON(members.json, flatten = TRUE)) %>% 
      select(first_name, last_name, party, missed_votes_pct)
    if (input$party == "all") {
      senate.members.115 <- senate
    } else if (input$party == "Democrat") {
      senate.members.115 <- senate %>% filter(party == "D")
    } else if (input$party == "Independent") {
      senate.members.115 <- senate %>% filter(party == "I")
    } else if (input$party == "Republican") {
      senate.members.115 <- senate %>% filter(party == "R")
    }
    senate.members.115 <- senate.members.115 %>% mutate(name = paste(first_name, last_name))
    senate.members <- senate.members.115[!sapply(senate.members.115$missed_votes_pct,is.null),]
    senate.members$missed_votes_pct <- as.numeric(unlist(senate.members$missed_votes_pct))
    senate.members$name <- as.factor(unlist(senate.members$name))
    senate.members$last_name <- as.factor(unlist(senate.members$last_name))
    senate.members$party <- as.factor(unlist(senate.members$party))
    senate.members$percent <- (senate.members$missed_votes_pct + 0.2)
    senate.members$name<- factor(senate.members$name, levels = senate.members$name[order(senate.members$last_name)])
   
    p <- ggplot(senate.members, aes(x = name, y = percent, fill = party)) +
      geom_bar(width = 1, stat = "identity") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
      theme(axis.ticks.x = element_blank()) +
      scale_y_continuous(limits = c(0, 100))+
      ggtitle("Senate % of Votes Missed")
    if (input$party == "all") {
      pp <- p + scale_fill_manual(values = c("#002868", "#6D1FA7", "#BF0A30"), labels = c("Democrat", "Independent", "Republican"))
    } else if (input$party == "Democrat") {
      pp <- p + scale_fill_manual(values = "#002868", labels = "Democrat")
    } else if (input$party == "Republican") {
      pp <- p + scale_fill_manual(values = "#BF0A30", labels = "Republican")
    } else if (input$party == "Independent") {
      pp <- p + scale_fill_manual(values = "#6D1FA7", labels = "Independent")
    }    
    ppp <- ggplotly(pp)
    return(ppp)
    
  })

  output$house.with <- renderPlotly({
    cmd <- 'curl "https://api.propublica.org/congress/v1/115/house/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
    parsed_cmd <- straighten(cmd)
    str(parsed_cmd)
    actual_function <- make_req(parsed_cmd)[[1]]
    request.body.list <- content(actual_function())
    members.list <- request.body.list$results[[1]]$members
    names(members.list) <- NULL
    members.json <- toJSON(members.list)
    house <- flatten(fromJSON(members.json, flatten = TRUE)) %>% 
      select(first_name, last_name, party, votes_with_party_pct)
    if (input$party == "all") {
      house.members.115 <- house
    } else if (input$party == "Democrat") {
      house.members.115 <- house %>% filter(party == "D")
    } else if (input$party == "Independent") {
      house.members.115 <- house %>% filter(party == "I")
    } else if (input$party == "Republican") {
      house.members.115 <- house %>% filter(party == "R")
    }
    house.members.115 <- house.members.115 %>% mutate(name = paste(first_name, last_name))
    house.members <- house.members.115[!sapply(house.members.115$votes_with_party_pct,is.null),]
    house.members$percent <- as.numeric(unlist(house.members$votes_with_party_pct))
    house.members$name <- as.factor(unlist(house.members$name))
    house.members$last_name <- as.factor(unlist(house.members$last_name))
    house.members$party <- as.factor(unlist(house.members$party))
    house.members$name<- factor(house.members$name, levels = house.members$name[order(house.members$percent, 
                                                                                      decreasing = TRUE)])
    p <- ggplot(house.members, aes(x = name, y = percent, fill = party)) +
      geom_bar(width = 1, stat = "identity") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 5))+
      theme(axis.ticks.x = element_blank()) +
      scale_y_continuous(limits = c(0, 100))+
      ggtitle("House of Representatives Votes With Party %")
    if (input$party == "all") {
      pp <- p + scale_fill_manual(values = c("#002868", "#BF0A30"), labels = c("Democrat", "Republican"))+
        theme(axis.text.x = element_blank())
    } else if (input$party == "Democrat") {
      pp <- p + scale_fill_manual(values = "#002868", labels = "Democrat")
    } else if (input$party == "Republican") {
      pp <- p + scale_fill_manual(values = "#BF0A30", labels = "Republican")
    }else if (input$party == "Independent") {
      pp <- p
    }
    ppp <- ggplotly(pp)
    return(ppp)
    
  })
  
  
  
  output$senate.with <- renderPlotly({
    cmd <- 'curl "https://api.propublica.org/congress/v1/115/senate/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
    parsed_cmd <- straighten(cmd)
    str(parsed_cmd)
    actual_function <- make_req(parsed_cmd)[[1]]
    request.body.list <- content(actual_function())
    members.list <- request.body.list$results[[1]]$members
    names(members.list) <- NULL
    members.json <- toJSON(members.list)
    senate <- flatten(fromJSON(members.json, flatten = TRUE)) %>% 
      select(first_name, last_name, party, votes_with_party_pct)
    if (input$party == "all") {
      senate.members.115 <- senate
    } else if (input$party == "Democrat") {
      senate.members.115 <- senate %>% filter(party == "D")
    } else if (input$party == "Independent") {
      senate.members.115 <- senate %>% filter(party == "I")
    } else if (input$party == "Republican") {
      senate.members.115 <- senate %>% filter(party == "R")
    }
    senate.members.115 <- senate.members.115 %>% mutate(name = paste(first_name, last_name))
    senate.members <- senate.members.115[!sapply(senate.members.115$votes_with_party_pct,is.null),]
    senate.members$percent <- as.numeric(unlist(senate.members$votes_with_party_pct))
    senate.members$name <- as.factor(unlist(senate.members$name))
    senate.members$last_name <- as.factor(unlist(senate.members$last_name))
    senate.members$party <- as.factor(unlist(senate.members$party))
    senate.members$name<- factor(senate.members$name, levels = senate.members$name[order(senate.members$percent, decreasing = TRUE)])
    
    p <- ggplot(senate.members, aes(x = name, y = percent, fill = party)) +
      geom_bar(width = 1, stat = "identity") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
      theme(axis.ticks.x = element_blank()) +
      scale_y_continuous(limits = c(0, 100))+
      ggtitle("Senate Votes With Party %")
    if (input$party == "all") {
      pp <- p + scale_fill_manual(values = c("#002868", "#6D1FA7", "#BF0A30"), labels = c("Democrat", "Independent", "Republican"))    } else if (input$party == "Democrat") {
      pp <- p + scale_fill_manual(values = "#002868", labels = "Democrat")
    } else if (input$party == "Republican") {
      pp <- p + scale_fill_manual(values = "#BF0A30", labels = "Republican")
    } else if (input$party == "Independent") {
      pp <- p + scale_fill_manual(values = "#6D1FA7", labels = "Independent")
    }    
    ppp <- ggplotly(pp)
    return(ppp)
  })
  
  output$house.missed.114 <- renderPlotly({
    cmd <- 'curl "https://api.propublica.org/congress/v1/114/house/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
    parsed_cmd <- straighten(cmd)
    str(parsed_cmd)
    actual_function <- make_req(parsed_cmd)[[1]]
    request.body.list <- content(actual_function())
    members.list <- request.body.list$results[[1]]$members
    names(members.list) <- NULL
    members.json <- toJSON(members.list)
    house <- flatten(fromJSON(members.json, flatten = TRUE)) %>% 
      select(first_name, last_name, party, missed_votes_pct)
    if (input$party == "all") {
      house.members.115 <- house
    } else if (input$party == "Democrat") {
      house.members.115 <- house %>% filter(party == "D")
    } else if (input$party == "Independent") {
      house.members.115 <- house %>% filter(party == "I")
    } else if (input$party == "Republican") {
      house.members.115 <- house %>% filter(party == "R")
    }
    house.members.115 <- house.members.115 %>% mutate(name = paste(first_name, last_name))
    house.members <- house.members.115[!sapply(house.members.115$missed_votes_pct,is.null),]
    house.members$missed_votes_pct <- as.numeric(unlist(house.members$missed_votes_pct))
    house.members$name <- as.factor(unlist(house.members$name))
    house.members$last_name <- as.factor(unlist(house.members$last_name))
    house.members$party <- as.factor(unlist(house.members$party))
    house.members$percent <- (house.members$missed_votes_pct + 0.5)
    if (input$order == "alphabetically") {
      house.members$name<- factor(house.members$name, levels = house.members$name[order(house.members$last_name)])
    } else if (input$order == "increasing") {
      house.members$name<- factor(house.members$name, levels = house.members$name[order(house.members$percent)])
    } else if (input$order == "decreasing") {
      house.members$name<- factor(house.members$name, levels = house.members$name[order(house.members$percent, 
                                                                                        decreasing = TRUE)])
    }
    p <- ggplot(house.members, aes(x = name, y = percent, fill = party)) +
      geom_bar(width = 1, stat = "identity") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 5))+
      theme(axis.ticks.x = element_blank()) +
      scale_y_continuous(limits = c(0, 100))+
      ggtitle("House of Representatives % of Votes Missed")
    if (input$party == "all") {
      pp <- p + scale_fill_manual(values = c("#002868", "#BF0A30"), labels = c("Democrat", "Republican"))+
        theme(axis.text.x = element_blank())
    } else if (input$party == "Democrat") {
      pp <- p + scale_fill_manual(values = "#002868", labels = "Democrat")
    } else if (input$party == "Republican") {
      pp <- p + scale_fill_manual(values = "#BF0A30", labels = "Republican")
    } else if (input$party == "Independent") {
      pp <- p
    }
    ppp <- ggplotly(pp)
    return(ppp)
  })
  
  
  
  output$senate.missed.114 <- renderPlotly({
    cmd <- 'curl "https://api.propublica.org/congress/v1/114/senate/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
    parsed_cmd <- straighten(cmd)
    str(parsed_cmd)
    actual_function <- make_req(parsed_cmd)[[1]]
    request.body.list <- content(actual_function())
    members.list <- request.body.list$results[[1]]$members
    names(members.list) <- NULL
    members.json <- toJSON(members.list)
    senate <- flatten(fromJSON(members.json, flatten = TRUE)) %>% 
      select(first_name, last_name, party, missed_votes_pct)
    if (input$party == "all") {
      senate.members.115 <- senate
    } else if (input$party == "Democrat") {
      senate.members.115 <- senate %>% filter(party == "D")
    } else if (input$party == "Independent") {
      senate.members.115 <- senate %>% filter(party == "I")
    } else if (input$party == "Republican") {
      senate.members.115 <- senate %>% filter(party == "R")
    }
    senate.members.115 <- senate.members.115 %>% mutate(name = paste(first_name, last_name))
    senate.members <- senate.members.115[!sapply(senate.members.115$missed_votes_pct,is.null),]
    senate.members$missed_votes_pct <- as.numeric(unlist(senate.members$missed_votes_pct))
    senate.members$name <- as.factor(unlist(senate.members$name))
    senate.members$last_name <- as.factor(unlist(senate.members$last_name))
    senate.members$party <- as.factor(unlist(senate.members$party))
    senate.members$percent <- (senate.members$missed_votes_pct + 0.2)
    if (input$order == "alphabetically") {
      senate.members$name<- factor(senate.members$name, levels = senate.members$name[order(senate.members$last_name)])
    } else if (input$order == "increasing") {
      senate.members$name<- factor(senate.members$name, levels = senate.members$name[order(senate.members$percent)])
    } else if (input$order == "decreasing") {
      senate.members$name<- factor(senate.members$name, levels = senate.members$name[order(senate.members$percent, 
                                                                                           decreasing = TRUE)])
    }    
    p <- ggplot(senate.members, aes(x = name, y = percent, fill = party)) +
      geom_bar(width = 1, stat = "identity") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
      theme(axis.ticks.x = element_blank()) +
      scale_y_continuous(limits = c(0, 100))+
      ggtitle("Senate % of Votes Missed")
    if (input$party == "all") {
      pp <- p + scale_fill_manual(values = c("#002868", "#6D1FA7", "#BF0A30"), labels = c("Democrat", "Independent", "Republican"))
    } else if (input$party == "Democrat") {
      pp <- p + scale_fill_manual(values = "#002868", labels = "Democrat")
    } else if (input$party == "Republican") {
      pp <- p + scale_fill_manual(values = "#BF0A30", labels = "Republican")
    } else if (input$party == "Independent") {
      pp <- p + scale_fill_manual(values = "#6D1FA7", labels = "Independent")
    }   else if (input$party == "Independent") {
      pp <- p
    }
    ppp <- ggplotly(pp)
    return(ppp)
    
  })
  
  output$house.with.114 <- renderPlotly({
    cmd <- 'curl "https://api.propublica.org/congress/v1/114/house/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
    parsed_cmd <- straighten(cmd)
    str(parsed_cmd)
    actual_function <- make_req(parsed_cmd)[[1]]
    request.body.list <- content(actual_function())
    members.list <- request.body.list$results[[1]]$members
    names(members.list) <- NULL
    members.json <- toJSON(members.list)
    house <- flatten(fromJSON(members.json, flatten = TRUE)) %>% 
      select(first_name, last_name, party, votes_with_party_pct)
    if (input$party == "all") {
      house.members.115 <- house
    } else if (input$party == "Democrat") {
      house.members.115 <- house %>% filter(party == "D")
    } else if (input$party == "Independent") {
      house.members.115 <- house %>% filter(party == "I")
    } else if (input$party == "Republican") {
      house.members.115 <- house %>% filter(party == "R")
    }
    house.members.115 <- house.members.115 %>% mutate(name = paste(first_name, last_name))
    house.members <- house.members.115[!sapply(house.members.115$votes_with_party_pct,is.null),]
    house.members$percent <- as.numeric(unlist(house.members$votes_with_party_pct))
    house.members$name <- as.factor(unlist(house.members$name))
    house.members$last_name <- as.factor(unlist(house.members$last_name))
    house.members$party <- as.factor(unlist(house.members$party))
    if (input$order == "alphabetically") {
    house.members$name<- factor(house.members$name, levels = house.members$name[order(house.members$last_name)])
    } else if (input$order == "increasing") {
    house.members$name<- factor(house.members$name, levels = house.members$name[order(house.members$percent)])
    } else if (input$order == "decreasing") {
      house.members$name<- factor(house.members$name, levels = house.members$name[order(house.members$percent, 
                                                                                        decreasing = TRUE)])
    }
    p <- ggplot(house.members, aes(x = name, y = percent, fill = party)) +
      geom_bar(width = 1, stat = "identity") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 5))+
      theme(axis.ticks.x = element_blank()) +
      ggtitle("House of Representatives Votes With Party %")
    if (input$party == "all") {
      pp <- p + scale_fill_manual(values = c("#002868", "#BF0A30"), labels = c("Democrat", "Republican"))+
        theme(axis.text.x = element_blank())
    } else if (input$party == "Democrat") {
      pp <- p + scale_fill_manual(values = "#002868", labels = "Democrat")
    } else if (input$party == "Republican") {
      pp <- p + scale_fill_manual(values = "#BF0A30", labels = "Republican")
    } else if (input$party == "Independent") {
      pp <- p
    }
    ppp <- ggplotly(pp)
    return(ppp)
    
  })
  
  
  
  output$senate.with.114 <- renderPlotly({
    cmd <- 'curl "https://api.propublica.org/congress/v1/114/senate/members.json" -H "X-API-Key: ApPfi2HAhD1AurYPyWXqU42XvSudAwVC3sQqvuYT"'
    parsed_cmd <- straighten(cmd)
    str(parsed_cmd)
    actual_function <- make_req(parsed_cmd)[[1]]
    request.body.list <- content(actual_function())
    members.list <- request.body.list$results[[1]]$members
    names(members.list) <- NULL
    members.json <- toJSON(members.list)
    senate <- flatten(fromJSON(members.json, flatten = TRUE)) %>% 
      select(first_name, last_name, party, votes_with_party_pct)
    if (input$party == "all") {
      senate.members.115 <- senate
    } else if (input$party == "Democrat") {
      senate.members.115 <- senate %>% filter(party == "D")
    } else if (input$party == "Independent") {
      senate.members.115 <- senate %>% filter(party == "I")
    } else if (input$party == "Republican") {
      senate.members.115 <- senate %>% filter(party == "R")
    }
    senate.members.115 <- senate.members.115 %>% mutate(name = paste(first_name, last_name))
    senate.members <- senate.members.115[!sapply(senate.members.115$votes_with_party_pct,is.null),]
    senate.members$percent <- as.numeric(unlist(senate.members$votes_with_party_pct))
    senate.members$name <- as.factor(unlist(senate.members$name))
    senate.members$last_name <- as.factor(unlist(senate.members$last_name))
    senate.members$party <- as.factor(unlist(senate.members$party))
    senate.members$name<- factor(senate.members$name, levels = senate.members$name[order(senate.members$last_name)])
    
    if (input$order == "alphabetically") {
      senate.members$name<- factor(senate.members$name, levels = senate.members$name[order(senate.members$last_name)])
    } else if (input$order == "increasing") {
      senate.members$name<- factor(senate.members$name, levels = senate.members$name[order(senate.members$percent)])
    } else if (input$order == "decreasing") {
      senate.members$name<- factor(senate.members$name, levels = senate.members$name[order(senate.members$percent, 
                                                                                           decreasing = TRUE)])
    }
    p <- ggplot(senate.members, aes(x = name, y = percent, fill = party)) +
      geom_bar(width = 1, stat = "identity") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
      theme(axis.ticks.x = element_blank()) +
      scale_y_continuous(limits = c(0, 100))+
      ggtitle("Senate Votes With Party %")
    if (input$party == "all") {
      pp <- p + scale_fill_manual(values = c("#002868", "#6D1FA7", "#BF0A30"), labels = c("Democrat", "Independent", "Republican"))
    } else if (input$party == "Democrat") {
      pp <- p + scale_fill_manual(values = "#002868", labels = "Democrat")
    } else if (input$party == "Republican") {
      pp <- p + scale_fill_manual(values = "#BF0A30", labels = "Republican")
    } else if (input$party == "Independent") {
      pp <- p + scale_fill_manual(values = "#6D1FA7", labels = "Independent")
    }    
    ppp <- ggplotly(pp)
    return(ppp)
  })
  
  
   
}

