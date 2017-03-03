library("shiny")
#setwd("~/INFO201/chipotle-crew-final")
source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)
