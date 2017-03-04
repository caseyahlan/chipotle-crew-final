library("shiny")
source("ui.R")
source("server.R")

shinyApp(ui = shinyUI(ui), server = shinyServer(server))