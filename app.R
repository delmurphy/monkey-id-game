library(shiny)

source("global.R")   # optional, if you use global.R
source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)