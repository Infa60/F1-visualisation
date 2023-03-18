library(shiny)
library(leaflet)
library(tidyverse)
library(fresh)

# Define UI for application that draws a histogram
source("ui.R")
source("server.R")

# Run the application 
shinyApp(ui = ui, server = server)
