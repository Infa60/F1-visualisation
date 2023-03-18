library(shiny)
library(leaflet)
library(tidyverse)

circuits <- read.csv("data/circuits.csv", sep = ",")
constructor_results <- read.csv("data/constructor_results.csv")
constructors <- read.csv("data/constructors.csv")


iconF1 <- makeIcon("assets/F1logo2.png", iconWidth = 70, iconHeight = 25)

link <- function(url, location, name, country) {
  content <- paste(sep = "",
                   "<b><a href='", as.character(url),"'>", as.character(name),"</a>",
                   "<br/>Ville : ", as.character(location),
                   "<br/>Pays : ", as.character(country), "</b>",
                   "<br/>",actionButton("selectBtnMap", label = "Sélectionner", onClick = 'Shiny.onInputChange("button_click",  Math.random())')
  )
}


