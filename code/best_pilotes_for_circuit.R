library(tidyverse)
library(leaflet)
library(reshape2)
library(lubridate)

source("code/best_lap_circuit.R")


getBestPilotesForCircuit <- function(id) {
  results <- read.csv("data/results.csv")
  
  
  id_races_for_circuit <- races |> 
    filter(circuitId == id) |>
    select(raceId)
  
  results_for_races <- results |>
    filter(raceId %in% id_races_for_circuit$raceId) |>
    select(raceId, points, driverId)
  
  
  total_points <- results_for_races |> 
    group_by(driverId) |>
    summarise(points = sum(points)) |> 
    arrange(-points)
    
  
  total_points <- total_points |>
    right_join(drivers) |>
    select(points, forename, surname, url)
  
  return(total_points[1:10,])
}