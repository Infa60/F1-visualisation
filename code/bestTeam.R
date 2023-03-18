library(tidyverse)

source("code/best_lap_circuit.R")



getBestTeams <- function(id) {

  constructors <- read.csv("data/constructors.csv")
      
  id_races_for_circuit <- races |> 
    filter(circuitId == id) |>
    select(raceId)
  
  constructor_results_for_races <- constructor_results |>
    filter(raceId %in% id_races_for_circuit$raceId) |>
    select(raceId, points, constructorId)
  
  totalPointsConstructor <- constructor_results_for_races |>
    group_by(constructorId) |>
    summarise(points = sum(points)) |>
    arrange(-points)
  
  totalPointsConstructor <- totalPointsConstructor |>
    right_join(constructors) |>
    select(points, name, nationality, url)
 
  return(totalPointsConstructor[1:10,])
  }
