library(tidyverse)
library(lubridate)

source("code/best_lap_circuit.R")


getBestTimes <- function(id) {
  
  races <- read.csv("data/races.csv")
  
  id_races_for_circuit <- races |> 
    filter(circuitId == id) |>
    select(raceId)
 
  lap_times_for_circuit <- lap_times |> 
    filter(raceId %in% id_races_for_circuit$raceId)|>
    select(driverId, milliseconds, raceId, time) |>
    arrange(milliseconds)
  
  lap_times_for_circuit <- lap_times_for_circuit |>
    right_join(drivers) |>
    right_join(races, by = "raceId") |>
    mutate(tempsS = as.character(seconds_to_period(milliseconds/1000))) |>
    select(forename, surname, tempsS, date)
  
  return(lap_times_for_circuit[1:10,])
  
}