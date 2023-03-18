library(tidyverse)
library(leaflet)
library(htmltools)
library(reshape2)
library(lubridate)

circuits <- read.csv("data/circuits.csv")
races <- read.csv("data/races.csv")
lap_times <- read.csv("data/lap_times.csv")
drivers <- read.csv("data/drivers.csv")


getBestLap <- function(id) {
    # récupère toutes les courses pour un circuit sélectionné
    id_races_for_circuit <- races |> filter(circuitId == id) |>
      select(raceId)
    
    # récupère tout les temps de courses de toutes les courses du circuit
    lap_times_for_circuit <- lap_times |> 
      filter(raceId %in% id_races_for_circuit$raceId)|>
      select(driverId, milliseconds, raceId, time)
    
    if (nrow(lap_times_for_circuit) == 0) {
      return(c("Pas de meilleur tour dans la DB"))
    }
    
    min_lap <- min(lap_times_for_circuit$milliseconds)
    
    # récupèrele meilleur temps
    best_times <- lap_times_for_circuit |> 
      filter(milliseconds == min_lap) |>
      select(milliseconds, driverId, raceId, time)
    
    timeLap <- best_times$time
    date <- races |> 
      filter(raceId == best_times$raceId)|>
      select(date)
    
    
    pilote <- drivers |> 
      filter(driverId == best_times$driverId) |>
      select(surname, forename)
    # resultat avec nom pilote, meilleur temps et année
    pilote <- pilote |> mutate(Temps = timeLap, Date = date$date) 

return(c(pilote$surname, pilote$forename, "en ", pilote$Temps, "le ", pilote$Date))
}

getBestLap(1)
