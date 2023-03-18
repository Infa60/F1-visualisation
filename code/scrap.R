library(rvest)
library(tidyverse)

source("code/mapCircuits.R")


# création de la data frame qui contient les url des photos
getImageCircuitURL <- function(id) {
  
  wiki_circuits_link <- circuits |> 
    filter(circuitId == id)
  
  image_url_value <- read_html(wiki_circuits_link$url) |>
      html_element(".infobox-image img") |>
      html_attr("src")
  
  return(image_url_value)
}

