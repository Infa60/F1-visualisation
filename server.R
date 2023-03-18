library(shiny)
library(leaflet)
library(tidyverse)
library(shinyWidgets)
library(plotly)
library(htmltools)
library(reshape2)
library(rsconnect)
library(lubridate)

source("code/mapCircuits.R")
source("code/scrap.R")
source("code/best_lap_circuit.R")
source("code/best_pilotes_for_circuit.R")
source("code/scrapPilote.R")
source("code/bestTeam.R")
source("code/bestTimes.R")

status <- read.csv("data/status.csv")


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  #------------------------------------Partie accueil --------------------------------------------------
  
  #------------------------------------Partie circuits --------------------------------------------------
  
  res_mod <- callModule(
    module = selectizeGroupServer,
    id = "my-filters",
    data = circuits,
    vars = c("name", "location", "country"),
    inline = FALSE
  )
  
  output$circuitsMap <- renderLeaflet({
    leaflet(data = res_mod()) |> 
      addTiles() |>
      addMarkers(~lng, ~lat, 
                 popup = ~link(url, location, name, country), 
                 icon = iconF1,
                 layerId = ~circuitId)
  })
  
  
  # Valeur par defaut affiché
  selectedCircuitId <- 1
  
  data_circuit <- circuits |> 
    filter(circuitId == selectedCircuitId) |> 
    select(name, location, country)
  
  output$selectedCircuit <- renderText(data_circuit$name)
  
  imageCircuitUrl <- getImageCircuitURL(selectedCircuitId)
  
  output$circuitImage <- renderText({
    c('<img src="',imageCircuitUrl, '"/>' )
  })
  
  output$ville <- renderText({c("Ville : ", data_circuit$location)})
  
  output$pays <- renderText({
    c("Pays : ", data_circuit$country)
  })
  
  bestLap <- getBestLap(selectedCircuitId)

  output$bestLap <- renderText({
    c("<p>Record du meilleur tour en course : </p>", as.character(bestLap))
  })
  
  output$bestPilotesHist <- renderPlotly({
    data <- getBestPilotesForCircuit(selectedCircuitId)
    
    plot_ly(x = data$surname, y = data$points, color = data$surname, type = "bar") |>
      layout(xaxis = list(categoryorder = "total descending", title = "Pilotes"), yaxis = list(title = "Total points"))  |>
      hide_legend()
  })
  
  output$bestTeamsHist <- renderPlotly({
    df <- getBestTeams(selectedCircuitId)
    plot_ly(x = df$name, y = df$points, color = df$name, type = "bar") |>
      layout(xaxis = list(categoryorder = "total descending", title = "Ecuries"), yaxis = list(title = "Total points")) |>
      hide_legend()

  })
  
  output$bestTimes <-  DT::renderDataTable({
    dataTimes <- getBestTimes(selectedCircuitId)
    names(dataTimes)[1] <- "Prénom"
    names(dataTimes)[2] <- "Nom"
    names(dataTimes)[3] <- "Temps(s)"
    names(dataTimes)[4] <- "Date"
    print(dataTimes)
    DT::datatable(dataTimes)
  }) 
  
  
  #------------------------------------Partie Pilotes --------------------------------------------------
  
  #Met a jour en permanence le pilote choisi
  piloteId <- reactive({
    unclass(filter(drivers, driverRef == input$F1drivers) |> select(driverId))
  })    
  #Met a jour en permanence le pilote premier en fonction de la course choisi
  pilote1st <- reactive({
    unclass(filter(results,raceId == input$F1course) |> filter(position == 1)|> select(driverId))
  })
  #Met a jour les course du pilote choisi
  coursesID <- reactive({
    unclass(arrange(filter(results,driverId == piloteId()) |>select(raceId),raceId))
  })
  
  
  output$piloteImage <- renderText({
    imageUrl <- getImagePiloteURL(piloteId())
    c('<img src="',imageUrl, '"/>' )
  })
  
  output$nom <- renderText({
    pilotenom <- filter(drivers,driverId==piloteId()) |> select(surname)
    HTML(paste("Nom :","<b>",pilotenom$surname,"</b>"))
  })
  #Récupérer le prénom
  output$prenom <- renderText({
    piloteprenom <- filter(drivers,driverId==piloteId()) |> select(forename)
    HTML(paste("Prénom :","<b>",piloteprenom$forename,"</b>"))
  })
  #Récupérer le code
  output$code <- renderText({
    co <- filter(drivers,driverId==piloteId()) |> select(code)
    paste("Code :",co$code)
  })  
  #Récupérer le numéro
  output$number <- renderText({
    num <- filter(drivers,driverId==piloteId()) |> select(number)
    paste("Numéro :",num$number)
  })   
  #Récupérer la nationalité
  output$nat <- renderText({
    natio <- filter(drivers,driverId==piloteId()) |> select(nationality)
    paste("Nationalité :",natio$nationality)
  })
  #Récupérer la date de naissance
  output$dob <- renderText({
    birth <- filter(drivers,driverId==piloteId()) |> select(dob)
    paste("Date de naissance :",birth$dob)
  })  
  #Récupérer le lien wikipedia
  output$wiki <- renderText({
    wikipilote <- filter(drivers,driverId==piloteId()) |> select(url)
    url <- a("Wiki Pilote",href=wikipilote)
    paste(url)
  }) 
  
  #Génerer la liste des courses effectué par le driver
  output$F1race <- renderUI({
    selectInput(inputId = "F1course", label = strong("Course à visionner"), choices = coursesID(),selected = input$table_cell_clicked[3])
  })
  
  output$wiki2 <- renderText({
    wikicourse <- filter(races, raceId==input$F1course) |> select(url)
    url <- a("Wiki Course",href=wikicourse)
    paste(url)
  })    
  
  #Affiche le nom de toute les courses et leurs date en fonction du pilote choisi
  output$table <- DT::renderDataTable({
    nameandyear3 <- races |> filter(raceId %in% (filter(results,driverId==(piloteId())) |> select(raceId))$raceId) |> select(name,date,raceId)
    names(nameandyear3)[1] <- " Nom de la course"
    names(nameandyear3)[2] <- " Date"
    names(nameandyear3)[3] <- " Course Id"
    
    DT::datatable(nameandyear3, options = list(dom="ft",pageLength = 999,scrollY = 200, scroller = TRUE),selection = list(target ='cell'))
  })
  
  #Défini le graphique avec le temps de passage a chaque tour pour le pilote et la course choisi  
  
  output$plot2 <- renderPlotly({
    passage <- arrange(filter (lap_times,raceId == input$F1course ) |> filter(driverId==piloteId()) |> select(lap,milliseconds),lap)
    passage1st <- arrange(filter(lap_times,raceId == input$F1course )|> filter(driverId==pilote1st()) |> select(lap,milliseconds),lap)
    ref_pilote_1st <- filter(drivers,driverId==pilote1st()) |> select(driverRef)
    plotpass <- plot_ly(passage, x = ~lap, y = ~milliseconds/1000, type = 'scatter', mode = 'lines', name = input$F1drivers)
    plotpass <- plotpass %>% add_trace(data = passage1st,x = ~lap,y = ~milliseconds/1000,type = 'scatter',mode ='lines', name = ref_pilote_1st)
    plotpass <- layout(plotpass,
                       title = "Temps de passage",
                       xaxis = list(title = "Tours"),
                       yaxis = list(title = "Temps (s)"))
    
    plotpass1 <- plot_ly(passage, x = ~lap, y = ~milliseconds/1000, type = 'scatter', mode = 'lines', name = input$F1drivers)
    plotpass1 <- layout(plotpass1,
                        title = "Temps de passage",
                        xaxis = list(title = "Tours"),
                        yaxis = list(title = "Temps (s)"))
    if(input$checkbox) { plotpass } else { plotpass1 }
  })
  
  #Affiche l'état de la course
  output$statut <- renderText({
    name_pilote <- filter(drivers,driverId==piloteId()) |> select(forename, surname)
    
    statut <- unclass(filter(results,driverId == piloteId()) |> filter(raceId == input$F1course) |> select(statusId))
    stat <- filter(status,statusId==statut) |> select(status)
    HTML(paste("Etat de la course de ",name_pilote$forename,name_pilote$surname," : ", "<b>",stat,"</b>"))
  })
  
  #Affiche le nom du pilote ayant fini premier sur la course selectionné
  output$first <- renderText({
    name_pilote_1st <- filter(drivers,driverId==pilote1st()) |> select(forename, surname)
    paste(name_pilote_1st$forename,name_pilote_1st$surname,"est le premier")
  })
  
  
  #Affiche la table des courses en fonction de la saison sélectionnée
  output$coursesaison <- DT::renderDataTable({
    season <- filter(races,year==input$saison) |> select(round,name,date,raceId)
    names(season)[1] <- " Étape"
    names(season)[2] <- " Nom du grand prix"
    names(season)[3] <- " Date"
    names(season)[4] <- " Course Id"    
    DT::datatable(season,options = list(pageLength = 999,dom="t",scrollY = 260, scroller = TRUE),selection = list(target ='cell'),rownames = FALSE)
  })
  
  #Affiche le nom du pilote ayant fini premier sur la course selectionné
  output$first <- renderText({
    name_pilote_1st <- filter(drivers,driverId==pilote1st()) |> select(forename, surname)
    paste(name_pilote_1st$forename,name_pilote_1st$surname,"est le premier")
  })
  
  #------------------------------------Partie saisons --------------------------------------------------
  
  output$coursesaison <- DT::renderDataTable({
    season <- filter(races,year==input$saison) |> select(round,name,date,raceId)
    names(season)[1] <- " Étape"
    names(season)[2] <- " Nom du grand prix"
    names(season)[3] <- " Date"
    names(season)[4] <- " Course Id"    
    DT::datatable(season,options = list(pageLength = 999,dom="t",scrollY = 260, scroller = TRUE),selection = list(target ='cell'),rownames = FALSE)
  })
  
  output$classementsaison <- DT::renderDataTable({
    season_pilote <- filter(races,year==input$saison) |> select(raceId)
    listepiloteId <- distinct(results |> filter(raceId %in% season_pilote$raceId) |> select(driverId))
    season_name_pilote <- drivers |> filter(driverId %in% listepiloteId$driverId) |> select(forename, surname,dob,nationality,driverId)
    
    racebysaison <- filter(races,year==input$saison) |> select(raceId)
    test <- results |> filter(raceId %in% racebysaison$raceId) |> select(driverId,points)
    classementsaison <- arrange(test %>% group_by(driverId) %>% summarise_if(is.numeric,sum),desc(points))
    
    classement_ord<-arrange(inner_join(season_name_pilote,classementsaison),desc(points))
    classementfinal <- classement_ord |> select(forename,surname,dob,nationality,points)
    names(classementfinal)[1] <- " Prénom"
    names(classementfinal)[2] <- " Nom"
    names(classementfinal)[3] <- " Date de naissance"
    names(classementfinal)[4] <- " Nationalité"
    names(classementfinal)[5] <- " Points"
    
    DT::datatable(classementfinal,options = list(pageLength = 999,dom="t",scrollY = 260, scroller = TRUE))
  })
  
  output$classementsaison_ecurie <- DT::renderDataTable({
    season_constru <- filter(races,year==input$saison) |> select(raceId)
    listeconstruId <- distinct(constructor_results |> filter(raceId %in% season_constru$raceId) |> select(constructorId))
    season_name_constru <- constructors |> filter(constructorId %in% listeconstruId$constructorId) |> select(name,nationality,url,constructorId)
    racebysaison <- filter(races,year==input$saison) |> select(raceId)
    test <- constructor_results |> filter(raceId %in% racebysaison$raceId) |> select(constructorId,points)
    classementsaison <- arrange(test %>% group_by(constructorId) %>% summarise_if(is.numeric,sum),desc(points))
    
    classement_ord<-arrange(inner_join(season_name_constru,classementsaison),desc(points))
    classementfinal <- classement_ord |> select(name,nationality,points)
    names(classementfinal)[1] <- " Nom"
    names(classementfinal)[2] <- " Nationalité"
    names(classementfinal)[3] <- " Points"
    
    DT::datatable(classementfinal,options = list(pageLength = 999,dom="t",scrollY = 260, scroller = TRUE))
    
  })
  
  output$premier <- renderText({
    premiersaison <- filter(results,raceId==as.numeric(input$coursesaison_cell_clicked[3]), position==1) |> select(driverId,milliseconds)
    tempspremier <-seconds_to_period(as.numeric(premiersaison$milliseconds)/1000)
    nom_premier <- filter(drivers,driverId==premiersaison$driverId) |> select(surname,forename)
    paste("1er : ", nom_premier$forename, nom_premier$surname, " en", round(tempspremier,4))
  })
  
  output$deuxieme <- renderText({
    premiersaison <- filter(results,raceId==as.numeric(input$coursesaison_cell_clicked[3]), position==2) |> select(driverId,milliseconds)
    tempspremier <-seconds_to_period(as.numeric(premiersaison$milliseconds)/1000)
    nom_premier <- filter(drivers,driverId==premiersaison$driverId) |> select(surname,forename)
    paste("2ème : ", nom_premier$forename,nom_premier$surname, " en", round(tempspremier,4))
  })
  
  output$troisieme <- renderText({
    premiersaison <- filter(results,raceId==as.numeric(input$coursesaison_cell_clicked[3]), position==3) |> select(driverId,milliseconds)
    tempspremier <-seconds_to_period(as.numeric(premiersaison$milliseconds)/1000)
    nom_premier <- filter(drivers,driverId==premiersaison$driverId) |> select(surname,forename)
    paste("3ème : ", nom_premier$forename,nom_premier$surname, " en", round(tempspremier,4))
  })
  
  output$datasBrut <- DT::renderDataTable({
    file <- read.csv(paste("data/",input$selectFile, sep = ""))
  })
  
  
  # change la valeur en fonction du click
  observeEvent(input$button_click, {
    
    selectedCircuitId <- input$circuitsMap_marker_click$id
    
    nom_circuit <- circuits |> 
      filter(circuitId == selectedCircuitId) |> 
      select(name, location, country)
    
    output$selectedCircuit <- renderText(nom_circuit$name)
    
    imageCircuitUrl <- getImageCircuitURL(selectedCircuitId)
    
    output$circuitImage <- renderText({
      c('<img src="',imageCircuitUrl, '"/>' )
    })
    
    output$ville <- renderText({
      c("Ville : ", nom_circuit$location)
    })
    
    output$pays <- renderText({
      c("Pays : ", nom_circuit$country)
    })
    
    bestLap <- getBestLap(selectedCircuitId)

    
    output$bestLap <- renderText({
      c("<p>Record du meilleur tour en course : </p>", as.character(bestLap))
    }) 

    output$bestPilotesHist <- renderPlotly({
      data <- getBestPilotesForCircuit(selectedCircuitId)
      plot_ly(x = data$surname, y = data$points, color = data$surname, type = "bar") |>
        layout(xaxis = list(categoryorder = "total descending", title = "Pilotes"), yaxis = list(title = "Total points")) |>
        hide_legend()
      
    })   

    output$bestTeamsHist <- renderPlotly({
      df <- getBestTeams(selectedCircuitId)
      print(df)
      plot_ly(x = df$name, y = df$points, color = df$name, type = "bar") |>
        layout(xaxis = list(categoryorder = "total descending", title = "Ecuries"), yaxis = list(title = "Total points")) |>
        hide_legend()
    })
    
    output$bestTimes <-  DT::renderDataTable({
      dataTimes <- getBestTimes(selectedCircuitId)
      names(dataTimes)[1] <- "Prénom"
      names(dataTimes)[2] <- "Nom"
      names(dataTimes)[3] <- "Temps(s)"
      names(dataTimes)[4] <- "Date"
      DT::datatable(dataTimes)
    }) 
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}
