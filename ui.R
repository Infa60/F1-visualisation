library(shiny)
library(fresh)
library(leaflet)
library(plotly)
library(shinyWidgets)
library(colourpicker)
library(tidyverse)
library(shinythemes)
library(shinycssloaders)

source("code/mapCircuits.R")

files_list <- c("circuits.csv", "constructor_results.csv", "constructor_standings.csv", "constructors.csv", 
                "driver_standings.csv", "drivers.csv", "lap_times.csv", "pit_stops.csv", "qualifying.csv", "races.csv", "results.csv", "seasons.csv", "sprin_results.csv", "status.csv")
# Define UI for application that draws a histogram
options(spinner.color="#e10600", spinner.color.background="#ffffff", spinner.size=0.5)

ui <- navbarPage(
    tags$head(
        includeCSS("styles/style.css")
    ),
    title = "Formula1Data",
    tabPanel("Accueil",
             div(id = "imageContainer",
                 div(id = "bgImage"),
                 h3(id = "titleImage", "Le site Data 100% Formule 1"),
                 p(id = "nomAuteurs", "par Mathieu Bourgeois et Victor Vattier")
                 ),
             div(id = "downloadDiv",
                 div(id = "line"),
                 h2("Un dataset complet de 1950 à aujourd'hui"),
                 a(id = "kaggleLink", target="_blank", href = "https://www.kaggle.com/datasets/rohanrao/formula-1-world-championship-1950-2020","Disponible ici"), align = "center"),
             div(id = "paragraphe",
                 h3("A propos"),
                 p("Ce site internet à été réalisé dans le cadre d'un projet de groupe évalué en visualisation de données. 
                R est le principal langage de programmation utilisé pour la réalisation de cette page web notamment via 
                  le package shiny qui permet de réaliser des applications web de manière simple et efficace.
                   Ce site traite des principales données au sujet du Championnat du monde de formule 1 depuis l'année 1950 
                   (meilleurs pilotes, circuits, meilleurs tours). Cela s'articule autour de graphiques et tables qui permettent de visualiser et 
                   comprendre l'information de manière très simple. Le dataset utilisé provient de Kaggle et à été posté par Rohan Rao, un Data Scientist 
                   Indien renommé.
                   Certaines données restent cependant manquantes notamment lors de la création du championnat du monde de formule 1 (chrono, temps au tour, etc..)."),
                 p("En vous souhaitant une bonne navigation."),
                 p("Mathieu Bourgeois et Victor Vattier", align = "right")),
             ),
    tabPanel(
        "Circuits",
        sidebarLayout(
            sidebarPanel(
                h3("Filtrer les circuits"),
                selectizeGroupUI(
                  id = "my-filters",
                  inline = FALSE,
                  label = NULL,
                  btn_label = "Reset filters",
                  params = list(
                    name = list(inputId = "name", title = "Par nom", placeholder = 'Choisir'),
                    country = list(inputId = "country", title = "Par pays", placeholder = 'Choisir'),
                    location = list(inputId = "location", title = "Par ville", placeholder = 'Choisir')
                  )
                )
                
      ),
      
        mainPanel(
            withSpinner(leafletOutput("circuitsMap"), type = 3),
            p(id = "mapLegend", "Carte de tous les circuits de Formule 1", align = "center"),
        )
      ),
      sidebarLayout(position = "right",
        sidebarPanel(fluid = TRUE,
          textOutput("selectedCircuit"),
          withSpinner(htmlOutput("circuitImage"), type = 3),
          div(id = "circuitInfo",textOutput("ville"),
          textOutput("pays"),
          htmlOutput("bestLap"),)
          
        ),
        mainPanel(
          tabsetPanel(type = c("pills"),
            id = "circuitTabset",
            tabPanel("Meilleurs pilotes", 
                     withSpinner(plotlyOutput("bestPilotesHist"), type = 3),
                     h5("Classement des 10 meilleurs pilotes sur ce circuit (total de points gagnés sur ce circuit)", align = "center"),
                     
                     ),
            tabPanel("Meilleurs écuries", 
                     withSpinner(plotlyOutput("bestTeamsHist"), type = 3),
                     h5("Classement des 10 meilleures écuries sur ce circuit (total de points gagnés sur ce circuit)", align = "center"),
                     ),
            tabPanel("Meilleurs chronos", 
                     withSpinner(DT::dataTableOutput("bestTimes"),type = 3),
                     h5("Classement des 10 meilleures chronos réalisés sur ce circuit (en s)", align = "center"),)
          )
        ),
      )
    ),
    
    tabPanel("Pilotes",
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "F1drivers",label = strong("Choisir pilote "),choices = drivers$driverRef),
                 withSpinner(htmlOutput("piloteImage"), type = 3),
                 div(id = "piloteInfos",uiOutput("nom"),
                 uiOutput("prenom"),
                 textOutput("code"),
                 textOutput("number"),
                 textOutput("dob"),
                 textOutput("nat"),
                 uiOutput("wiki")),
                 tags$hr(style="border-color:MidnightBlue;"),
                 #Permet de sélectionner la course dont on veut voir les temps de passage a chaque tour sur le graphique en fonction du pilote choisi
                 #ATTENTION peut etre passé en liste et non vecteur 
                 uiOutput(outputId = "F1race"),
                 uiOutput("wiki2"),
                 #Permet d'ajouter les temps de passage du premier sur le graphique
                 checkboxInput(inputId = "checkbox",label = strong("Afficher le pilote premier"),value=FALSE),
               ),
               mainPanel(
                         #Table de donnée
                         withSpinner(DT::dataTableOutput("table"), type = 3),
                         br(),
                         #Affiche le graphique avec les temps de passage
                         plotlyOutput("plot2"),conditionalPanel(condition = "input$checkbox==FALSE"),
                         br(),
                         #Afficher le statut de la course
                         div(uiOutput("statut"),align="center"),
                         div(textOutput("first"),align="center"),
               )
             )),
    tabPanel('Saisons',
             sidebarLayout(
               sidebarPanel(width = 3,
                            #Choisir une saison à visualiser 
                            selectInput(inputId = "saison",label = strong("Choisissez une saison à visualiser"),choices = distinct(arrange(races |> select(year),year))),
                            p("Conseil : Cliquez sur une cellule course ID directement dans la table pour obtenir les information sur la course")
                            
                            
               ),
               mainPanel(width = 9,
                         tabsetPanel(
                           tabPanel("Course",
                                    #Afficher les courses ayant été faite sur une saison 
                                    withSpinner(DT::dataTableOutput("coursesaison"),type = 3),
                                    br(),
                                    div(id = "titleClassement", paste("Classement des 3 premiers coureurs pour une course sélectionné :"),align="center"),
                                    br(),
                                    div(id = "classmentPilotes",
                                        textOutput("premier"), 
                                        textOutput("deuxieme"),
                                        textOutput("troisieme")),
                                    tags$hr(style="border-color:LightGrey;"),
                                    #Permet de masquer tout les messages d'erreurs
                                    tags$style(type="text/css",".shiny-output-error { visibility: hidden; }",".shiny-output-error:before { visibility: hidden; }"),
                                    
                                    
                           ),
                           tabPanel("Classement pilote",
                                    #Afficher les pilote ayant courru sur une saison 
                                    withSpinner(DT::dataTableOutput("classementsaison"), 3),
                                    br(),
                                    br(),
                                    tags$hr(style="border-color:LightGrey;"),
                           ),
                           tabPanel("Classement écurie",
                                    withSpinner(DT::dataTableOutput("classementsaison_ecurie"), type = 3),
                                    textOutput("no_data_ecurie"),
                                    br(),
                                    br(),
                                    tags$hr(style="border-color:LightGrey;"),
                           )
                         )
               )
             )
             
    ),
    tabPanel("Données",
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "selectFile", label = "Choisir un fichier", choices = files_list),
                 p(id = "remarque","Remarque : Il sagit des données bruts du dataset")
               ),
               mainPanel(
                 withSpinner(DT::dataTableOutput("datasBrut"), type = 3)
               )
             ))
)
