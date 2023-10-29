# Liste des packages à installer et charger
packages_to_install <- c("shiny", "shinydashboard","plotly","shinyWidgets", "ggplot2", "shinyjs", "leaflet", "httr", "jsonlite", "tidyverse")

# Vérifier si chaque package est installé et l'installer s'il ne l'est pas
for (package in packages_to_install) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package, dependencies = TRUE)
  }
}

# Charger les packages
library(shiny)
library(shinydashboard)
library(ggplot2)
library(shinyjs)
library(leaflet)
library(httr)
library(jsonlite)
library(tidyr)
library(dplyr)
library(tidyverse)
library(shinyWidgets)
library(plotly)

cp<-read.csv2("Code_postaux.csv")

#Importer fichier json

station <- fromJSON(txt = "https://api.jcdecaux.com/vls/v1/stations?contract=Lyon&apiKey=06113d357d15151b52d370088dc8ea0d16e596b5")

station <- unnest(data = station, cols = position)#avec cette fonction, la colonne est scindée 


###Renommer colonnes pour plus de compréhension : 


station <- setNames(station, c("Station_id", "Nom_contrat", "Nom_station", "Adresse_station", "Latitude", "Longitude", "Si_Transaction_bancaire", "Si_promotion", "Nb_total_support_station", "Nb_support_station_dispo", "Nb_velos_dispo", "Statut", "Derniere_maJ"))


# Define UI ----
ui <- dashboardPage(
  dashboardHeader(
    title = "VELIBRE",
    titleWidth = 350
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Accueil", tabName = "accueil"),
      menuItem("Graphiques", tabName = "graphiques"),
      menuItem("Carte", tabName = "carte")  # Ajout de l'onglet "Carte"
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML(".custom-title {
                        color: #00bfff; 
                        text-align: center; 
                      }")),
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css") 
    ),
    tabItems(
      tabItem(tabName = "accueil",
              fluidRow(
                tags$h1("VELIBRE", class = "custom-title"),
                column(12, align = "center",
                       h3("L'API qui vous permet de simplifier l'usage des Vélo'v", style = "color: #333; margin-top: 10px;"),
                       
                       # liens vers only moov
                       p(class = "text-before-button", "ONLY'MOOV pour calculer vos trajets : "),
                       tags$a(href = "https://www.onlymoov.com/", 
                              target = "_blank",
                              tags$button("ONLY'MOOV", 
                                          style = "background-color: red; color: white; border: none; padding: 10px 20px; font-size: 16px; cursor: pointer;"),
                              style = "text-align: center; margin-top: 10px;"
                       ),
                       
                       # liens vers velib
                       p(class = "text-before-button", "Liens vers le site officiel Velo'v : "),
                       tags$a(href = "https://www.velov.grandlyon.com/", 
                              target = "_blank",
                              tags$button("Vélo'v", 
                                          style = "background-color: red; color: white; border: none; padding: 10px 20px; font-size: 16px; cursor: pointer;"),
                              style = "text-align: center; margin-top: 10px;"
                       )
                ),
              )

      ),
      tabItem(tabName = "graphiques",
              fluidRow(
                 column(9,
                        box(
                          title = "Répartition des places et vélos disponibles",
                          plotOutput("piechart")
                        ), 
                        # Bouton d'export
                        downloadButton("exportGraphButton", "Exporter le graphique")
                        ),
                 # column(9,
                 #        box(
                 #          title = "Répartition des stations prenant les transactions bancaires",
                 #          plotOutput("piechart")
                 #        ))
                 
                 # Bouton de rafraîchissement des données
                 actionButton("refreshDataButton", "Rafraîchir les données")
              )
      ),
      tabItem(tabName = "carte",
              fluidRow(
                column(10,
                       box(
                         title = "Stations Vélov à Lyon", width = 9,
                         #tags$style(type = "text/css", "html, body {width:100%; height:100%}")
                         leafletOutput("m", width = "100vh",height = "100vh")  # Sortie pour la carte
                       )),
                column(10,
                       box(
                       selectInput("filter_Code_postal", "Filtrer par Code_postal :", 
                                   choices = unique(cp$Code_postal), selected = NULL),
                       # Bouton de rafraîchissement des données
                       actionButton("refreshDataButton", "Rafraîchir les données")
                       ))
              )
      )
    )
  )
)

server <- function(input, output) {
  
  # Exemple de données pour les graphiques
  data <- data.frame(
    Category = c("A", "B", "C"),
    Value = c(30, 20, 50)
  )
  
  # Diagramme circulaire
   places_disponibles <- station$Nb_support_station_dispo
  velos_disponibles <- station$Nb_velos_dispo
  
   total_places <- sum(places_disponibles)
   total_velos <- sum(velos_disponibles)
  
  labels <- c(paste("Places (", total_places, ")", sep = ""), paste("Vélos (", total_velos, ")", sep = ""))
   colors <- c("blue", "green")
  
   output$piechart <- renderPlot({
     pie(c(total_places, total_velos), labels = labels, explode = 0.1, col = colors)
   })

   # banking_counts <- table(station$Si_Transaction_bancaire)
   # banking_percentages <- banking_counts / sum(banking_counts) * 100
   # 
   # labels <- c("Non", "Oui")
   # colors <- c("red", "green")
   # 
   # output$piechart <- renderPlot({
   #   pie(banking_percentages, labels = labels, col = colors)
   # })
   

  # Exemple de carte (vous devrez personnaliser la carte avec les données réelles)
 
output$m <- renderLeaflet({
  leaflet() %>%
    addTiles() %>%  # Ajouter des tuiles de carte par défaut
    addMarkers(
      data = station,  # Utilisez vos données de station ici
      lng = ~Longitude,
      lat = ~Latitude,
      popup = ~paste("Station: ", Nom_station),
      clusterOptions = markerClusterOptions()
    )
})

######Filtrer la carte 
filtered_data <- reactive({
  cp_filtered <- cp  # Créez une copie de votre jeu de données
  
  # Si une valeur est sélectionnée dans la liste déroulante, filtrez les données
  if (!is.null(input$filter_Code_postal)) {
    cp_filtered <- cp_filtered[cp_filtered$Code_postal == input$filter_Code_postal, ]
  }
  
  return(cp_filtered)
})

# Mettez à jour vos sorties en fonction des données filtrées
output$cp <- renderTable({
  cp_filtered <- filtered_data()
  return(cp_filtered)
})

}


#Rafraîchir les données
observeEvent(input$refreshDataButton, {
  #Pour mettre à jour les données (le chargement depuis une source externe)
  updated_station <- fromJSON(txt = "https://api.jcdecaux.com/vls/v1/stations?contract=Lyon&apiKey=06113d357d15151b52d370088dc8ea0d16e596b5")
  
  # On met à jour station avec les nouvelles données
  station <- updated_station
  
  # Actualiser la carte si nécessaire
  leafletProxy("m") %>%
    clearMarkers() %>%
    addMarkers(
      data = station,
      lng = ~Longitude,
      lat = ~Latitude,
      popup = ~paste("Station: ", Nom_station),
      clusterOptions = markerClusterOptions()
    )
})


# Générer l'image PNG

  exportGraphButton <- downloadHandler(
  filename = function() {
    "graphique.png"  # Nom du fichier de sortie
 },
   content = function(file) {
     # générer l'image PNG 
     g <- ggplot(data = station) + pie(c(total_places, total_velos), labels = labels, explode = 0.1, col = colors) 
     ggsave(file, plot = g, width = 8, height = 6, dpi = 300)  # Personnalisation des dimensions et la résolution
   }
 )


# Run the app
shinyApp(ui = ui, server = server)