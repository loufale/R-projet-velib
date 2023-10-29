#Installer package pour lire un fichier json
install.packages(c("httr","jsonlite"))
library(httr)
library(jsonlite)

#Importer fichier json

station <- fromJSON(txt = "https://api.jcdecaux.com/vls/v1/stations?contract=Lyon&apiKey=06113d357d15151b52d370088dc8ea0d16e596b5")


#Scinder la position en 2 car la colonne est de type DATA.FRAME
library(tidyr)
library(dplyr)

# Tentative 3
install.packages("tidyverse")
library(tidyverse)
station <- unnest(data = station, cols = position)


names(station)[1] <- "Station_id"
names(station)[2] <- "Nom_contrat"
names(station)[3] <- "Nom_station"
names(station)[4] <- "Adresse_station"
names(station)[5] <- "Latitude"
names(station)[6] <- "Longitude"
names(station)[7] <- "Si_Transaction_bancaire"
names(station)[8] <- "Si_promotion"
names(station)[9] <- "Nb_total_support_station"
names(station)[10] <- "Nb_support_station_dispo"
names(station)[11] <- "Nb_velos_dispo"
names(station)[12] <- "Statut"
names(station)[13] <- "Derniere_maJ"








install.packages('tidygeocoder')
library(tidygeocoder)



code_postal<-reverse_geocode(station, lat = Latitude, long = Longitude, method = 'osm',
                address = address_found, full_results = TRUE)

code_postal<-code_postal[,c("Station_id","postcode")]

write.csv2(code_postal, "Code_postaux.csv",row.names = F)

#utiliser le csv pour filtrer les code postaux
#le csv doit etre au même endroit que nos script pr ne pas ecrire un chemin long (tout doit etre dans le dossier du projet)