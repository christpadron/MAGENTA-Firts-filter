# Lista de paquetes que se estan utilizando
paquetes <- c(
  "tidyverse", "readxl", "devtools", "leaflet", "conflicted","ggmap")

# Paqueterias -------------------------------------------------------------


# Función para verificar e instalar paquetes
instalar <- function(paquetes) {
  for (paquete in paquetes) {
    if (grepl("/", paquete)) {
      devtools::install_github(paquete)
    } else if (!require(paquete, character.only = TRUE)) {
      install.packages(paquete)
    }
    library(paquete, character.only = TRUE)
  }
}

instalar(paquetes)

lapply(paquetes, function(paquete) library(paquete, character.only = TRUE))

#Cargar base de datos
#ponemos nombre a la ruta para que solo tengas que cambair esto
ruta_archivo <- "C:\\Users\\cp_cu\\OneDrive\\Escritorio\\Base de datos\\Metabase magenta 1.xlsx"
metadata <- read_excel(ruta_archivo , skip=1 ) %>%
  distinct()

#Ceamos base de datos wgs------------------------------------------------------
wgs <- data.frame()


for (col in names(metadata)) {
  filas_con_wgs <- which(
    grepl("WGS", metadata[[col]],ignore.case = TRUE))
  
  
  if (length(filas_con_wgs) > 0) {
    wgs <- rbind(wgs, metadata[filas_con_wgs, ])
  }
}

wgs <- wgs%>%
  unique()
colnames(wgs)

wgs_mapa <- wgs %>%
  select(study_accession, location) %>%
  drop_na()


head(wgs_mapa)
  
##Mapa-------------------------------------------------------------------------
# Crear una función para parsear las coordenadas en latitud y longitud
parse_coordinates <- function(coord_string) {

  parts <- strsplit(coord_string, " ")
  lat <- as.numeric(parts[[1]][1])
  lon <- as.numeric(parts[[1]][3])
  

  if (grepl("S", coord_string)) {
    lat <- -lat
  }
  if (grepl("W", coord_string)) {
    lon <- -lon
  }
  
  return(c(lat, lon))
}


wgs_mapa$coords <- lapply(wgs_mapa$location, parse_coordinates)


coord_df <- data.frame(do.call(rbind, wgs_mapa$coords))


wgs_mapa$lat <- coord_df$X1
wgs_mapa$lon <- coord_df$X2


num_wgs <- nrow(wgs_mapa)


map_wgs <- leaflet(wgs_mapa) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~lon,        
    lat = ~lat,        
    radius = 1,        
    fillOpacity = 0.6, 
    color = "purple"   
  ) %>%
  addControl(
    html = "<h3>WGS</h3>",
    position = "topright"
  ) %>%
  addControl(
    html = paste("Number of points ", num_wgs),
    position = "bottomleft"
  )

# Mostrar el mapa
map_wgs

# MAPA Water-------------------------------------------------------------------
wgs_water <- data.frame()

for (col in names(metadata)) {
  filas_con_water <- which(
    grepl("SEAWATER", metadata[[col]],ignore.case = TRUE))
  
  if (length(filas_con_water) > 0) {
    wgs_water <- rbind(wgs_water, metadata[filas_con_water, ])
  }
}


wgs_water <- wgs_mapa[wgs_mapa$study_accession %in% wgs_water$study_accession, ]



num_wgs_water <- nrow(wgs_water)


map_wgs_water <- leaflet(wgs_water) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~lon,        
    lat = ~lat,       
    radius =0.5,     
    fillOpacity = 0.6, 
    color = "darkblue"   
  ) %>%
  addControl(
    html = "<h3>WGS, Seaweater</h3>",
    position = "topright"
  ) %>%
  addControl(
    html = paste("Number of points: ", num_wgs_water),
    position = "bottomleft"
  )

# Mostrar el mapa
map_wgs_water


#WGS sediment
wgs_sediment <- data.frame()

for (col in names(metadata)) {
  filas_con_sediment <- which(
    grepl("sediment", metadata[[col]],ignore.case = TRUE))
  
  if (length(filas_con_sediment) > 0) {
    wgs_sediment <- rbind(wgs_sediment, metadata[filas_con_sediment, ])
  }
}


wgs_sediment <- wgs_mapa[
  wgs_mapa$study_accession %in% wgs_sediment$study_accession, ]


num_wgs_sediment <- nrow(wgs_sediment)


map_wgs_sediment <- leaflet(wgs_sediment) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~lon,        
    lat = ~lat,        
    radius = 1,        
    fillOpacity = 0.6, 
    color = "pink"   
  ) %>%
  addControl(
    html = "<h3>WGS, SEDIMENTO</h3>",
    position = "topright"
  ) %>%
  addControl(
    html = paste("Número de puntos: ", num_wgs_sediment),
    position = "bottomleft"
  )

# Mostrar el mapa
map_wgs_sediment

## Rizosfera-------------------------------------------------------------------
wgs_rhizosphere <- data.frame()

for (col in names(metadata)) {
  filas_con_rhizosphere <- which(
    grepl("rhizosphere", metadata[[col]],ignore.case = TRUE))
  
  if (length(filas_con_rhizosphere) > 0) {
    wgs_rhizosphere <- rbind(wgs_rhizosphere, metadata[filas_con_rhizosphere, ])
  }
}


wgs_rhizosphere <- wgs_mapa[wgs_mapa$study_accession %in% wgs_rhizosphere$study_accession, ]



num_wgs_rhizosphere <- nrow(wgs_rhizosphere)


map_wgs_rhizosphere <- leaflet(wgs_rhizosphere) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~lon,        
    lat = ~lat,        
    radius = 1,        
    fillOpacity = 0.6, 
    color = "brown"   
  ) %>%
  addControl(
    html = "<h3>WGS, RHIZOSPHERE</h3>",
    position = "topright"
  ) %>%
  addControl(
    html = paste("Number of points: ", num_wgs_rhizosphere),
    position = "bottomleft"
  )

# Mostrar el mapa
map_wgs_rhizosphere

## MAPA POR CATEGORIA----------------------------------------------------------

locations_water <- data.frame(
  lat = wgs_water$lat, lon = wgs_water$lon)
locations_rhizosphere <- data.frame(
  lat = wgs_rhizosphere$lat, lon = wgs_rhizosphere$lon)
locations_sediment <- data.frame(
  lat = wgs_sediment$lat, lon = wgs_sediment$lon)


map_combined <- leaflet() %>%
  addTiles() %>%
  
  
  addCircleMarkers(
    data = locations_water,
    lng = ~lon,
    lat = ~lat,
    radius = 4,
    fillOpacity = 0.6,
    color = "blue",
    group = "Water" 
  ) %>%
  
  
  addCircleMarkers(
    data = locations_rhizosphere,
    lng = ~lon,
    lat = ~lat,
    radius = 4,
    fillOpacity = 0.6,
    color = "green",
    group = "Rhizosphere" 
  ) %>%
  
  addCircleMarkers(
    data = locations_sediment,
    lng = ~lon,
    lat = ~lat,
    radius = 4,
    fillOpacity = 0.6,
    color = "red",
    group = "Sediment" 
  ) %>%
  
  
  addLegend(
    position = "bottomright",
    colors = c("blue", "green", "red"),
    labels = c("Water", "Rhizosphere", "Sediment")
  )

#Show map
map_combined

