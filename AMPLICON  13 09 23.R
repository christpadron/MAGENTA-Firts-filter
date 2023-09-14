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


amplicon <- data.frame()


for (col in names(metadata)) {
  filas_con_amplicon <- which(
    grepl("amplicon", metadata[[col]],ignore.case = TRUE))
  

  if (length(filas_con_amplicon) > 0) {
    amplicon <- rbind(amplicon, metadata[filas_con_amplicon, ])
  }
}

amplicon <- amplicon%>%
  unique()
colnames(amplicon)

amplicon_mapa <- amplicon %>%
  select(study_accession, location) %>%
  drop_na()


head(amplicon_mapa)

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


amplicon_mapa$coords <- lapply(amplicon_mapa$location, parse_coordinates)


coord_df <- data.frame(do.call(rbind, amplicon_mapa$coords))


amplicon_mapa$lat <- coord_df$X1
amplicon_mapa$lon <- coord_df$X2


num_amplicon <- nrow(amplicon_mapa)


map_amplicon <- leaflet(amplicon_mapa) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~lon,        
    lat = ~lat,        
    radius = 1,        
    fillOpacity = 0.6, 
    color = "purple"   
  ) %>%
  addControl(
    html = "<h3>AMPLICON</h3>",
    position = "topright"
  ) %>%
  addControl(
    html = paste("Número de puntos: ", num_amplicon),
    position = "bottomleft"
  )

# Mostrar el mapa
map_amplicon

# MAPA Water-------------------------------------------------------------------
amplicon_water <- data.frame()

for (col in names(metadata)) {
  filas_con_water <- which(
    grepl("SEAWATER", metadata[[col]],ignore.case = TRUE))
  
  if (length(filas_con_water) > 0) {
    amplicon_water <- rbind(amplicon_water, metadata[filas_con_water, ])
  }
}

amplicon_water <- amplicon_mapa[
  amplicon_mapa$study_accession %in% amplicon_water$study_accession, ]

num_amplicon_water <- nrow(amplicon_water)

map_amplicon_water <- leaflet(amplicon_water) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~lon,        
    lat = ~lat,        
    radius = 1,        
    fillOpacity = 0.6, 
    color = "orange"   
  ) %>%
  addControl(
    html = "<h3>AMPLICON, SEAWEATER</h3>",
    position = "topright"
  ) %>%
  addControl(
    html = paste("Número de puntos: ", num_amplicon_water),
    position = "bottomleft"
  )

# Mostrar el mapa
map_amplicon_water


#amplicon sediment
amplicon_sediment <- data.frame()

for (col in names(metadata)) {
  filas_con_sediment <- which(
    grepl("sediment", metadata[[col]],ignore.case = TRUE))
  
  if (length(filas_con_sediment) > 0) {
    amplicon_sediment <- rbind(amplicon_sediment, metadata[filas_con_sediment, ])
  }
}


amplicon_sediment <- amplicon_mapa[
  amplicon_mapa$study_accession %in% amplicon_sediment$study_accession, ]


num_amplicon_sediment <- nrow(amplicon_sediment)

map_amplicon_sediment <- leaflet(amplicon_sediment) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~lon,        
    lat = ~lat,        
    radius = 1,        
    fillOpacity = 0.6, 
    color = "brown"   
  ) %>%
  addControl(
    html = "<h3>AMPLICON, SEDIMENT</h3>",
    position = "topright"
  ) %>%
  addControl(
    html = paste("Número de puntos: ", num_amplicon_sediment),
    position = "bottomleft"
  )

# Mostrar el mapa
map_amplicon_sediment

## Rizosfera-------------------------------------------------------------------
amplicon_rhizosphere <- data.frame()

for (col in names(metadata)) {
  filas_con_rhizosphere <- which(
    grepl("rhizosphere", metadata[[col]],ignore.case = TRUE))
  
  if (length(filas_con_rhizosphere) > 0) {
    amplicon_rhizosphere <- rbind(amplicon_rhizosphere, metadata[filas_con_rhizosphere, ])
  }
}

amplicon_rhizosphere <- amplicon_mapa[
  amplicon_mapa$study_accession %in% amplicon_rhizosphere$study_accession, ]



num_amplicon_rhizosphere <- nrow(amplicon_rhizosphere)


map_amplicon_rhizosphere <- leaflet(amplicon_rhizosphere) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~lon,        
    lat = ~lat,        
    radius = 1,        
    fillOpacity = 0.6, 
    color = "yellow"   
  ) %>%
  addControl(
    html = "<h3>AMPLICORNIO, RIZOSFERA</h3>",
    position = "topright"
  ) %>%
  addControl(
    html = paste("Número de puntos: ", num_amplicon_rhizosphere),
    position = "bottomleft"
  )

# Mostrar el mapa
map_amplicon_rhizosphere

## MAPA POR CATEGORIA----------------------------------------------------------

# Cargar las ubicaciones de las tres bases de datos
locations_water <- data.frame(
  lat = amplicon_water$lat, lon = amplicon_water$lon)
locations_rhizosphere <- data.frame(
  lat = amplicon_rhizosphere$lat, lon = amplicon_rhizosphere$lon)
locations_sediment <- data.frame(
  lat = amplicon_sediment$lat, lon = amplicon_sediment$lon)

# Crear un mapa Leaflet
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
    group = "Sediment" # Puedes asignar un nombre a este grupo
  ) %>%
  
  
  # Agregar leyenda
  addLegend(
    position = "bottomright",
    colors = c("blue", "green", "red"),
    labels = c("Water", "Rhizosphere", "Sediment")
  )

# Mostrar el mapa
map_combined

