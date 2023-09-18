####----------------Packs---------------------------------------------------####

paquetes <- c(
  "tidyverse", "readxl", "devtools", "leaflet", "conflicted","ggmap")

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
####------------------Data base--------------------------------------------####
#por poner enlace para descargar
ruta_archivo <- "C:\\Users\\cp_cu\\OneDrive\\Escritorio\\Base de datos\\Metabase magenta 1.xlsx"

metadata <- read_excel(ruta_archivo , skip=1 ) %>%
  distinct()

####-----------------WGS---------------------------------------------------####
wgs <- data.frame()


for (col in names(metadata)) {
  filas_con_wgs <- which(
    grepl("WGS", metadata[[col]],ignore.case = TRUE))
  
  filas_con_illumina <- which(
    grepl("illumina", metadata[[col]], ignore.case = TRUE))
  
  filas_con_ambos <- intersect(filas_con_wgs, filas_con_illumina)
  
  if (length(filas_con_ambos) > 0) {
    wgs <- rbind(wgs, metadata[filas_con_ambos, ])
  }
}

wgs <- wgs%>%
  unique()

wgs_mapa <- wgs[, c("study_accession", "location")]
wgs_mapa <- na.omit(wgs_mapa)


####-----------------Metagenomics map--------------------------------------####
#This funtion cheangethe format of coords
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

####--------------------Metagenomics---------------------------------------####
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
    radius = 0.1,        
    fillOpacity = 0.6, 
    color = "black"   
  ) %>%
  addControl(
    html = "<h3>METAGENOMICS</h3>",
    position = "topright"
  ) %>%
  addControl(
    html = paste("DATA NUMBER: ", num_wgs),
    position = "bottomleft"
  )

map_wgs

####----------------METAGENOMICS WATER-------------------------------------####
wgs_water <- data.frame()

for (col in names(metadata)) {
  filas_con_water <- which(
    grepl("SEAWATER", metadata[[col]],ignore.case = TRUE))
  
  if (length(filas_con_water) > 0) {
    wgs_water <- rbind(wgs_water, metadata[filas_con_water, ])
  }
}


wgs_water <- wgs_mapa[
  wgs_mapa$study_accession %in% wgs_water$study_accession, ]



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
    html = "<h3>METAGENOMICS:WATER</h3>",
    position = "topright"
  ) %>%
  addControl(
    html = paste("DATA NUMBER: ", num_wgs_water),
    position = "bottomleft"
  )

map_wgs_water


####-------------------METAGENOMICS:SEDIMENT-------------------------------####
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
    radius = 0.1,        
    fillOpacity = 0.6, 
    color = "brown"   
  ) %>%
  addControl(
    html = "<h3>METAGENOMICS:SEDIMENTO</h3>",
    position = "topright"
  ) %>%
  addControl(
    html = paste("DATA NUMBER: ", num_wgs_sediment),
    position = "bottomleft"
  )

map_wgs_sediment


####-----------METAGENOMICS:RHIZOSPHERE------------------------------------####
wgs_rhizosphere <- data.frame()

for (col in names(metadata)) {
  filas_con_rhizosphere <- which(
    grepl("rhizosphere", metadata[[col]],ignore.case = TRUE))
  
  if (length(filas_con_rhizosphere) > 0) {
    wgs_rhizosphere <- rbind(
      wgs_rhizosphere, metadata[filas_con_rhizosphere, ])
  }
}

wgs_rhizosphere <- wgs_mapa[
  wgs_mapa$study_accession %in% wgs_rhizosphere$study_accession, ]

num_wgs_rhizosphere <- nrow(wgs_rhizosphere)

map_wgs_rhizosphere <- leaflet(wgs_rhizosphere) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~lon,        
    lat = ~lat,        
    radius = 1,        
    fillOpacity = 0.6, 
    color = "orange"   
  ) %>%
  addControl(
    html = "<h3>METAGENOMICS:RHIZOSPHERE</h3>",
    position = "topright"
  ) %>%
  addControl(
    html = paste("DATA NUMBER: ", num_wgs_rhizosphere),
    position = "bottomleft"
  )

map_wgs_rhizosphere

####-----------------METAGENOM:COLOR---------------------------------------####

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
    radius = 1,
    fillOpacity = 0.6,
    color = "darkblue",
    group = "WATER" 
  ) %>%
  
  addCircleMarkers(
    data = locations_rhizosphere,
    lng = ~lon,
    lat = ~lat,
    radius = 0.6,
    fillOpacity = 0.6,
    color = "orange",
    group = "RHIZOSPHERE" 
  ) %>%
  
  addCircleMarkers(
    data = locations_sediment,
    lng = ~lon,
    lat = ~lat,
    radius = 0.6,
    fillOpacity = 0.6,
    color = "brown",
    group = "SEDIMENT" 
  ) %>%
  
  
  addLegend(
    position = "bottomright",
    colors = c("darkblue", "orange", "brown"),
    labels = c("WATER", "RHIZOSPHERE", "SEDIMENT")
  )

map_combined

