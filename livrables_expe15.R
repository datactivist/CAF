# livrables pour la deuxième expé de données


# carte des REAAP et LAEP

library(sp)
library(spdplyr)
library(tidyverse)
library(tmap)
library(tmaptools)
library(banR)
library(hrbrthemes)
library(geojsonio)
library(banR)

library(sf)
iris <- read_sf("./data/CONTOURS-IRIS75.shp", stringsAsFactors = FALSE)

library(extrafont)
# font_import()
loadfonts("pdf", quiet = TRUE)
loadfonts("postscript", quiet = TRUE)

# chargement données LAEP et REAAP

LAEP <- read_csv2("./data/LAEP_2015.csv")
REAAP <- read_csv2("./data/REAAP_2015.csv")

# carte lieux

LAEP <- LAEP %>% 
  mutate(adresse = paste0(`N° de rue`, " ", `Nom de la rue`, " ", Ville)) %>% 
  dplyr::rename(code_postal = `Code postal`) %>% 
  ban_geocode(adresse, code_postal = "code_postal") %>% 
  filter(!is.na(latitude))

REAAP <- REAAP %>% 
  mutate(adresse = paste0(`n° de rue`, " ", `nom de rue`, " ", ville)) %>% 
  dplyr::rename(code_postal = `code postal`) %>% 
  ban_geocode(adresse, code_postal = "code_postal") %>% 
  filter(!is.na(latitude))

coordinates(LAEP) <- c("longitude", "latitude")
LAEP@proj4string <- CRS("+init=epsg:4326")

coordinates(REAAP) <- c("longitude", "latitude")
REAAP@proj4string <- CRS("+init=epsg:4326")

iris <- iris %>% 
  st_transform(4326)

iris15 <- iris %>% 
  filter(DEPCOM %in% "75115") %>% 
  st_union() %>% 
  st_sf() %>% 
  st_transform(4326)


stbb <- iris %>% 
  filter(DEPCOM %in% "75115") %>% 
  st_bbox()

stbb_tmapbb <- function(x) {
  matrix(c(x["xmin"], x["ymin"], x["xmax"], x["ymax"]), nrow = 2, dimnames = list(c("x", "y"), c("min", "max")))
}

paris15_osm <- read_osm(stbb_tmapbb(stbb), type = "https://cartodb-basemaps-a.global.ssl.fastly.net/light_all/{z}/{x}/{y}.png", minNumTiles = 40, zoom = 16)

paris15_osm_dark <- read_osm(stbb_tmapbb(stbb), type = "https://cartodb-basemaps-a.global.ssl.fastly.net/dark_all/{z}/{x}/{y}.png", minNumTiles = 40, zoom = 16)

freresvoisins <- iris %>% 
  filter(NOM_IRIS %in% "JAVEL 1") %>% 
  st_union() %>% 
  st_sf() %>% 
  st_transform(4326)

pdf("./livrables/carte_parentalite.pdf", width = 16.54, height = 23.39)
png("./livrables/carte_parentalite.png", width = 16.54, height = 23.39, units = "in", res = 300)
tm_shape(paris15_osm) +
  tm_raster() +
  tm_shape(iris %>% filter(DEPCOM %in% "75115")) +
    tm_borders(alpha = 0.1) +
  tm_shape(iris15) +
    tm_borders() +
  tm_shape(freresvoisins) +
    tm_borders(lty = 4, lwd = 2) + 
  tm_shape(LAEP) +
    tm_squares(col = "blue") +
    tm_text(text = "Nom du LAEP", just = c("left", "bottom"), xmod = 0.5, size = 0.7) +
  tm_shape(REAAP) +
    tm_squares(col = "red") +
    tm_text(text = "Gestionnaire demandeur", just = c("right", "bottom"), xmod = -0.5, size = 0.7) +
  tm_layout(title = "LAEP et REAAP dans le 15e arrondissement", title.position = c("RIGHT", "TOP"), scale = 2, attr.outside.position = "bottom", attr.outside = TRUE) +
  tm_credits(text = "En bleu : LAEP. En rouge : REAAP. \nSource : CAF de Paris. Réalisation : École des données/OKF pour la CAF. Map tiles by Carto, under CC BY 3.0. Data by OpenStreetMap, under ODbL.")
dev.off()
dev.off()

pdf("./livrables/carte_parentalite_dark.pdf", width = 16.54, height = 23.39)
png("./livrables/carte_parentalite_dark.png", width = 16.54, height = 23.39, units = "in", res = 300)
tm_shape(paris15_osm_dark) +
  tm_raster() +
  tm_shape(iris %>% filter(DEPCOM %in% "75115")) +
  tm_borders(alpha = 0.1) +
  tm_shape(iris15) +
  tm_borders() +
  tm_shape(freresvoisins) +
  tm_borders(lty = 4, lwd = 2) + 
  tm_shape(LAEP) +
  tm_squares(col = "blue") +
  tm_text(text = "Nom du LAEP", just = c("left", "bottom"), xmod = 0.5, col = "#d3d3d3", size = 0.7) +
  tm_shape(REAAP) +
  tm_squares(col = "red") +
  tm_text(text = "Gestionnaire demandeur", just = c("right", "bottom"), xmod = -0.5, col = "#d3d3d3", size = 0.7) +
  tm_layout(title = "LAEP et REAAP dans le 15e arrondissement", title.position = c("RIGHT", "TOP"), scale = 2, attr.outside.position = "bottom", attr.outside = TRUE, title.color = "#A8A8A8") +
  tm_credits(text = "En bleu : LAEP. En rouge : REAAP. \nSource : CAF de Paris. Réalisation : École des données/OKF pour la CAF. Map tiles by Carto, under CC BY 3.0. Data by OpenStreetMap, under ODbL.")
dev.off()
dev.off()

# export geo_json
LAEP <- st_as_sf(LAEP)
geojson_write(LAEP, file = "./geojson/LAEP.geojson")
REAAP <- st_as_sf(REAAP)
geojson_write(REAAP, file = "./geojson/REAAP.geojson")
