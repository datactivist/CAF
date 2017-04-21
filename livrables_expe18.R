# livrables pour la troisième expé de données


# carte des divers dispositifs

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

# chargement données REAAP, CLASS, PROJET_ADO

REAAP <- read_csv2("./data/REAAP_2015_18.csv")
CLASSasso <- read_csv2("./data/CLASS_associatif.csv")
CLASSmuni <- read_csv2("./data/CLASS_DASCO.csv")

# carte lieux

REAAP <- REAAP %>% 
  mutate(adresse = paste0(`n° de rue`, " ", `nom de rue`, " ", ville)) %>% 
  dplyr::rename(code_postal = `code postal`) %>% 
  ban_geocode(adresse, code_postal = "code_postal") %>% 
  filter(!is.na(latitude)) 

coordinates(REAAP) <- c("longitude", "latitude")
REAAP@proj4string <- CRS("+init=epsg:4326")

REAAP <- REAAP %>% 
  mutate(TOPADO = recode(TOPADO, "1" = "Oui", "0" = "Non"))

CLASSasso <- CLASSasso %>% 
  separate(Arrondissement, into = c("code_postal", "ville"), sep = " ") %>% 
  mutate(adresse = paste0(Adresse, ", ", ville)) %>% 
  ban_geocode(adresse, code_postal = "code_postal") %>% 
  filter(!is.na(latitude)) %>% 
  mutate(adresse = paste0(Adresse, ", ", ville)) 

coordinates(CLASSasso) <- c("longitude", "latitude")
CLASSasso@proj4string <- CRS("+init=epsg:4326")

CLASSmuni <- CLASSmuni %>% 
  mutate(code_postal = Arrondissement) %>% 
  mutate(ville = "Paris") %>% 
  mutate(adresse = paste0(Adresse, ", ", ville)) %>% 
  ban_geocode(adresse, code_postal = "code_postal") %>% 
  filter(!is.na(latitude))

coordinates(CLASSmuni) <- c("longitude", "latitude")
CLASSmuni@proj4string <- CRS("+init=epsg:4326")
  
projet_ado <- read_csv2("./data/PROJET_ADO.csv") %>% 
  ban_geocode(Adresse, code_postal = "Arrondissement") %>% 
  filter(!is.na(latitude))
coordinates(projet_ado) <- c("longitude", "latitude")
projet_ado@proj4string <- CRS("+init=epsg:4326")



iris <- iris %>% 
  st_transform(4326)

iris18 <- iris %>% 
  filter(DEPCOM %in% "75118") %>% 
  st_sf() %>% 
  st_union() %>% 
  st_sf() %>% 
  st_transform(4326)


stbb <- iris18 %>% 
  st_bbox()

stbb_tmapbb <- function(x) {
  matrix(c(x["xmin"], x["ymin"], x["xmax"], x["ymax"]), nrow = 2, dimnames = list(c("x", "y"), c("min", "max")))
}

paris18_osm <- read_osm(stbb_tmapbb(stbb), type = "https://cartodb-basemaps-a.global.ssl.fastly.net/light_all/{z}/{x}/{y}.png", minNumTiles = 40, zoom = 16, ext = 1.2)



lachapelle <- iris %>% 
  filter(stringr::str_detect(NOM_IRIS, "CHAPELLE")) %>% 
  st_sf() %>% 
  st_union() %>% 
  st_sf() %>% 
  st_transform(4326)

pdf("./livrables/carte_18.pdf", width = 16.54, height = 23.39)
png("./livrables/carte_18.png", width = 16.54, height = 23.39, units = "in", res = 300)
tm_shape(paris18_osm) +
    tm_raster() +
  tm_shape(iris %>% filter(DEPCOM %in% "75118") %>% st_sf()) +
    tm_borders(alpha = 0.1) +
  tm_shape(iris18) +
    tm_borders() +
  tm_shape(lachapelle) +
    tm_borders(lty = 4, lwd = 2) + 
  tm_shape(REAAP) +
    tm_symbols(col = "red", shape = "TOPADO", title.shape = "REEAP - actions en direction des ados", size = 0.2) +
#    tm_text(text = "Gestionnaire.demandeur", just = c("right", "bottom"), xmod = -0.5, size = 0.5, auto.placement = TRUE) +
  tm_shape(CLASSasso) +
    tm_bubbles(col = "blue", size = "Collège", title.size = "CLASS - nombre de collégiens pris en charge") +
#    tm_text(text = "ASSOCIATION", just = c("right", "bottom"), xmod = -0.5, size = 0.5, auto.placement = TRUE) +
  tm_shape(CLASSmuni) +
    tm_bubbles(col = "darkgreen", size = 0.1) +
#    tm_text(text = "ASSOCIATION", just = c("right", "bottom"), xmod = -0.5, size = 0.5) +
  tm_shape(projet_ado) +
   tm_symbols(shape = 17, col = "orange", size = 0.2) +
  tm_layout(title = "Dispositifs de soutien aux relations parents-école dans le 18e arrondissement", title.position = c("RIGHT", "TOP"), scale = 2, attr.outside.position = "bottom", attr.outside = TRUE) +
  tm_credits(text = "En vert : CLASS municipaux, qui ne prennent a priori pas en charge les collégiens.\nEn orange : projets Ados financés par la CAF. \nSource : CAF de Paris. Réalisation : École des données/OKF pour la CAF.\nMap tiles by Carto, under CC BY 3.0. Data by OpenStreetMap, under ODbL.") +
  tm_layout(legend.outside = TRUE, legend.outside.position = "bottom")
dev.off()
dev.off()


# export geo_json
REAAP <- st_as_sf(REAAP)
geojson_write(REAAP, file = "./geojson/REAAP18.geojson")

CLASSasso <- st_as_sf(CLASSasso)
geojson_write(CLASSasso, file = "./geojson/CLASSasso18.geojson")

CLASSmuni <- st_as_sf(CLASSmuni)
geojson_write(CLASSmuni, file = "./geojson/CLASSasso18.geojson")

projet_ado <- st_as_sf(projet_ado)
geojson_write(projet_ado, file = "./geojson/projet_ado18.geojson")
