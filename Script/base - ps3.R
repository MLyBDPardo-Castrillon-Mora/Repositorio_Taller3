# LIBRERIAS ====================================================================
library(caret)
library(dplyr)
library(leaflet)
library(MASS)
library(osmdata)
library(pacman)
library(plotly)
library(randomForest)
library(rgeos)
library(sf)
library(SuperLearner)
library(tidyverse)
library(tmaptools)


# DATOS ========================================================================
# Base de datos ----------------------------------------------------------------

# Semilla
set.seed(4040)

# Limite de memoria
memory.limit(100000)

# Importacion de datos
db <- read.csv("~/uniandes-bdml-20231-ps3/train.csv")
kaggle <- read.csv("~/uniandes-bdml-20231-ps3/test.csv")

# Split de datos
db$obs <- 1:nrow(db)
indices <- createDataPartition(y = db$obs, p = 0.8, list = FALSE)
train <- db[indices, ]
dev_set <- db[-indices, ]

indices <- createDataPartition(y = dev_set$obs, p = 0.5, list = FALSE)
test <- dev_set[indices, ]
dev_set <- dev_set[-indices, ]

# Ver base de datos
glimpse(db)

summary(db$price) %>%
  as.matrix() %>%
  as.data.frame() %>%
  mutate(V1 = scales::dollar(V1))

# Distribucion de precios
p <- ggplot(db, aes(x = price)) +
  geom_histogram(fill = "coral", alpha = 0.4) +
  labs(x = "Precio (log-scale)", y = "Frecuencia") +
  scale_x_log10(labels = scales::dollar) +
  theme_bw()
ggplotly(p)

# Expandir base de datos -------------------------------------------------------

parques <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "leisure" , value = "park") 
parques_sf <- osmdata_sf(parques)
parques_geometria <- parques_sf$osm_polygons %>% 
  select(osm_id, name)

gyms <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "leisure" , value = "fitness_centre") 
gyms_sf <- osmdata_sf(gyms)
gyms_geometria <- gyms_sf$osm_polygons %>% 
  select(osm_id, name)

hospitales <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity" , value = "hospital") 
hospitales_sf <- osmdata_sf(hospitales)
hospitales_geometria <- hospitales_sf$osm_polygons %>% 
  select(osm_id, name)

colegios <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity" , value = "school") 
colegios_sf <- osmdata_sf(colegios)
colegios_geometria <- colegios_sf$osm_polygons %>% 
  select(osm_id, name)

restaurantes <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity" , value = "restaurant") 
restaurantes_sf <- osmdata_sf(restaurantes)
restaurantes_geometria <- restaurantes_sf$osm_polygons %>% 
  select(osm_id, name)

bares <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity" , value = "bar") 
bares_sf <- osmdata_sf(bares)
bares_geometria <- bares_sf$osm_polygons %>% 
  select(osm_id, name)

tm <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "public_transport" , value = "station") 
tm_sf <- osmdata_sf(tm)
tm_geometria <- tm_sf$osm_polygons %>% 
  select(osm_id, name)

malls <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "shop" , value = "mall") 
malls_sf <- osmdata_sf(malls)
malls_geometria <- malls_sf$osm_polygons %>% 
  select(osm_id, name)

# Centroides -------------------------------------------------------------------

# Parques
centro_park <- gCentroid(as(parques_geometria$geometry, "Spatial"), byid = T)
leaflet() %>%
  addTiles() %>%
  addPolygons(data = parques_geometria, col = "green",
              opacity = 0.8, popup = parques_geometria$name) %>%
  addCircles(lng = centro_park$x, 
             lat = centro_park$y, 
             col = "red", opacity = 1, radius = 1)

# Gimnasios
centro_gym <- gCentroid(as(gyms_geometria$geometry, "Spatial"), byid = T)
leaflet() %>%
  addTiles() %>%
  addPolygons(data = gyms_geometria, col = "green",
              opacity = 0.8, popup = gyms_geometria$name) %>%
  addCircles(lng = centro_gym$x, 
             lat = centro_gym$y, 
             col = "red", opacity = 1, radius = 1)

# Hospitales
centro_hosp <- gCentroid(as(hospitales_geometria$geometry, "Spatial"), byid = T)
leaflet() %>%
  addTiles() %>%
  addPolygons(data = hospitales_geometria, col = "green",
              opacity = 0.8, popup = hospitales_geometria$name) %>%
  addCircles(lng = centro_hosp$x, 
             lat = centro_hosp$y, 
             col = "red", opacity = 1, radius = 1)

# Colegios
centro_school <- gCentroid(as(colegios_geometria$geometry, "Spatial"), byid = T)
leaflet() %>%
  addTiles() %>%
  addPolygons(data = colegios_geometria, col = "green",
              opacity = 0.8, popup = colegios_geometria$name) %>%
  addCircles(lng = centro_school$x, 
             lat = centro_school$y, 
             col = "red", opacity = 1, radius = 1)

# Restaurantes
centro_rest <- gCentroid(as(restaurantes_geometria$geometry, "Spatial"), byid = T)
leaflet() %>%
  addTiles() %>%
  addPolygons(data = restaurantes_geometria, col = "green",
              opacity = 0.8, popup = restaurantes_geometria$name) %>%
  addCircles(lng = centro_rest$x, 
             lat = centro_rest$y, 
             col = "red", opacity = 1, radius = 1)

# Bares
centro_bar <- gCentroid(as(bares_geometria$geometry, "Spatial"), byid = T)
leaflet() %>%
  addTiles() %>%
  addPolygons(data =bares_geometria, col = "green",
              opacity = 0.8, popup = bares_geometria$name) %>%
  addCircles(lng = centro_bar$x, 
             lat = centro_bar$y, 
             col = "red", opacity = 1, radius = 1)

# Transmilenio
centro_tm <- gCentroid(as(tm_geometria$geometry, "Spatial"), byid = T)
leaflet() %>%
  addTiles() %>%
  addPolygons(data =tm_geometria, col = "green",
              opacity = 0.8, popup = tm_geometria$name) %>%
  addCircles(lng = centro_tm$x, 
             lat = centro_tm$y, 
             col = "red", opacity = 1, radius = 1)

# Centros comerciales
centro_malls <- gCentroid(as(malls_geometria$geometry, "Spatial"), byid = T)
leaflet() %>%
  addTiles() %>%
  addPolygons(data =malls_geometria, col = "green",
              opacity = 0.8, popup = malls_geometria$name) %>%
  addCircles(lng = centro_malls$x, 
             lat = centro_malls$y, 
             col = "red", opacity = 1, radius = 1)

# Distancias -------------------------------------------------------------------
# Elegir centroides de apartamentos
db_sf <- st_as_sf(db, coords = c("lon", "lat"))
st_crs(db_sf) <- 4326

centro_park_sf <- st_as_sf(centro_park, coords = c("x", "y"))
centro_gym_sf <- st_as_sf(centro_gym, coords = c("x", "y"))
centro_hosp_sf <- st_as_sf(centro_hosp, coords = c("x", "y"))
centro_bar_sf <- st_as_sf(centro_bar, coords = c("x", "y"))
centro_malls_sf <- st_as_sf(centro_malls, coords = c("x", "y"))
centro_rest_sf <- st_as_sf(centro_rest, coords = c("x", "y"))
centro_school_sf <- st_as_sf(centro_school, coords = c("x", "y"))
centro_tm_sf <- st_as_sf(centro_tm, coords = c("x", "y"))

# Matrices de distancias
dist_park <- st_distance(x = db_sf, y = centro_park_sf)
dist_gym <- st_distance(x = db_sf, y = centro_gym_sf)
dist_hosp <- st_distance(x = db_sf, y = centro_hosp_sf)
dist_bar <- st_distance(x = db_sf, y = centro_bar_sf)
dist_malls <- st_distance(x = db_sf, y = centro_malls_sf)
dist_rest <- st_distance(x = db_sf, y = centro_rest_sf)
dist_school <- st_distance(x = db_sf, y = centro_school_sf)
dist_tm <- st_distance(x = db_sf, y = centro_tm_sf)

# Distancia minima
dmin_park <- apply(dist_park, 1, min)
dmin_gym <- apply(dist_gym, 1, min)
dmin_hosp <- apply(dist_hosp, 1, min)
dmin_bar <- apply(dist_bar, 1, min)
dmin_malls <- apply(dist_malls, 1, min)
dmin_rest <- apply(dist_rest, 1, min)
dmin_school <- apply(dist_school, 1, min)
dmin_tm <- apply(dist_tm, 1, min)

# Agregar a la base de datos
db$dmin_park <- dmin_park
db$dmin_gym <- dmin_gym
db$dmin_hosp <- dmin_hosp
db$dmin_bar <- dmin_bar
db$dmin_malls <- dmin_malls
db$dmin_rest <- dmin_rest
db$dmin_school <- dmin_school
db$dmin_tm <- dmin_tm

# Palabras Clave ---------------------------------------------------------------

# Garaje
garaje <- rep(0,nrow(db))
for (i in 1:nrow(db)) {
  if (grepl("(?i)\\b(garaje(s)?|parqueadero(s)?)\\b", db$description[i], perl=TRUE)) {
    garaje[i] <- 1
  }
}
db$garaje <- garaje

# Deposito
db$deposito <- rep(0,nrow(db))
for (i in 1:nrow(db)) {
  if (grepl("(?i)\\b(deposito(s)?)\\b", db$description[i], perl=TRUE)) {
    db$deposito[i] <- 1
  }
}

# Balcon o terraza
db$balcon <- rep(0,nrow(db))
for (i in 1:nrow(db)) {
  if (grepl("(?i)\\b(balcon|terraza)\\b", db$description[i], perl=TRUE)) {
    db$balcon[i] <- 1
  }
}

# Ascensor
db$ascensor <- rep(0,nrow(db))
for (i in 1:nrow(db)) {
  if (grepl("(?i)\\b(ascensor)\\b", db$description[i], perl=TRUE)) {
    db$ascensor[i] <- 1
  }
}


house_chapi <- st_intersection(x = db_sf, y = chapinero)

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = chapinero , col = "red") %>%
  addCircles(data = house_chapi)

ka_sf <- st_as_sf(kaggle, coords = c("lon", "lat"))
st_crs(ka_sf) <- 4326
ka_chapi <- st_intersection(x = ka_sf, y = chapinero)

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = chapinero , col = "coral") %>%
  addCircles(data = ka_chapi)


# MODELOS ======================================================================


# EXPORT =======================================================================

# Expandir Base de datos de Kaggle

# Distancias -------------------------------------------------------------------
# Elegir centroides de apartamentos
ka_sf <- st_as_sf(kaggle, coords = c("lon", "lat"))
st_crs(ka_sf) <- 4326

# Matrices de distancias
kdist_park <- st_distance(x = ka_sf, y = centro_park_sf)
kdist_gym <- st_distance(x = ka_sf, y = centro_gym_sf)
kdist_hosp <- st_distance(x = ka_sf, y = centro_hosp_sf)
kdist_bar <- st_distance(x = ka_sf, y = centro_bar_sf)
kdist_malls <- st_distance(x = ka_sf, y = centro_malls_sf)
kdist_rest <- st_distance(x = ka_sf, y = centro_rest_sf)
kdist_school <- st_distance(x = ka_sf, y = centro_school_sf)
kdist_tm <- st_distance(x = ka_sf, y = centro_tm_sf)

# Distancia minima
kdmin_park <- apply(kdist_park, 1, min)
kdmin_gym <- apply(kdist_gym, 1, min)
kdmin_hosp <- apply(kdist_hosp, 1, min)
kdmin_bar <- apply(kdist_bar, 1, min)
kdmin_malls <- apply(kdist_malls, 1, min)
kdmin_rest <- apply(kdist_rest, 1, min)
kdmin_school <- apply(kdist_school, 1, min)
kdmin_tm <- apply(kdist_tm, 1, min)

# Agregar a la base de datos
kaggle$dmin_park <- kdmin_park
kaggle$dmin_gym <- kdmin_gym
kaggle$dmin_hosp <- kdmin_hosp
kaggle$dmin_bar <- kdmin_bar
kaggle$dmin_malls <- kdmin_malls
kaggle$dmin_rest <- kdmin_rest
kaggle$dmin_school <- kdmin_school
kaggle$dmin_tm <- kdmin_tm

# Palabras Clave ---------------------------------------------------------------

# Garaje
kaggle$garaje <- rep(0,nrow(kaggle))
for (i in 1:nrow(kaggle)) {
  if (grepl("(?i)\\b(garaje(s)?|parqueadero(s)?)\\b", kaggle$description[i], perl=TRUE)) {
    kaggle$garaje[i] <- 1
  }
}

# Deposito
kaggle$deposito <- rep(0,nrow(kaggle))
for (i in 1:nrow(kaggle)) {
  if (grepl("(?i)\\b(deposito(s)?)\\b", kaggle$description[i], perl=TRUE)) {
    kaggle$deposito[i] <- 1
  }
}

# Balcon o terraza
kaggle$balcon <- rep(0,nrow(kaggle))
for (i in 1:nrow(kaggle)) {
  if (grepl("(?i)\\b(balcon|terraza)\\b", kaggle$description[i], perl=TRUE)) {
    kaggle$balcon[i] <- 1
  }
}

# Ascensor
kaggle$ascensor <- rep(0,nrow(kaggle))
for (i in 1:nrow(kaggle)) {
  if (grepl("(?i)\\b(ascensor)\\b", kaggle$description[i], perl=TRUE)) {
    kaggle$ascensor[i] <- 1
  }
}
