# LIBRERIAS ====================================================================
rm(list = ls())

library(pacman)
p_load(osmdata,
       MASS,
       leaflet,
       glmnet,
       gbm,
       dplyr,
       caret,
       plotly,
       randomForest,
       rgeos,
       sf,
       stargazer,
       SuperLearner,
       tidyverse,
       tmaptools,
       MLmetrics)

# DATOS ========================================================================
# Base de datos ----------------------------------------------------------------

# Semilla
set.seed(4040)

# Importacion de datos
db <- read.csv("./Stores/train.csv")
kaggle <- read.csv("./Stores/test.csv")

# Ver base de datos
glimpse(db)

summary(db$price) %>%
  as.matrix() %>%
  as.data.frame() %>%
  mutate(V1 = scales::dollar(V1))

# Distribucion de precios
hist_precios <- ggplot(db, aes(x = price)) +
  geom_histogram(fill = "coral", alpha = 0.4) +
  labs(x = "Precio (log-scale)", y = "Frecuencia") +
  scale_x_log10(labels = scales::dollar) +
  theme_bw()
ggsave("hist_precios.png", path = "./Views")

# Expandir base de datos -------------------------------------------------------

available_features()

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

policia <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity" , value = "police") 
policia_sf <- osmdata_sf(policia)
policia_geometria <- policia_sf$osm_polygons %>% 
  select(osm_id, name)

disco <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity" , value = "nightclub") 
disco_sf <- osmdata_sf(disco)
disco_geometria <- disco_sf$osm_polygons %>% 
  select(osm_id, name)

casino <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity" , value = "casino") 
casino_sf <- osmdata_sf(casino)
casino_geometria <- casino_sf$osm_polygons %>% 
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

# Policia
centro_policia <- gCentroid(as(policia_geometria$geometry, "Spatial"), byid = T)
leaflet() %>%
  addTiles() %>%
  addPolygons(data = policia_geometria, col = "green",
              opacity = 0.8, popup = policia_geometria$name) %>%
  addCircles(lng = centro_policia$x, 
             lat = centro_policia$y, 
             col = "red", opacity = 1, radius = 1)

# Discotecas
centro_disco <- gCentroid(as(disco_geometria$geometry, "Spatial"), byid = T)
leaflet() %>%
  addTiles() %>%
  addPolygons(data = disco_geometria, col = "green",
              opacity = 0.8, popup = disco_geometria$name) %>%
  addCircles(lng = centro_disco$x, 
             lat = centro_disco$y, 
             col = "red", opacity = 1, radius = 1)

# Casinos
centro_casino <- gCentroid(as(casino_geometria$geometry, "Spatial"), byid = T)
leaflet() %>%
  addTiles() %>%
  addPolygons(data = casino_geometria, col = "green",
              opacity = 0.8, popup = casino_geometria$name) %>%
  addCircles(lng = centro_casino$x, 
             lat = centro_casino$y, 
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
centro_policia_sf <- st_as_sf(centro_policia, coords = c("x", "y"))
centro_disco_sf <- st_as_sf(centro_disco, coords = c("x", "y"))
centro_casino_sf <- st_as_sf(centro_casino, coords = c("x", "y"))

# Matrices de distancias
dist_park <- st_distance(x = db_sf, y = centro_park_sf)
dist_gym <- st_distance(x = db_sf, y = centro_gym_sf)
dist_hosp <- st_distance(x = db_sf, y = centro_hosp_sf)
dist_bar <- st_distance(x = db_sf, y = centro_bar_sf)
dist_malls <- st_distance(x = db_sf, y = centro_malls_sf)
dist_rest <- st_distance(x = db_sf, y = centro_rest_sf)
dist_school <- st_distance(x = db_sf, y = centro_school_sf)
dist_tm <- st_distance(x = db_sf, y = centro_tm_sf)
dist_policia <- st_distance(x = db_sf, y = centro_policia_sf)
dist_disco <- st_distance(x = db_sf, y = centro_disco_sf)
dist_casino <- st_distance(x = db_sf, y = centro_casino_sf)

# Distancia minima
dmin_park <- apply(dist_park, 1, min)
dmin_gym <- apply(dist_gym, 1, min)
dmin_hosp <- apply(dist_hosp, 1, min)
dmin_bar <- apply(dist_bar, 1, min)
dmin_malls <- apply(dist_malls, 1, min)
dmin_rest <- apply(dist_rest, 1, min)
dmin_school <- apply(dist_school, 1, min)
dmin_tm <- apply(dist_tm, 1, min)
dmin_policia <- apply(dist_policia, 1, min)
dmin_disco <- apply(dist_disco, 1, min)
dmin_casino <- apply(dist_casino, 1, min)

# Agregar a la base de datos
db$dmin_park <- dmin_park
db$dmin_gym <- dmin_gym
db$dmin_hosp <- dmin_hosp
db$dmin_bar <- dmin_bar
db$dmin_malls <- dmin_malls
db$dmin_rest <- dmin_rest
db$dmin_school <- dmin_school
db$dmin_tm <- dmin_tm
db$dmin_policia <- dmin_policia
db$dmin_disco <- dmin_disco
db$dmin_casino <- dmin_casino

# Calcular densidades de distancias
park_density <- density(db$dmin_park)
gym_density <- density(db$dmin_gym)
hosp_density <- density(db$dmin_hosp)
bar_density <- density(db$dmin_bar)
malls_density <- density(db$dmin_malls)
rest_density <- density(db$dmin_rest)
tm_density <- density(db$dmin_school)

densidades <- data.frame(x = tm_density$x, park = park_density$y,
                         gym = gym_density$y,
                         hosp = hosp_density$y,
                         bar = bar_density$y,
                         malls = malls_density$y,
                         rest = rest_density$y,
                         tm = tm_density$y)

ggplot(data = densidades, aes(x = x)) +
  labs(x = "Distancia de apartamentos a distintas zonas urbanas", y = "Densidad") + theme_bw() +
  geom_line(aes(y = park, color = "Parques")) +
  geom_line(aes(y = gym, color = "Gimnasios")) +
  geom_line(aes(y = hosp, color = "Hospitales")) +
  geom_line(aes(y = bar, color = "Bares")) +
  geom_line(aes(y = malls, color = "C.C.")) +
  geom_line(aes(y = rest, color = "Restaurantes")) +
  geom_line(aes(y = tm, color = "Transmilenio")) +
  scale_color_manual(name = "Lugar", values = c("Parques" = "gray","Gimnasios"="red",
                                                "Hospitales" = "purple","Bares" = "darkblue",
                                                "C.C." = "black","Restaurantes" = "darkgreen",
                                                "Transmilenio" = "orange")) -> hist_ammen

scatter_p_tm <- ggplot(db, aes(x = dmin_park, y = price)) +
  geom_point(col = "darkblue", alpha = 0.1) +
  geom_smooth(col="lightblue", alpha = 0.8, se=F,method = "lm") + 
  labs(x = "Distancia mínima (log-scale)", 
       y = "Valor del inmueble (log-scale)") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggsave("scatter_p_tm.png",path = "./Views")

scatter_p_park <-ggplot(db, aes(x = dmin_park, y = price)) +
  geom_point(col = "darkblue", alpha = 0.1) +
  geom_smooth(col="lightblue", alpha = 0.8, se=F,method = "lm") + 
  labs(x = "Distancia mínima (log-scale)", 
       y = "Valor del inmueble (log-scale)") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggsave("scatter_p_park.png",path = "./Views")

scatter_p_rest <- ggplot(db, aes(x = dmin_rest, y = price)) +
  geom_point(col = "darkblue", alpha = 0.1) +
  geom_smooth(col="lightblue", alpha = 0.8, se=F,method = "lm") + 
  labs(x = "Distancia mínima (log-scale)", 
       y = "Valor del inmueble (log-scale)") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggsave("scatter_p_rest.png",path = "./Views")

scatter_p_bar <- ggplot(db, aes(x = dmin_bar, y = price)) +
  geom_point(col = "darkblue", alpha = 0.1) +
  geom_smooth(col="lightblue", alpha = 0.8, se=F,method = "lm") + 
  labs(x = "Distancia mínima (log-scale)", 
       y = "Valor del inmueble (log-scale)") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggsave("scatter_p_bar.png", path = "./Views")

scatter_p_mall <- ggplot(db, aes(x = dmin_malls, y = price)) +
  geom_point(col = "darkblue", alpha = 0.1) +
  geom_smooth(col="lightblue", alpha = 0.8, se=F,method = "lm") + 
  labs(x = "Distancia mínima (log-scale)", 
       y = "Valor del inmueble (log-scale)") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggsave("scatter_p_mall.png", path = "./Views")

scatter_p_hosp <- ggplot(db, aes(x = dmin_hosp, y = price)) +
  geom_point(col = "darkblue", alpha = 0.1) +
  geom_smooth(col="lightblue", alpha = 0.8, se=F,method = "lm") + 
  labs(x = "Distancia mínima (log-scale)", 
       y = "Valor del inmueble (log-scale)") +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  theme_bw()
ggsave("scatter_p_hosp.png", path = "./Views")

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

# Planta electrica
db$planta <- rep(0,nrow(db))
for (i in 1:nrow(db)) {
  if (grepl("(?i)\\b(planta|generador)\\b", db$description[i], perl=TRUE)) {
    db$planta[i] <- 1
  }
}

# Seguridad
db$seg <- rep(0,nrow(db))
for (i in 1:nrow(db)) {
  if (grepl("(?i)\\b(vigilancia|seguridad)\\b", db$description[i], perl=TRUE)) {
    db$seg[i] <- 1
  }
}

# Vista
db$vista <- rep(0,nrow(db))
for (i in 1:nrow(db)) {
  if (grepl("(?i)\\b(vista)\\b", db$description[i], perl=TRUE)) {
    db$vista[i] <- 1
  }
}

# Conjunto
db$conjunto <- rep(0,nrow(db))
for (i in 1:nrow(db)) {
  if (grepl("(?i)\\b(conjunto)\\b", db$description[i], perl=TRUE)) {
    db$conjunto[i] <- 1
  }
}

# Chimenea
db$chimenea <- rep(0,nrow(db))
for (i in 1:nrow(db)) {
  if (grepl("(?i)\\b(chimenea)\\b", db$description[i], perl=TRUE)) {
    db$chimenea[i] <- 1
  }
}

# Smart Building
db$smart <- rep(0,nrow(db))
for (i in 1:nrow(db)) {
  if (grepl("(?i)\\b(smart|inteligente)\\b", db$description[i], perl=TRUE)) {
    db$smart[i] <- 1
  }
}

# Area
db$area <- rep(0,nrow(db))
for (i in 1:nrow(db)) {
  if (grepl("(?i)\\b(m|area|m2|mt2|mts|mts2|metro|metros)\\b", db$description[i], perl=TRUE)) {
    db$area[i] <- 1
  }
}
db$area <- ifelse(grepl("\\d+(m|area|m2|mts|mts2|metro|metros)", db$description), 1, db$area)

db$area_number <- sapply(
  str_extract_all(db$description, 
                  "\\d+(\\.\\d+)?\\s?(m|m2|mts|mtrs|mts2|metro|metros)"), 
  function(x) {
    if (length(x) > 0) {
      max(as.numeric(gsub("\\D", "", x)))
    } else {
      NA
    }
  })
db$area_number <- ifelse(db$area_number < 45, NA, db$area_number)

db$casa <- rep(0,nrow(db))
db$casa <- ifelse (db$property_type == "Casa", 1, 0)

m.aux <- randomForest (area_number ~ balcon + ascensor + casa + planta + vista
                       + chimenea + bedrooms, data = db, ntree = 500, 
                       na.action = na.omit)
m_aux <- predict(m.aux, newdata = db)
db$area_number <- ifelse(is.na(db$area_number==TRUE), m_aux[i], db$area_number)

# Dummies ----------------------------------------------------------------------

# Dummy de casa
db$casa <- rep(0,nrow(db))
db$casa <- ifelse (db$property_type == "Casa", 1, 0)

# Dummies de trimestre y ano
db$tr1 <- rep(0,nrow(db))
db$tr1 <- ifelse (db$month == "1"|db$month == "2"|db$month == "3", 1, 0)

db$tr2 <- rep(0,nrow(db))
db$tr2 <- ifelse (db$month == "4"|db$month == "5"|db$month == "6", 1, 0)

db$tr3 <- rep(0,nrow(db))
db$tr3 <- ifelse (db$month == "7"|db$month == "8"|db$month == "9", 1, 0)

db$tr4 <- rep(0,nrow(db))
db$tr4 <- ifelse (db$month == "10"|db$month == "11"|db$month == "12", 1, 0)

db$y_19 <- rep(0,nrow(db))
db$y_19 <- ifelse (db$year == "2019", 1, 0)

db$y_20 <- rep(0,nrow(db))
db$y_20 <- ifelse (db$year == "2020", 1, 0)

db$y_21 <- rep(0,nrow(db))
db$y_21 <- ifelse (db$year == "2021", 1, 0)

# MISC -------------------------------------------------------------------------
bogota <- getbb(place_name = "UPZ Chapinero, Bogota", 
                   featuretype = "boundary:administrative", 
                   format_out = "sf_polygon") %>% .$multipolygon

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
  addCircles(lng = centro_policia$x, 
             lat = centro_policia$y, 
             col = "red", opacity = 1, radius = 1)


# MODELOS ======================================================================

# Regresion lineal

model.1 <- glm(price ~ bedrooms + dmin_bar + dmin_gym + dmin_hosp + 
                 I(dmin_hosp^2) + dmin_malls + dmin_park + dmin_rest 
               + dmin_school + dmin_tm + I(dmin_tm^2) + dmin_casino 
               + dmin_disco + dmin_policia + I(dmin_policia^2)
               + garaje + deposito + balcon + ascensor + casa 
               + planta + seg + conjunto + vista + chimenea 
               + smart, data = db)

summary(model.1)
m1 <- predict(model.1)
scales::dollar(MAE(y_pred = m1, y_true = db$price))
scales::dollar(RMSE(y_pred = m1, y_true = db$price))

# RIDGE 

# Control
ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 3, 
                     verboseIter = FALSE)
# Grid
grid <- expand.grid(alpha = seq(0, 1, by = 0.1),
                    lambda = seq(0, 1, by = 0.1))

# Ridge
model.2 <- train(price ~ dmin_bar + dmin_gym + dmin_hosp + dmin_malls + dmin_park +
                   dmin_rest + dmin_school + dmin_tm + garaje + deposito + balcon +
                   ascensor + casa, 
                 data = db, 
                 method = "glmnet", 
                 trControl = ctrl, 
                 tuneGrid = grid, 
                 standardize = TRUE)

print(model.2)
m2 <- predict(model.2)
scales::dollar(MAE(y_pred = m2, y_true = db$price))
scales::dollar(RMSE(y_pred = m2, y_true = db$price))

# Random Forests
model.3 <- randomForest (price ~ bedrooms + dmin_bar + dmin_gym + dmin_hosp + 
                           + dmin_malls + dmin_park + dmin_rest + dmin_school 
                         + dmin_tm + dmin_casino + dmin_disco + dmin_policia
                         + garaje + deposito + balcon + ascensor + casa 
                         + planta + seg + conjunto + vista + chimenea 
                         + smart, data = db, ntree = 100)

m3 <- predict(model.3)
scales::dollar(MAE(y_pred = m3, y_true = db$price))
scales::dollar(RMSE(y_pred = m3, y_true = db$price))

model.4 <- randomForest(price ~ bedrooms + dmin_bar + dmin_gym + dmin_hosp + 
                           I(dmin_hosp^2) + dmin_malls + dmin_park + dmin_rest 
                         + dmin_school + dmin_tm + I(dmin_tm^2) + dmin_casino 
                         + dmin_disco + dmin_policia + I(dmin_policia^2)
                         + garaje + deposito + balcon + ascensor + casa 
                         + planta + seg + conjunto + vista + chimenea 
                         + smart, data = db, ntree = 250)

m4 <- predict(model.4)
scales::dollar(MAE(y_pred = m4, y_true = db$price))
scales::dollar(RMSE(y_pred = m4, y_true = db$price))


# Boosting
model.5 <- gbm(price ~ bedrooms + dmin_bar + dmin_gym + dmin_hosp + 
                 I(dmin_hosp^2) + dmin_malls + dmin_park + dmin_rest 
               + dmin_school + dmin_tm + I(dmin_tm^2) + dmin_casino 
               + dmin_disco + dmin_policia + I(dmin_policia^2)
               + garaje + deposito + balcon + ascensor + casa 
               + planta + seg + conjunto + vista + chimenea 
               + smart, data = db, n.trees = 250, interaction.depth = 3,
               shrinkage = 0.1, distribution = "gaussian")

m5 <- predict(model.5)
scales::dollar(MAE(y_pred = m5, y_true = db$price))
scales::dollar(RMSE(y_pred = m5, y_true = db$price))



# SUPER-LEARNER ================================================================

# Definir learners y precio
x <- db  %>% select(bedrooms, dmin_bar, dmin_gym, dmin_hosp, dmin_malls, 
                    dmin_park, dmin_rest, dmin_school, dmin_tm, dmin_casino, 
                    dmin_disco, dmin_policia, garaje, deposito, balcon,
                    ascensor, casa, planta, seg, conjunto, vista, chimenea, smart)
y <- db$price

# Super learner
sl.lib <- c("SL.randomForest", "SL.lm")

fit_Y <- SuperLearner(Y = y,  X= x, 
                      method = "method.NNLS",
                      SL.library = sl.lib)


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
kdist_policia <- st_distance(x = ka_sf, y = centro_policia_sf)
kdist_casino <- st_distance(x = ka_sf, y = centro_casino_sf)
kdist_disco <- st_distance(x = ka_sf, y = centro_disco_sf)

# Distancia minima
kdmin_park <- apply(kdist_park, 1, min)
kdmin_gym <- apply(kdist_gym, 1, min)
kdmin_hosp <- apply(kdist_hosp, 1, min)
kdmin_bar <- apply(kdist_bar, 1, min)
kdmin_malls <- apply(kdist_malls, 1, min)
kdmin_rest <- apply(kdist_rest, 1, min)
kdmin_school <- apply(kdist_school, 1, min)
kdmin_tm <- apply(kdist_tm, 1, min)
kdmin_policia <- apply(kdist_policia, 1, min)
kdmin_disco <- apply(kdist_disco, 1, min)
kdmin_casino <- apply(kdist_casino, 1, min)

# Agregar a la base de datos
kaggle$dmin_park <- kdmin_park
kaggle$dmin_gym <- kdmin_gym
kaggle$dmin_hosp <- kdmin_hosp
kaggle$dmin_bar <- kdmin_bar
kaggle$dmin_malls <- kdmin_malls
kaggle$dmin_rest <- kdmin_rest
kaggle$dmin_school <- kdmin_school
kaggle$dmin_tm <- kdmin_tm
kaggle$dmin_policia <- kdmin_policia
kaggle$dmin_casino <- kdmin_casino
kaggle$dmin_disco <- kdmin_disco

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

# Dummy de casa
kaggle$casa <- rep(0,nrow(kaggle))
kaggle$casa <- ifelse (kaggle$property_type == "Casa", 1, 0)

# Planta electrica
kaggle$planta <- rep(0,nrow(kaggle))
for (i in 1:nrow(kaggle)) {
  if (grepl("(?i)\\b(planta|generador)\\b", kaggle$description[i], perl=TRUE)) {
    kaggle$planta[i] <- 1
  }
}

# Seguridad
kaggle$seg <- rep(0,nrow(kaggle))
for (i in 1:nrow(kaggle)) {
  if (grepl("(?i)\\b(vigilancia|seguridad)\\b", kaggle$description[i], perl=TRUE)) {
    kaggle$seg[i] <- 1
  }
}

# Vista
kaggle$vista <- rep(0,nrow(kaggle))
for (i in 1:nrow(kaggle)) {
  if (grepl("(?i)\\b(vista)\\b", kaggle$description[i], perl=TRUE)) {
    kaggle$vista[i] <- 1
  }
}

# Conjunto
kaggle$conjunto <- rep(0,nrow(kaggle))
for (i in 1:nrow(kaggle)) {
  if (grepl("(?i)\\b(conjunto)\\b", kaggle$description[i], perl=TRUE)) {
    kaggle$conjunto[i] <- 1
  }
}

# Chimenea
kaggle$chimenea <- rep(0,nrow(kaggle))
for (i in 1:nrow(kaggle)) {
  if (grepl("(?i)\\b(chimenea)\\b", kaggle$description[i], perl=TRUE)) {
    kaggle$chimenea[i] <- 1
  }
}

# Smart Building
kaggle$smart <- rep(0,nrow(kaggle))
for (i in 1:nrow(kaggle)) {
  if (grepl("(?i)\\b(smart|inteligente)\\b", kaggle$description[i], perl=TRUE)) {
    kaggle$smart[i] <- 1
  }
}

# Area
kaggle$smart <- rep(0,nrow(kaggle))
for (i in 1:nrow(kaggle)) {
  if (grepl("(?i)\\b(area)\\b", kaggle$description[i], perl=TRUE)) {
    kaggle$smart[i] <- 1
  }
}

# Dummies de trimestre y ano
kaggle$tr1 <- rep(0,nrow(kaggle))
kaggle$tr1 <- ifelse (kaggle$month == "1"|kaggle$month == "2"|kaggle$month == "3", 1, 0)

kaggle$tr2 <- rep(0,nrow(kaggle))
kaggle$tr2 <- ifelse (kaggle$month == "4"|kaggle$month == "5"|kaggle$month == "6", 1, 0)

kaggle$tr3 <- rep(0,nrow(kaggle))
kaggle$tr3 <- ifelse (kaggle$month == "7"|kaggle$month == "8"|kaggle$month == "9", 1, 0)

kaggle$tr4 <- rep(0,nrow(kaggle))
kaggle$tr4 <- ifelse (kaggle$month == "10"|kaggle$month == "11"|kaggle$month == "12", 1, 0)

kaggle$y_19 <- rep(0,nrow(kaggle))
kaggle$y_19 <- ifelse (kaggle$year == "2019", 1, 0)

kaggle$y_20 <- rep(0,nrow(kaggle))
kaggle$y_20 <- ifelse (kaggle$year == "2020", 1, 0)

kaggle$y_21 <- rep(0,nrow(kaggle))
kaggle$y_21 <- ifelse (kaggle$year == "2021", 1, 0)


export <- as.data.frame(kaggle$property_id)
export$price <- predict(model.4, newdata = kaggle)
colnames(export)[1] <- "property_id"
write.csv(export, "./Stores/prueba_4.csv", row.names = FALSE)