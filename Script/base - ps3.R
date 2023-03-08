# LIBRERIAS ====================================================================
rm(list = ls())

library(pacman)
p_load(caret, # Clasificación y regresión
       tidyverse, # Manejo de Datos
       leaflet, # Mapas interactivos
       MASS, 
       osmdata, # Street Map Data
       plotly, # Mapeo y graficos
       randomForest, # modetosRandom Forest
       rgeos, # Regex
       sf, # Objetos Espaciales
       SuperLearner, #Modelo Super Learner
       tmaptools, # Herramientas de mapeo
       stargazer) #Tablas de regresiones

# DATOS ========================================================================
# Base de datos ----------------------------------------------------------------

# Semilla
set.seed(4040)

# Limite de memoria
memory.limit(100000)

# Importacion de datos
db <- read.csv("./Stores/train.csv")
kaggle <- read.csv("./Stores/test.csv")

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
hist_precios <- ggplot(db, aes(x = price)) +
  geom_histogram(fill = "coral", alpha = 0.4) +
  labs(x = "Precio (log-scale)", y = "Frecuencia") +
  scale_x_continuous(labels = scales::dollar) +
  theme_bw()
ggplotly(hist_precios)

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

chapinero <- getbb(place_name = "UPZ Chapinero, Bogota",
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
  addCircles(data = ka_chapi)


# MODELOS ======================================================================
db_temp <- db
# NA de surface
db_temp$property_type<-factor(db_temp$property_type, levels=c("Apartamento","Casa"), labels = c(0,1))
db_temp$property_type<-as.numeric(db_temp$property_type)
aux <- subset(db, select = c("surface_total", "property_type", "bedrooms", "balcon", "ascensor","deposito","garaje", "dmin_tm","dmin_hosp","dmin_park"))
lm_model <- rpart(surface_total ~ .
               + I(bedrooms^2)+ I(bedrooms^3)+ I(bedrooms^4)+ I(bedrooms^5)
               + I(dmin_tm^2)+ I(dmin_tm^3)+ I(dmin_tm^4)+ I(dmin_tm^5)
               + I(dmin_hosp^2)+ I(dmin_hosp^3)+ I(dmin_hosp^4)+ I(dmin_hosp^5), data = aux)
aux$surface[is.na(aux$surface)] <- predict(lm_model, newdata = aux[is.na(aux$surface), ])
db_temp$surface <- aux$surface
summary(db_temp$surface)
summary(db_temp$surface_total)

#Super Learner ===============================================================
Ysl <- db$price
Xsl <- db %>% select(dmin_bar)#dmin_gym,dmin_hosp,dmin_tm,garaje,ascensor)

Modelos <- c("SL.randomForest", "SL.lm")

fitY <- SuperLearner(Y = Ysl,  X= data.frame(Xsl),
                     method = "method.NNLS", # combinación convexa
                     SL.library = Modelos)

# EXPORT =======================================================================
ggsave("hist_precios.png", path = "./Views")
ggsave("hist_ammen.png", path="./Views")

stargazer(db[c("bedrooms","rooms","bathrooms","surface_total","surface_covered","price")], digits=1,
          covariate.labels = c("Dormitorios","Habitaciones","Baños","Área Total","Área Cubierta","precio"),
          summary.stat = c("n","mean","sd","min","p25","median","p75","max"),
          type = "latex", title = "Estadisticas Descriptivas")

stargazer(db[c("price")],summary.stat = c("n","mean","sd","min","p25","median","p75","max"), type="latex",
          flip=T, title = "Variable Respuesta",digits=0)
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
