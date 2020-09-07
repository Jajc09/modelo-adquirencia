rm(list=ls())
gc(T)

#________________________________________________________________________________________________________________________________________________
library(dplyr)
library(data.table)
library(tidyverse)
library(osmdata)
library(sf)
library(ggmap)
library(reshape2)
library(osmar)
#________________________________________________________________________________________________________________________________________________


#________________________________________________________________________________________________________________________________________________
# Creación del listado de centroides para todos los poligonos de Bogotá
#________________________________________________________________________________________________________________________________________________
longitud <- vector()
latitud <- vector()

# Extracción de los centroides de cada uno de los poligonos de Bogotá
for(i in 1:20634){
  cat("i:",i,"\n")
  longitud[i] <- dat@polygons[[i]]@Polygons[[1]]@labpt[1]
  latitud[i] <- dat@polygons[[i]]@Polygons[[1]]@labpt[2]
}

# data.frame con la siguiente información: id del poligono, longitud y latitud
centroides <- data_frame(1:20634,longitud,latitud)
centroides

# El objeto centroides corresponde al archivo ubicaciones_hexagonos.RDS
#________________________________________________________________________________________________________________________________________________


#________________________________________________________________________________________________________________________________________________
# Extracción de la información de OpenStreetMap haciendo uso la libreria osmar y el listado
# de centroides
#________________________________________________________________________________________________________________________________________________
# Listado de centroides de los hexagones de tamaño 100 de Bogotá
ubicaciones_hex <- readRDS("ubicaciones_hexagonos.RDS")

# OSM API version 0.6 data source; see http://wiki.openstreetmap.org/wiki/API_v0.6.
src <- osmsource_api(url = "https://api.openstreetmap.org/api/0.6/")

# Objeto tipo lista cuyas entradas son la información de cada polígono. 
osm_raw_info <- list()

# for para obtener la información de todos los poligonos 
for (i in 1:nrow(ubicaciones_hex)){
  bb <- center_bbox(ubicaciones_hex[i,2], ubicaciones_hex[i,3], 250, 250) 
  ua <- get_osm(bb, source = src)
  objects <- ua[[1]][2]$tags
  osm_raw_info[[i]] <- objects
  cat("i:",i,"\n")
}

# El nombre de cada entrada del objeto tipo lista corresponde al id de º
names(osm_raw_info) <- as.character(seq(1, nrow(ubicaciones_hex), 1))
saveRDS(object = osm_raw_info, "osm_raw_info.RDS")
#________________________________________________________________________________________________________________________________________________


#________________________________________________________________________________________________________________________________________________
# Limpieza de la base de datos osm_raw_info.RDS
#________________________________________________________________________________________________________________________________________________
osm_raw_info <- readRDS("~/adquirencia/osm_raw_info.RDS")


dd <- osm_raw_info[[1]]
interes_cat <- c("amenity","shop","leisure","building","healthcare")


for(i in 1:length(osm_raw_info)){
  cat("i:",i,"\n")
  dd_2 <- osm_raw_info[[i]]
  setDT(dd_2)
  dd_2 <- dd_2[k %in% interes_cat,]
  dd_2$pol <- i
  dd <- bind_rows(dd,dd_2)
}


base_comp <- dd
setDT(base_comp)

#________________________________________________________________________________________________________________________________________________
amenity <- c("bar","fuel","restaurant","college","atm","parking","fast_food","cafe","pharmacy","veterinary","bank","post_office","marketplace",
             "hospital","clinic","payment_terminal","police","payment_centre","Paga Todo","shop","restaurant;fast_food","restaurant;bank")
shop <- c('convenience','bakery','butcher','supermarket','yes','department_store',
          'dairy','baby_goods','sports','pet','kiosk','alcohol',
          'greengrocer','supermarket;radiotechnics','mall','grocery','food','medical_supply','supermaket',
          'funeral_directors','de_barrio','Plaza de mercado Tunjuelito','Plaza de mercado San Benito')
leisure <- c('park','dog_park','sport','stadium')
building <- c('yes','house','commercial','apartments','Banco Davivienda','public','residential',
              'university','church','office','industrial','retail','kiosk','chapel',
              'Oficina Central','hospital','warehouse','college')
tourism <- c('apartment','motel','hotel','guest_house','hostel','museum')
healtcare <- c('pharmacy','doctor','clinic','hospital')
base_comp <- base_comp[ v %in% c(amenity,shop,leisure,building,tourism,healtcare)]
#________________________________________________________________________________________________________________________________________________
publicos <- c('park','stadium','public','church','chapel','museum')
tiendas <- c("yes","shop",'sports','sport')
restaurante <- c("bar","restaurant","fast_food","cafe","restaurant;fast_food","restaurant;bank")
servicios <- c("fuel","parking","post_office","police",'funeral_directors','motel','hotel','guest_house','hostel')
instituto <- c("college",'university','college')
atm <- c("atm","payment_terminal","payment_centre","Paga Todo")
farmacias <- c("pharmacy",'medical_supply','pharmacy')
pets <- c("veterinary",'pet','dog_park')
bancos <- c("bank",'Banco Davivienda')
supermercados <- c("marketplace",'convenience','supermarket','department_store','supermarket;radiotechnics','mall','supermaket')
hospitalarios <- c("hospital","clinic",'hospital','doctor','clinic','hospital')
pasteleria <- c("bakery")
tiendas_barrio <- c('butcher','dairy','baby_goods','kiosk','alcohol','greengrocer','grocery','food','de_barrio','Plaza de mercado Tunjuelito',
                    'Plaza de mercado San Benito','retail','kiosk',"commercial")
otros <- c('yes','office','industrial','Oficina Central','warehouse')
residencial <- c('house','apartments','residential','apartment')
#________________________________________________________________________________________________________________________________________________
base_comp[,':='(categoria = case_when(v %in% publicos ~ "publicos",
                                      v %in% tiendas ~ "tiendas",
                                      v %in% restaurante ~ "restaurante",
                                      v %in% servicios ~ "servicios",
                                      v %in% instituto ~ "instituto",
                                      v %in% atm ~ "atm",
                                      v %in% farmacias ~ "farmacias",
                                      v %in% pets ~ "pets",
                                      v %in% bancos ~ "bancos",
                                      v %in% supermercados ~ "supermercados",
                                      v %in% hospitalarios ~ "hospitalarios",
                                      v %in% pasteleria ~ "pasteleria",
                                      v %in% tiendas_barrio ~ "tiendas_barrio",
                                      v %in% otros ~ "otros",
                                      v %in% residencial ~ "residencial",
                                      TRUE ~ "otros"))]
base_comp[,':='(k=NULL,v=NULL)]
d <- base_comp[,.(n=.N),by=id][order(-n)]
base_comp_2 <- base_comp[id %in% d[n<5,]$id,][,head(.SD,1),by=id]
pol_cat <- base_comp_2[,.(cantidad=.N),by=.(pol,categoria)]
pol_cat2 <- dcast(pol_cat, pol~categoria,value.var = "cantidad")

saveRDS(pol_cat2,"externas/osm/osm_variables.RDS")
#________________________________________________________________________________________________________________________________________________
