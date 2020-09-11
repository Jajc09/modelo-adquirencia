#___________________________________________________________________________________________
# remove all the objects in global environment 
#___________________________________________________________________________________________
rm(list=ls())
#___________________________________________________________________________________________



#___________________________________________________________________________________________
# garbage collector automatically releases memory when an object is no longer used 
#___________________________________________________________________________________________
gc()
#___________________________________________________________________________________________


#___________________________________________________________________________________________
# load libraries 
#___________________________________________________________________________________________
library(plyr)
library(data.table)
library(dummies)
library(VIM)
library(xgboost)
#library(h2o)
library(ROCR)
library(dplyr)
library(data.table)
library(pROC)
library(sp)
library(raster)
library(geojsonio)
library(mapview)
library(caret)
#___________________________________________________________________________________________

final_todos_v2 <- readRDS("/Users/jjimenez/Documents/Trabajo/CredibanCo/Adquirencia/Datos/final_todos_v2.RDS")
final_todos_v2 <- final_todos_v2[,-c("CU")]

#___________________________________________________________________________________________
############Lectura de la subdivisión creada anterior###############
#___________________________________________________________________________________________
Poly <- geojson_read("barrios_catastrales/bogota_subdivi_100.geojson", what = "sp")
#___________________________________________________________________________________________

#___________________________________________________________________________________________
#View map by division of 100 
#___________________________________________________________________________________________
mapview(Poly)
#___________________________________________________________________________________________


#___________________________________________________________________________________________
# original X
#___________________________________________________________________________________________

M_X<-readRDS("Matriz_X_modelo.RDS")
M_X_copy<-M_X
#___________________________________________________________________________________________


#___________________________________________________________________________________________
####################### Alinear coordenadas ###########
#___________________________________________________________________________________________
coordinates(M_X_copy) <- ~ LONG + LAT
proj4string(M_X_copy) <- proj4string(Poly)
#___________________________________________________________________________________________


#___________________________________________________________________________________________
##############Generar tabla con los puntos encontrados ###############
#___________________________________________________________________________________________
M_X$ID_Poly<-over(M_X_copy, Poly)  # Polygon using hexagons of size 100
M_X 
M_X<-M_X[!is.na(ID_Poly)]   #stores off the map are removed 
#___________________________________________________________________________________________


#___________________________________________________________________________________________
# ID generation by week and polygon
M_X[,IDX:=paste0(Semana,"_",ID_Poly)]

# ID generation by week, polygon and super APP category
M_X[,IDX1:=paste0(Semana,"_",ID_Poly,"_",CATEGORIA_SUPER_APP)]
#___________________________________________________________________________________________


#__________________________________________________________________________________________
# Creación de la variable crecimiento
#__________________________________________________________________________________________
crecimiento <- M_X[,c("Dia1","Dia2","Dia3","Dia4","Dia5","Dia6","Dia7","Dia8","Dia9","Dia10","Dia11","Dia12","Dia13","Dia14")]

crecimiento <- crecimiento[,.(dif1 = Dia1 - Dia2,
                              dif2 = Dia2 - Dia3,
                              dif3 = Dia3 - Dia4,
                              dif4 = Dia4 - Dia5,
                              dif5 = Dia5 - Dia6,
                              dif6 = Dia6 - Dia7,
                              dif7 = Dia7 - Dia8,
                              dif8 = Dia8 - Dia9,
                              dif9 = Dia9 - Dia10,
                              dif10 = Dia10 - Dia11,
                              dif11 = Dia11 - Dia12, 
                              dif12 = Dia12 - Dia13,
                              dif13 = Dia13 - Dia14)]

crecimiento <- sign(crecimiento)
crecimiento <- crecimiento[,.(crecimiento = sign(rowSums(x = crecimiento)) * -1)]
M_X$crecimiento <- crecimiento
M_X$crecimiento <- ifelse(M_X$crecimiento == -1 | M_X$crecimiento == 0, 0, 1)
#__________________________________________________________________________________________




#__________________________________________________________________________________________
# creation of dummy variables for Tipo_comercio and RED
#__________________________________________________________________________________________
M_X<-dummies::dummy.data.frame(data = M_X,names = c("Tipo_comercio", "RED"),sep="_")%>%setDT()
#__________________________________________________________________________________________


#__________________________________________________________________________________________
# New variables grouping by week and polygon
#__________________________________________________________________________________________
M_X_IDX<-M_X[,list(Facturacion_Total_Pol=sum(Facturacion),Trx_Total_Pol=sum(TRX),Tarj_Total_Pol=sum(Tarjetas),Avg_Tarj_Pol=mean(Tarjetas),
                     Num_Comercio_Pol=uniqueN(CU), Num_Categoria_Pol=uniqueN(CATEGORIA_SUPER_APP), Num_Credibanco_Pol = sum(RED_Credibanco),
                   Num_Redeban_Pol = sum(RED_Redeban), Num_Dual_Pol = sum(RED_Dual), Num_Antiguo_Pol = sum(Tipo_comercio_Antiguo), 
                   Num_Nuevo_Pol = sum(Tipo_comercio_Nuevo),Var_Tarjetas_Pol =  sum(Tarjetas)/sum(TRX), Porc_Crecimiento_Pol = sum(crecimiento)/.N),by=c("IDX")]
#__________________________________________________________________________________________


#__________________________________________________________________________________________
# New variables grouping by week, polygon and super APP category 
#__________________________________________________________________________________________
M_X_IDX1<-M_X[,list(Facturacion_Total_Pol_Cat=sum(Facturacion),Trx_Total_Pol_Cat=sum(TRX),Tarj_Total_Pol_Cat=sum(Tarjetas),Avg_Tarj_Pol_Cat=mean(Tarjetas),
                     Num_Comercio_Pol_Cat=uniqueN(CU), Num_Credibanco_Pol_Cat = sum(RED_Credibanco),
                    Num_Redeban_Pol_Cat = sum(RED_Redeban), Num_Dual_Pol_Cat = sum(RED_Dual), Num_Antiguo_Pol_Cat = sum(Tipo_comercio_Antiguo), 
                    Num_Nuevo_Pol_Cat = sum(Tipo_comercio_Nuevo), Var_Tarjetas_Pol_Cat = sum(Tarjetas)/sum(TRX), Porc_Crecimiento_Pol = sum(crecimiento)/.N),by=c("IDX1")]
#__________________________________________________________________________________________


#__________________________________________________________________________________________
# original Y
#__________________________________________________________________________________________
Y<-readRDS("y_con_pol.RDS")
Y_INFO<-readRDS("y_model.RDS")
Y_INFO<-Y_INFO[,.(CU,CATEGORIA_SUPER_APP)]  # selection of distinct values for CU and CATEGORIA_SUPER_APP 
Y<-merge(Y,Y_INFO,by="CU")   # merge of Y and Y_INFO by CU

Y[, Y_Total := rowSums(.SD,na.rm = T), .SDcols = 8:37] #sub-set data.table (.SD) 

Y[,Y_Total_Target:=ifelse(Y_Total >= 10,1,0)] #ifelse function ifelse(condition, value_true, value_false)
table(Y$Y_Total_Target) # total of 0's and 1's for Y_Total_Target 
Y<-Y[,.(CU,Semana,LONG,LAT,CATEGORIA_SUPER_APP,Y_Total_Target)] # values of columns CU, Semana, LONG, LAT, CATEGORIA_SUPER_APP and Y_Total_Target
Y_copy<-Y
#__________________________________________________________________________________________


#__________________________________________________________________________________________
# Alinear coordenadas
#__________________________________________________________________________________________
coordinates(Y_copy) <- ~ LONG + LAT
proj4string(Y_copy) <- proj4string(Poly)
#__________________________________________________________________________________________

#__________________________________________________________________________________________
# Generar tabla con los puntos encontrados
#__________________________________________________________________________________________
Y$ID_Poly<-over(Y_copy, Poly)
Y
Y<-Y[!is.na(ID_Poly)]
#__________________________________________________________________________________________


#__________________________________________________________________________________________
# ID generation by week and polygon
Y[,IDX:=paste0(Semana,"_",ID_Poly)]
# ID generation by week, polygon and super APP category 
Y[,IDX1:=paste0(Semana,"_",ID_Poly,"_",CATEGORIA_SUPER_APP)]
Y
#__________________________________________________________________________________________


#__________________________________________________________________________________________
# convert CATEGORIA_SUPER_APP to dummy variable and set as data.frame 
#__________________________________________________________________________________________
combin <- dummy.data.frame(Y, names = c("CATEGORIA_SUPER_APP"), sep = "_")%>%setDT()
# merge by IDX
combin<-merge(M_X_IDX,combin,by="IDX",all.y = T)


#__________________________________________________________________________________________
# variables selection for model and  merge by IDX1
#__________________________________________________________________________________________
BASE_MODELO <- combin[,.(IDX,IDX1,CU,CATEGORIA_SUPER_APP_SUPERMERCADOS,CATEGORIA_SUPER_APP_RESTAURANTES,
                         CATEGORIA_SUPER_APP_DROGUERIAS,CATEGORIA_SUPER_APP_HOGAR,CATEGORIA_SUPER_APP_MASCOTAS,CATEGORIA_SUPER_APP_EVENTOS,
                         Facturacion_Total_Pol, Trx_Total_Pol, Tarj_Total_Pol,Avg_Tarj_Pol, Num_Comercio_Pol, Num_Categoria_Pol, Num_Credibanco_Pol, Num_Redeban_Pol,
                         Num_Dual_Pol, Num_Antiguo_Pol, Num_Nuevo_Pol, Var_Tarjetas_Pol,
                         Y_Total_Target)]     # super APP categories were chosen because of importance in polygon and previous experience
FINAL<-merge(M_X_IDX1,BASE_MODELO,by="IDX1",all.y = T)
FINAL_F<-FINAL[,c("IDX","IDX1","CU"):=NULL]
#__________________________________________________________________________________________


#__________________________________________________________________________________________
# set seed and partition in train and test subsets
#__________________________________________________________________________________________
set.seed(98)
inx<-sample(seq(1,2),size=nrow(FINAL_F),replace=T,prob=c(0.7,0.3))
# c.train <- combin[1:nrow(train),]
c.train1 <- FINAL_F[inx==1,]

# c.test <- combin[-(1:nrow(train)),]
c.test1 <- FINAL_F[inx==2,]


######9.definición variable objetivo (malos)#####
y_train<-as.matrix(c.train1[,Y_Total_Target])
y_test<-as.matrix(c.test1[,Y_Total_Target])
y_all<-as.matrix(FINAL_F[,Y_Total_Target])


c.train1 <- c.train1[,"Y_Total_Target":=NULL]
c.test1 <- c.test1[,"Y_Total_Target":=NULL]
c.all <- FINAL_F[,"Y_Total_Target":=NULL]


train.matrix = as.matrix(c.train1)
mode(train.matrix) = "numeric"
test.matrix = as.matrix(c.test1)
mode(test.matrix) = "numeric"
all.matrix = as.matrix(c.all)
mode(all.matrix) = "numeric"
#__________________________________________________________________________________________

#__________________________________________________________________________________________
### Grid search for hyperparameters
#__________________________________________________________________________________________
# note to start nrounds from 200, as smaller learning rates result in errors so
# big with lower starting points that they'll mess the scales
tune_grid <- expand.grid(
  nrounds = seq(from = 10, to = 50, by = 10),
  eta = c(0.025, 0.05, 0.1, 0.3),
  max_depth = c(2, 3, 4, 5),
  gamma = seq(0, 0.3, 0.1),
  colsample_bytree = seq(0.8, 1, 0.1), # min 0 max 1
  min_child_weight = seq(1, 3, 1),
  subsample = seq(0.8, 1, 0.1)
)

tune_control <- caret::trainControl(
  method = "cv", # cross-validation
  number = 7, # with n folds 
  #index = createFolds(tr_treated$Id_clean), # fix the folds
  verboseIter = FALSE, # no training log
  allowParallel = TRUE, # FALSE for reproducible results
  classProbs = TRUE,
  summaryFunction = twoClassSummary
  )

train.y <- factor(y_train, labels = c("BUENO", "MALO"))

xgb_tune <- caret::train(
  x = train.matrix,
  y = train.y,
  trControl = tune_control,
  tuneGrid = tune_grid,
  method = "xgbTree",
  verbose = TRUE,
  metric = "ROC",
  #nthreat = 4
)

xgb_tune$bestTune
xgb_tune$finalModel

#nrounds max_depth   eta gamma colsample_bytree min_child_weight subsample
#   62      20         2 0.025     0              0.9                2       0.8

nrounds <- 20; max_depth <- 2; eta <- 0.025; gamma <- 0; colsample_bytree <- 0.9; min_child_weight <- 2; subsample <- 0.8 

#__________________________________________________________________________________________


#__________________________________________________________________________________________
# Aplicación del modelo XGBoost
#__________________________________________________________________________________________
dtrain <- xgb.DMatrix(train.matrix, label = y_train)
dtest <- xgb.DMatrix(test.matrix, label = y_test)
watchlist <- list(eval = dtest, train = dtrain)


set.seed(736)
param <- list("objective" = "binary:logistic",    # multiclass classification
              "eval_metric" = "auc",    # evaluation metric
              "nthread" = 4,   # number of threads to be used
              "max_depth" = max_depth,    # maximum depth of tree
              "eta" = eta,    # step size shrinkage
              "gamma" = gamma,    # minimum loss reduction
              "subsample" = subsample,    # part of data instances to grow tree
              "colsample_bytree" = colsample_bytree,  # subsample ratio of columns when constructing each tree
              "min_child_weight" = min_child_weight,  # minimum sum of instance weight needed in a child
              "scale_pos_weight" = sum(BASE_MODELO$Y_Total_Target==0)/sum(BASE_MODELO$Y_Total_Target==1)
)

bst <- xgb.train(param, dtrain, nrounds = nrounds, watchlist) # nrounds: max number of boosting iterations

setwd("~/Documents/Trabajo/CredibanCo/Adquirencia/Mod_Adquirencia-master")
saveRDS(object = bst, file = "model_javi.RDS")
saveRDS(object = data.frame(train.matrix, y = y_train), file = "train_javi.RDS")
saveRDS(object = data.frame(test.matrix, y = y_test), file = "test_javi.RDS")


matplot(bst$evaluation_log[,2:3],type = "l")
print(max(bst$evaluation_log$eval_auc))
print((max(bst$evaluation_log$train_auc)))

xgb.importance(model = bst)
variables <- xgb.importance(model = bst)
write.csv2(x = variables, file = "~/Downloads/importance_variables.csv")
#bst <- xgb.train(param, dtrain, nrounds = 14, watchlist)
#__________________________________________________________________________________________


#__________________________________________________________________________________________
#########RESULTADO##########
#__________________________________________________________________________________________
library(ROCR)
pred <- predict(bst, train.matrix)  
pred.1 <- ROCR::prediction(pred,y_train)
perf.1 <- performance(pred.1,"tpr", "fpr")
plot(perf.1, colorize=TRUE)
plot(perf.1, colorize=TRUE)
abline(a=0, b= 1)
m1.auc <- performance(pred.1, "auc")
m1.auc@y.values[[1]]

2*(m1.auc@y.values[[1]]-0.5)

plot.roc(y_train, pred,
         
         main="Confidence interval of a threshold", percent=TRUE,
         
         ci=TRUE, of="thresholds", # compute AUC (of threshold)
         print.auc=TRUE,
         thresholds="best", # select the (best) threshold
         
         print.thres="best") # also highlight this threshold on the plot
#########RESULTADO##########
library(ROCR)
pred <- predict(bst, test.matrix)  
hist(pred)
pred.1 <- ROCR::prediction(pred,y_test)
perf.1 <- performance(pred.1,"tpr", "fpr")
plot(perf.1, colorize=TRUE)
abline(a=0, b= 1)
m1.auc <- performance(pred.1, "auc")
m1.auc@y.values[[1]]

2*(m1.auc@y.values[[1]]-0.5)


plot.roc(y_test, pred,
         
         main="Confidence interval of a threshold", percent=TRUE,
         
         ci=TRUE, of="thresholds", # compute AUC (of threshold)
         
         thresholds="best", # select the (best) threshold
         print.auc=TRUE,
         
         print.thres="best") # also highlight this threshold on the plot    


hist(pred)


table(y_test,pred<0.5)

#################
library(ROCR)
pred <- predict(bst, all.matrix)  
pred.1 <- ROCR::prediction(pred,y_all)
perf.1 <- performance(pred.1,"tpr", "fpr")
plot(perf.1, colorize=TRUE)
m1.auc <- performance(pred.1, "auc")
m1.auc@y.values[[1]]
2*(m1.auc@y.values[[1]]-0.5)

hist(pred)

table(y_all, pred < 0.49)

pred_2 <- ifelse(test = pred < 0.47, 0, 1)

table(y_all, pred_2)

write.csv(data.frame(TARGET=y_all, PRED_DUMMY = pred_2, PRED_DEC = pred),file="all_javi.csv")

  library(pROC)

data(aSAH)



plot.roc(y_test, pred,
         
         main="Confidence interval of a threshold", percent=TRUE,
         
         ci=TRUE, of="thresholds", # compute AUC (of threshold)
         
         thresholds="best", # select the (best) threshold
         
         print.thres="best") # also highlight this threshold on the plot




library(ggplot2)
DD<-data.frame(test.matrix,y_test,pred=predict(bst, test.matrix) )

ggplot(DD, aes(pred, fill = as.factor(y_test))) +
  geom_density(position = "stack")


max(attr(perf.1,'y.values')[[1]]-attr(perf.1,'x.values')[[1]])



#____________________________________________________________________________________
# unification of models
#____________________________________________________________________________________
FINAL_TODOS <- readRDS(file = "~/Downloads/Fina_Todos.RDS")
FINAL_TODOS <- FINAL_TODOS_ORIG[,.(Percentil_pol, Sd_qn, Ticket_pol, var2_7, Cons_x_trj, Facturacion_Total_Pol, Dens.CU, Var_Tarjetas_Pol, 
                                   Avg_Tarj_Pol, Ticket_pol_cat, Y_Total_Target)]

#____________________________________________________________________________________
set.seed(321)
inx<-sample(seq(1,2),size=nrow(FINAL_TODOS),replace=T,prob=c(0.6,0.4))
# c.train <- combin[1:nrow(train),]
c.train1 <- FINAL_TODOS[inx==1,]

# c.test <- combin[-(1:nrow(train)),]
c.test1 <- FINAL_TODOS[inx==2,]


######9.definición variable objetivo (malos)#####
y_train<-as.matrix(c.train1[,Y_Total_Target])
y_test<-as.matrix(c.test1[,Y_Total_Target])
y_all<-as.matrix(FINAL_TODOS[,Y_Total_Target])


c.train1 <- c.train1[,"Y_Total_Target":=NULL]
c.test1 <- c.test1[,"Y_Total_Target":=NULL]
c.all <- FINAL_TODOS[,"Y_Total_Target":=NULL]


train.matrix = as.matrix(c.train1)
mode(train.matrix) = "numeric"
test.matrix = as.matrix(c.test1)
mode(test.matrix) = "numeric"
all.matrix = as.matrix(c.all)
mode(all.matrix) = "numeric"
#____________________________________________________________________________________



#____________________________________________________________________________________
dtrain <- xgb.DMatrix(train.matrix, label = y_train)
dtest <- xgb.DMatrix(test.matrix, label = y_test)
watchlist <- list(eval = dtest, train = dtrain)


set.seed(123)
param <- list("objective" = "binary:logistic",    # multiclass classification
              "eval_metric" = "auc",    # evaluation metric
              "nthread" = 4,   # number of threads to be used
              "max_depth" =5 ,    # maximum depth of tree
              "eta" = 0.01,    # step size shrinkage
              "gamma" = 0,    # minimum loss reduction
              "subsample" =0.81,    # part of data instances to grow tree
              "colsample_bytree" = 0.9,  # subsample ratio of columns when constructing each tree
              "min_child_weight" = 5,  # minimum sum of instance weight needed in a child
              "scale_pos_weight" = sum(FINAL_TODOS_ORIG$Y_Total_Target==0)/sum(FINAL_TODOS_ORIG$Y_Total_Target==1)
)

bst <- xgb.train(param, dtrain, nrounds = 1000, watchlist) # nrounds: max number of boosting iterations

matplot(bst$evaluation_log[,2:3],type = "l")
print(max(bst$evaluation_log$eval_auc))

xgb.importance(model = bst)
variables <- xgb.importance(model = bst)
write.csv2(x = variables, file = "~/Downloads/importance_variables.csv")
#bst <- xgb.train(param, dtrain, nrounds = 14, watchlist)
#____________________________________________________________________________________


modelLookup("xgbLinear")

# dput function allows to get structure from another function 
dput(setdiff(x = colnames(FINAL), y = colnames(M_X)))



#____________________________________________________________________________________
# Open Street Maps 
#____________________________________________________________________________________

library(sf)
setwd("~/Documents/Trabajo/CredibanCo/Adquirencia/Datos/mygeodata/map")
setwd("~/Downloads/mygeodata/map")
aoi_boundary <- st_read("buildings-polygon.shp")
st_geometry_type(aoi_boundary)
st_crs(aoi_boundary)

st_bbox(aoi_boundary)

aoi_boundary$building

ggplot() + 
  geom_sf(data = aoi_boundary, size = 3, color = "black", fill = "cyan1") + 
  ggtitle("AOI Boundary Plot") + 
  coord_sf()

aoi_boundary


library(gamlss.demo)
demoDist()
gamlss.dist::de




sessionInfo()

#__________________________________________________________________________________________
#OpenStreetMap
#__________________________________________________________________________________________

library(osmar)

ubicaciones_hex <- readRDS("/Users/jjimenez/Downloads/ubicaciones_hexagonos.RDS")
src <- osmsource_api(url = "https://api.openstreetmap.org/api/0.6/")

#get_osm(node(18961430), source = src)
#get_osm(way(3810479), source = src)
#get_osm(way(3810479), source = src, full = TRUE)

cami <- list()

bb <- center_bbox(-74.057028, 4.654532, 250, 250) 
#bb <- corner_bbox(left, bottom, right, top)
ua <- get_osm(bb, source = src)

ua

# bbox:           xmin: 11767280 ymin: 18335410 xmax: 12000920 ymax: 18678370
# CRS:            +proj=utm +ellps=GRS80 +datum=WGS84


summary(ua$nodes)

objects <- ua[[2]][2]$tags
summary(objects)

cami[[1]] <- objects
names(cami) <- "1"

osm_raw_info <- list()

for (i in 1:nrow(ubicaciones_hex)){
  bb <- center_bbox(ubicaciones_hex[i,2], ubicaciones_hex[i,3], 250, 250) 
  ua <- get_osm(bb, source = src)
  objects <- ua[[1]][2]$tags
  osm_raw_info[[i]] <- objects
}

names(osm_raw_info) <- as.character(seq(1, nrow(ubicaciones_hex), 1))

View(objects)

ts_ids <- find(ua, node(tags(v == "bicycle_parking")))

ts_ids

bs_ids <- find(ua, node(tags(v %agrep% "busstop")))

bs_ids

hw_ids <- find(ua, way(tags(k == "highway")))

hw_ids <- find_down(ua, way(hw_ids))

str(hw_ids)

ts <- subset(ua, node_ids = ts_ids)

ts

bs <- subset(ua, node_ids = bs_ids)

bs

hw <- subset(ua, ids = hw_ids)

hw

plot(ua)
plot_ways(hw, add = TRUE, col = "green")
plot_nodes(ua, add = TRUE, col = "red")
plot_nodes(bs, add = TRUE, col = "blue")


args(as_sp)

bg_ids <- find(ua, way(tags(k == "building")))
bg_ids <- find_down(ua, way(bg_ids))
bg <- subset(ua, ids = bg_ids)
bg

bg_poly <- as_sp(bg, "polygons")

spplot(bg_poly, c("version"))

hw_line <- as_sp(hw, "lines")
bs_points <- as_sp(ua, "points")


bus_ids <- find(ua, relation(tags(v == "bus")))
bus <- lapply(bus_ids,
  function(i) {
    raw <- get_osm(relation(i),source = src ,full = TRUE)
    as_sp(raw, "lines")
  })
    
    
plot(bg_poly, col = "gray")    
plot(hw_line, add = TRUE, col = "green")    
plot(bs_points, add = TRUE, col = "blue") 
for ( i in seq(along = bus) ) {
  plot(bus[[i]], add = TRUE, col = "blue") }



vpol_upz <- readRDS(file = "pol_upz.RDS")

















#_______
#Prueba modelo de todos
#_________

#__________________________________________________________________________________________
# set seed and partition in train and test subsets
#__________________________________________________________________________________________
set.seed(98)
inx<-sample(seq(1,2),size=nrow(final_todos_v2),replace=T,prob=c(0.7,0.3))
# c.train <- combin[1:nrow(train),]
c.train1 <- final_todos_v2[inx==1,]

# c.test <- combin[-(1:nrow(train)),]
c.test1 <- final_todos_v2[inx==2,]


######9.definición variable objetivo (malos)#####
y_train<-as.matrix(c.train1[,Y_Total_Target])
y_test<-as.matrix(c.test1[,Y_Total_Target])
y_all<-as.matrix(final_todos_v2[,Y_Total_Target])


c.train1 <- c.train1[,"Y_Total_Target":=NULL]
c.test1 <- c.test1[,"Y_Total_Target":=NULL]
c.all <- final_todos_v2[,"Y_Total_Target":=NULL]


train.matrix = as.matrix(c.train1)
mode(train.matrix) = "numeric"
test.matrix = as.matrix(c.test1)
mode(test.matrix) = "numeric"
all.matrix = as.matrix(c.all)
mode(all.matrix) = "numeric"
#__________________________________________________________________________________________

#__________________________________________________________________________________________
### Grid search for hyperparameters
#__________________________________________________________________________________________
# note to start nrounds from 200, as smaller learning rates result in errors so
# big with lower starting points that they'll mess the scales
tune_grid <- expand.grid(
  nrounds = seq(from = 10, to = 50, by = 10),
  eta = c(0.025, 0.05, 0.1, 0.3),
  max_depth = c(2, 3, 4, 5),
  gamma = seq(0, 0.3, 0.1),
  colsample_bytree = seq(0.8, 1, 0.1), # min 0 max 1
  min_child_weight = seq(1, 3, 1),
  subsample = seq(0.8, 1, 0.1)
)

tune_control <- caret::trainControl(
  method = "cv", # cross-validation
  number = 7, # with n folds 
  #index = createFolds(tr_treated$Id_clean), # fix the folds
  verboseIter = FALSE, # no training log
  allowParallel = TRUE, # FALSE for reproducible results
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

train.y <- factor(y_train, labels = c("BUENO", "MALO"))

xgb_tune <- caret::train(
  x = train.matrix,
  y = train.y,
  trControl = tune_control,
  tuneGrid = tune_grid,
  method = "xgbTree",
  verbose = TRUE,
  metric = "ROC",
  #nthreat = 4
)

xgb_tune$bestTune
xgb_tune$finalModel

#nrounds max_depth   eta gamma colsample_bytree min_child_weight subsample
#   62      20         2 0.025     0              0.9                2       0.8

nrounds <- 20; max_depth <- 2; eta <- 0.025; gamma <- 0.3; colsample_bytree <- 1; min_child_weight <- 3; subsample <- 0.9 

#__________________________________________________________________________________________


#__________________________________________________________________________________________
# Aplicación del modelo XGBoost
#__________________________________________________________________________________________
dtrain <- xgb.DMatrix(train.matrix, label = y_train)
dtest <- xgb.DMatrix(test.matrix, label = y_test)
watchlist <- list(eval = dtest, train = dtrain)


set.seed(736)
param <- list("objective" = "binary:logistic",    # multiclass classification
              "eval_metric" = "auc",    # evaluation metric
              "nthread" = 4,   # number of threads to be used
              "max_depth" = max_depth,    # maximum depth of tree
              "eta" = eta,    # step size shrinkage
              "gamma" = gamma,    # minimum loss reduction
              "subsample" = subsample,    # part of data instances to grow tree
              "colsample_bytree" = colsample_bytree,  # subsample ratio of columns when constructing each tree
              "min_child_weight" = min_child_weight,  # minimum sum of instance weight needed in a child
              "scale_pos_weight" = sum(y_all=="0")/sum(y_all=="1")
)

bst <- xgb.train(param, dtrain, nrounds = nrounds, watchlist) # nrounds: max number of boosting iterations

setwd("~/Documents/Trabajo/CredibanCo/Adquirencia/Mod_Adquirencia-master")
saveRDS(object = bst, file = "model_javi.RDS")
saveRDS(object = data.frame(train.matrix, y = y_train), file = "train_javi.RDS")
saveRDS(object = data.frame(test.matrix, y = y_test), file = "test_javi.RDS")


matplot(bst$evaluation_log[,2:3],type = "l")
print(max(bst$evaluation_log$eval_auc))
print((max(bst$evaluation_log$train_auc)))

xgb.importance(model = bst)
variables <- xgb.importance(model = bst)
write.csv2(x = variables, file = "~/Downloads/importance_variables.csv")
#bst <- xgb.train(param, dtrain, nrounds = 14, watchlist)
#__________________________________________________________________________________________


    



  
    
    
    