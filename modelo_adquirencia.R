#________________________________________________________________________________________________________
# WORK DIRECTORY 
#________________________________________________________________________________________________________

setwd("~/Documents/Trabajo/CredibanCo/Adquirencia/Datos")

#________________________________________________________________________________________________________
# PACKAGES
#________________________________________________________________________________________________________

pacman::p_load("dplyr",
               "data.table",
               "tidyverse",
               "fastDummies",
               "randomForest",
               "MLmetrics",
               "corrplot")

#________________________________________________________________________________________________________
# DATA
#________________________________________________________________________________________________________

X <- readRDS(file = "X_modelo.RDS")
y <- readRDS(file = "y_modelo.RDS")

#________________________________________________________________________________________________________
# UPPERCASE COLUMN NAMES
#________________________________________________________________________________________________________

X_colnames <- colnames(X)
X_colnames <- toupper(X_colnames)
colnames(X) <- X_colnames

y_colnames <- colnames(y)
y_colnames <- toupper(y_colnames)
colnames(y) <- y_colnames

rm(X_colnames,y_colnames)

#________________________________________________________________________________________________________
# FEATURE ENGINEERING - BASE - POLYGONS IN HEXAGONS  
#________________________________________________________________________________________________________

X_grouped <- distinct(X[,c("POL_BARRIO","SEMANA","CATEGORIA_SUPER_APP")])

#________________________________________________________________________________________________________
# EXPLANATION OF THE WEEKS   
#________________________________________________________________________________________________________

# WEEK 1 - 13 MARZO 2020           AL       26 MARZO 2020       PRE-COVID
# WEEK 2 - 20 MARZO 2020           AL       2 DE ABRIL 2020     POST-COVID 
# WEEK 3 - 27 DE MARZO 2020        AL       9 DE ABRIL 2020     POST-COVID
# WEEK 4 - 3 DE ABRIL 2020         AL       16 DE ABRIL 2020    POST-COVID
# WEEK 5 - 10 DE ABRIL 2020        AL       23 DE ABRIL 2020    POST-COVID
# WEEK 6 - 17 DE ABRIL 2020        AL       30 DE ABRIL 2020    POST-COVID
# WEEK 7 - 24 DE ABRIL 2020        AL       7 DE MAYO 2020      POST-COVID
# WEEK 8 - 1 DE MAYO 2020          AL       14 DE MAYO 2020     POST-COVID
# WEEK 9 - 8 DE MAYO 2020          AL       21 DE MAYO 2020     POST-COVID
# WEEK 10 - 15 DE MAYO 2020        AL       28 DE MAYO 2020     POST-COVID
# WEEK 11 - 22 DE MAYO 2020        AL       4 DE JUNIO 2020     POST-COVID
# WEEK 12 - 29 DE MAYO 2020        AL       11 DE JUNIO 2020    POST-COVID
# WEEK 13 - 5 DE JUNIO 2020        AL       18 DE JUNIO 2020    POST-COVID
# WEEK 14 - 12 DE JUNIO 2020       AL       25 DE JUNIO 2020    POST-COVID
# WEEK 15 - 19 DE JUNIO 2020       AL       2 DE JULIO 2020     POST-COVID

#________________________________________________________________________________________________________
# ONE HOT ENCODING  
#________________________________________________________________________________________________________

X <- fastDummies::dummy_cols(.data = X, select_columns = c("TIPO_COMERCIO","RED"))
CANT_CATEG <- X[,c("POL_BARRIO","SEMANA","CATEGORIA_SUPER_APP")] 
CANT_CATEG <- fastDummies::dummy_cols(.data = CANT_CATEG, select_columns = c("CATEGORIA_SUPER_APP"))

#________________________________________________________________________________________________________
# VARIABLE ONE 
# CANT_COMERCIOS: number of stores per week in polygons 
#________________________________________________________________________________________________________

CANT_CATEG <- data.table(CANT_CATEG)
CANT_CATEG <- CANT_CATEG[,.(CANT_CINE_TEATRO = sum(`CATEGORIA_SUPER_APP_CINE Y TEATRO`),
                            CANT_DISCOTECA_BARES = sum(`CATEGORIA_SUPER_APP_DISCOTECA Y BARES`),
                            CANT_DROGUERIAS = sum(CATEGORIA_SUPER_APP_DROGUERIAS),
                            CANT_EDS = sum(CATEGORIA_SUPER_APP_EDS),
                            CANT_EDUCACION = sum(CATEGORIA_SUPER_APP_EDUCACION),
                            CANT_ENTRETENIMIENTO = sum(CATEGORIA_SUPER_APP_ENTRETENIMIENTO),
                            CANT_EVENTOS = sum(CATEGORIA_SUPER_APP_EVENTOS),
                            CANT_GIMNASIOS = sum(CATEGORIA_SUPER_APP_GIMNASIOS),
                            CANT_HOGAR = sum(CATEGORIA_SUPER_APP_HOGAR),
                            CANT_MASCOTAS = sum(CATEGORIA_SUPER_APP_MASCOTAS),
                            CANT_MI_ARMARIO = sum(`CATEGORIA_SUPER_APP_MI ARMARIO`),
                            CANT_OTROS = sum(CATEGORIA_SUPER_APP_OTROS),
                            CANT_PARQUEADEROS = sum(CATEGORIA_SUPER_APP_PARQUEADEROS),
                            CANT_PELUQUERIAS = sum(CATEGORIA_SUPER_APP_PELUQUERIAS),
                            CANT_RESTAURANTES = sum(CATEGORIA_SUPER_APP_RESTAURANTES),
                            CANT_SECTOR_FINANCIERO = sum(`CATEGORIA_SUPER_APP_SECTOR FINANCIERO Y ASEGURADOR`),
                            CANT_SERVICIOS = sum(CATEGORIA_SUPER_APP_SERVICIOS),
                            CANT_SUPERMERCADOR = sum(CATEGORIA_SUPER_APP_SUPERMERCADOS),
                            CANT_TECNOLOGIA = sum(CATEGORIA_SUPER_APP_TECNOLOGIA),
                            CANT_TIENDAS = sum(`CATEGORIA_SUPER_APP_TIENDAS Y ALMACENES`),
                            CANT_TRANSPORTE = sum(CATEGORIA_SUPER_APP_TRANSPORTE),
                            CANT_VIAJES_TURISMO = sum(`CATEGORIA_SUPER_APP_VIAJES Y TURISMO`)), by = .(POL_BARRIO, SEMANA)]

CANT_CATEG_ONE <- cbind(CANT_CATEG[,1:2], CANT_COMERCIOS = rowSums(CANT_CATEG[,3:ncol(CANT_CATEG)]))
X_grouped <- dplyr::left_join(x = X_grouped, y = CANT_CATEG_ONE, by = c("POL_BARRIO" = "POL_BARRIO", "SEMANA" = "SEMANA"))

#________________________________________________________________________________________________________
# VARIABLE TWO 
# CANT_COMERCIOS_POR_SUPER_APP: number of stores per week in polygons by super app category 
#________________________________________________________________________________________________________

#CANT_CATEG_TWO <- CANT_CATEG[,.(CANT_COMERCIOS_POR_SUPER_APP = .N), by = .(POL_BARRIO, SEMANA)]
#X_grouped <- dplyr::left_join(x = X_grouped, y = CANT_CATEG_TWO, by = c("POL_BARRIO" = "POL_BARRIO", "SEMANA" = "SEMANA"))

#rm(CANT_CATEG, CANT_CATEG_ONE, CANT_CATEG_TWO)

#________________________________________________________________________________________________________
# VARIABLE THREE, FOUR, FIVE, SIX AND SEVEN
# CANT_ANTIGUOS: number of old stores 
# CANT_NUEVOS: number of new stores
# CANT_CREDIBANCO: number of stores with CredibanCo network  
# CANT_REDEBAN: number of stores with Redeban network
# CANT_DUAL: number of stores with both networks
#________________________________________________________________________________________________________

CANT_TIPO_RED <- X[,.(CANT_ANTIG = sum(TIPO_COMERCIO_Antiguo), 
                      CANT_NUEVOS = sum(TIPO_COMERCIO_Nuevo), 
                      CANT_CREDIBANCO = sum(RED_Credibanco), 
                      CANT_REDEBAN = sum(RED_Redeban), 
                      CANT_DUAL = sum(RED_Dual)), by = .(POL_BARRIO, SEMANA) ]
X_grouped <- dplyr::left_join(x = X_grouped, y = CANT_TIPO_RED, by = c("POL_BARRIO" = "POL_BARRIO", "SEMANA" = "SEMANA"))

#________________________________________________________________________________________________________
# VARIABLE EIGHT, NINE, TEN AND ELEVEN 
# PROM_FACTUR: average total billing 
# TOTAL_TRX: total transactions 
# TOTAL_TARJETAS: total cardholders 
# PROM_TICKET: average billing 
#________________________________________________________________________________________________________

FACT_TRX_TARJETAS <- X[,.(PROM_FACTUR = mean(FACTURACION),
                          TOTAL_TRX = sum(TRX),
                          TOTAL_TARJETAS = sum(TARJETAS),
                          PROM_TICKET = mean(TICKET_PROM)), by = .(POL_BARRIO, SEMANA)]
X_grouped <- dplyr::left_join(x = X_grouped, y = FACT_TRX_TARJETAS, by = c("POL_BARRIO" = "POL_BARRIO", "SEMANA" = "SEMANA"))

#________________________________________________________________________________________________________
# VARIABLE TWELVE 
# VAR_TARJETAS: TOTAL_TARJETAS / TOTAL_TRX
#________________________________________________________________________________________________________

VAR_TARJETAS <- FACT_TRX_TARJETAS[,.(VAR_TARJETAS = TOTAL_TARJETAS / TOTAL_TRX), by = .(POL_BARRIO, SEMANA)]
X_grouped <- dplyr::left_join(x = X_grouped, y = VAR_TARJETAS, by = c("POL_BARRIO" = "POL_BARRIO", "SEMANA" = "SEMANA"))


#________________________________________________________________________________________________________
# OUTPUT VARIABLE
#________________________________________________________________________________________________________

base_model <- dplyr::inner_join(x = X_grouped, y = y[,c("POL_BARRIO","SEMANA","Y_FINAL")], by = c("POL_BARRIO" = "POL_BARRIO", "SEMANA" = "SEMANA"))
base_model <- na.omit(base_model)
base_model <- base_model[base_model$SEMANA != 15,]

#________________________________________________________________________________________________________
# SPLIT DATA 
#________________________________________________________________________________________________________

#table(base_model$Y_FINAL)/(765 + 415)

#0         1 
#0.6483051 0.3516949 

# training Sample with 300 observations
train = sample(1:nrow(base_model),nrow(base_model) * 0.7)
str(base_model)  

#________________________________________________________________________________________________________
# MODEL - RANDOM FOREST - TRAIN
#________________________________________________________________________________________________________
base_model$Y_FINAL <- as.factor(base_model$Y_FINAL)

base_model.rf = randomForest(Y_FINAL ~ . , data = base_model , subset = train)
base_model.rf

#Call:
#  randomForest(formula = Y_FINAL ~ ., data = base_model, subset = train) 
#Type of random forest: classification
#Number of trees: 500
#No. of variables tried at each split: 3

#OOB estimate of  error rate: 38.14%
#Confusion matrix:
#    0  1 class.error
#0 462 72   0.1348315
#1 243 49   0.8321918

plot(base_model.rf)

table(base_model.rf$predicted)
y_true <- base_model[train]$Y_FINAL
y_pred <- base_model.rf$predicted
MLmetrics::F1_Score(y_true = y_true, y_pred = y_pred)
#  0.7457627
MLmetrics::Accuracy(y_true = y_true, y_pred = y_pred)
# 0.6186441
MLmetrics::Sensitivity(y_true = y_true, y_pred = y_pred)
# 0.8651685
MLmetrics::Specificity(y_true = y_true, y_pred = y_pred)
# 0.1678082 
MLmetrics::Recall(y_true = y_true, y_pred = y_pred)
# 0.8651685
MLmetrics::AUC(y_true = y_true, y_pred = y_pred)

#________________________________________________________________________________________________________
# MODEL - RANDOM FOREST - TEST
#________________________________________________________________________________________________________

pred<-predict(base_model.rf,base_model[-train,])
true<-base_model[-train]$Y_FINAL
MLmetrics::F1_Score(y_true = true, y_pred = pred)
# 0.7333333
MLmetrics::Accuracy(y_true = true, y_pred = pred)
# 0.5932203
MLmetrics::Sensitivity(y_true = true, y_pred = pred)
# 0.8571429
MLmetrics::Specificity(y_true = true, y_pred = pred)
# 0.09756098
MLmetrics::Recall(y_true = true, y_pred = pred)
# 0.8571429
MLmetrics::AUC(y_true = true, y_pred = pred)

#MLmetrics::ConfusionMatrix(pred, true)
#        y_pred
#y_true   0   1
#     0 198  33
#     1 111  12



#________________________________________________________________________________________________________
# MODEL - RANDOM FOREST - TRAIN
#________________________________________________________________________________________________________

base_model.rf = randomForest(Y_FINAL ~ . , data = base_model[,4:ncol(base_model)] , subset = train)
base_model.rf

#Call:
#  randomForest(formula = Y_FINAL ~ ., data = base_model[, 4:ncol(base_model)],      subset = train) 
#Type of random forest: classification
#Number of trees: 500
#No. of variables tried at each split: 3
#
#OOB estimate of  error rate: 41.28%
#Confusion matrix:
#     0   1 class.error
#0 426 108   0.2022472
#1 233  59   0.7979452

plot(base_model.rf)

table(base_model.rf$predicted)
y_true <- base_model[train]$Y_FINAL
y_pred <- base_model.rf$predicted
MLmetrics::F1_Score(y_true = y_true, y_pred = y_pred)
#  0.714166
MLmetrics::Accuracy(y_true = y_true, y_pred = y_pred)
# 0.5871671
MLmetrics::Sensitivity(y_true = y_true, y_pred = y_pred)
# 0.7977528
MLmetrics::Specificity(y_true = y_true, y_pred = y_pred)
# 0.2020548
MLmetrics::Recall(y_true = y_true, y_pred = y_pred)
# 0.7977528
MLmetrics::AUC(y_true = y_true, y_pred = y_pred)

#________________________________________________________________________________________________________
# MODEL - RANDOM FOREST - TEST
#________________________________________________________________________________________________________

pred<-predict(base_model.rf,base_model[-train,4:ncol(base_model)])
true<-base_model[-train]$Y_FINAL
MLmetrics::F1_Score(y_true = true, y_pred = pred)
# 0.7333333
MLmetrics::Accuracy(y_true = true, y_pred = pred)
# 0.5932203
MLmetrics::Sensitivity(y_true = true, y_pred = pred)
# 0.8571429
MLmetrics::Specificity(y_true = true, y_pred = pred)
# 0.09756098
MLmetrics::Recall(y_true = true, y_pred = pred)
# 0.8571429
MLmetrics::AUC(y_true = y_true, y_pred = y_pred)

#MLmetrics::ConfusionMatrix(pred, true)
#        y_pred
#y_true   0   1
#     0 198  33
#     1 111  12

str(base_model[train, 4:ncol(base_model)])
corr <- round(cor(base_model[train, 4:(ncol(base_model)-1)]), 2)
corrplot(corr, method = "number", type = "upper")

#________________________________________________________________________________________________________
# MODEL - RANDOM FOREST - VARIABLE IMPORTANCE
#________________________________________________________________________________________________________

base_model.rf$importance

list_features=data.frame("feature"= names(optimal_ranger$variable.importance),"value"=unname(optimal_ranger$variable.importance))

list_features %>%
  dplyr::arrange(desc(value)) %>%
  dplyr::top_n(25) %>%
  ggplot(aes(reorder(feature, value), value)) +
  geom_col() +
  coord_flip() +
  ggtitle("Top 25 important variables")


