
#==============================================================================
# Librerias y argumentos
# =============================================================================
# Librerias
library(xgboost)
library(pROC)

# Argumentos 
modelo = "desc"
size_1 = 0.38
size_na = 60
porc = 0.6
tipos = c("mean", "median", "cero")
tipo = tipos[1]
eliminacion = TRUE
balanceado = FALSE
balanceado_no_50 = FALSE

#==============================================================================
# Rutas, source y lectura de ficheros
# =============================================================================
ruta_proj = "/Advanced_Analytics/Data_Science_Des/ILOCALIZABLES/00_EVIDENCIAS"
ruta_data = file.path(ruta_proj, "01_CREACION_TABLONES", "Resultados", "Procesadas", "RDS")
ruta_output = file.path(ruta_proj, "05_MEDICIONES", "Resultados")

source("/Advanced_Analytics/Data_Science_Des/ILOCALIZABLES/00_EVIDENCIAS/01_CREACION_TABLONES/Codigos/j_giniksauc_v2.R")
source("/Advanced_Analytics/Data_Science_Des/ILOCALIZABLES/00_EVIDENCIAS/01_CREACION_TABLONES/Codigos/g_results_grupos_v3.R")

# Lectura de ficheros 
files = list.files(ruta_data)
file = files[grep(pattern = modelo, files)]
tablon = readRDS(file.path(ruta_data, file))

#==============================================================================
# Preparacion tablon y eliminacion de variables con muchos NA?s
# =============================================================================
# Eliminacion de variables de fechas
tablon$fecha_salida_irreg = NULL
tablon$fecha_dato = NULL
tablon$f_mes = NULL
tablon$mes_ilocalizado = NULL

# Creacion variables nuevas
tablon$ind_NAs = apply(tablon, 1, function(x) sum(is.na(x)))
tablon$ind_pasivo_balance = ifelse(tablon$pasivo_balance > 0, 1, 0)
tablon$ind_fuera_balance = ifelse(tablon$fuera_balance > 0, 1, 0)

# Eliminacion de variables con mucho % de NA?s
if (eliminacion == TRUE) {
  
  for (i in 1:ncol(tablon)) {
    sum_na = sum(is.na(tablon[,i]))
    porc_na = sum_na/nrow(tablon)*100
    # print(porc_na)
    
    if (porc_na > size_na) {
      print(paste0(colnames(tablon)[i], " eliminada"))
      tablon[, colnames(tablon)[i]] = NULL
    }
    
  }
  
}

# # Imputacion de NA?s de ingresos y gastos a la mediana
# tablon$ingresos = ifelse(is.na(tablon$ingresos), median(tablon$ingresos, na.rm = TRUE), tablon$ingresos)
# tablon$gastos = ifelse(is.na(tablon$gastos), median(tablon$gastos, na.rm = TRUE), tablon$gastos)

#==============================================================================
# Variables que tienen NA
# =============================================================================
variables_na <- colnames(tablon)[colSums(is.na(tablon)) > 0]

# Imputacion del resto de NA?s a 0
sum(is.na(tablon))

# Sustitucion de NA?s segun se indique al comienzo del codigo (media, moda, mediana o por ceros)
for (i in 1:ncol(tablon)) {
  if (tipo != "cero") {
    # print(tipo)
    modo = eval(parse(text = as.character(tipo)))
    if (class(tablon[,i]) != "character") {
      tablon[,i] = ifelse(is.na(tablon[,i]), modo(tablon[,i], na.rm = TRUE), tablon[,i])
    } else {
      moda = function(x){
        ux = unique(x)
        ux[which.max(tabulate(match(x, ux)))]
      }
      tablon[,i] = ifelse(is.na(tablon[,i]), moda(tablon[,i]), tablon[,i])
    }
  } else {
    print("Quitar NA?s por 0")
    tablon[is.na(tablon)] = 0
  }
}

sum(is.na(tablon))

#==============================================================================
# Particion Train Test
#==============================================================================
# Porcentaje para particion train/test
set.seed(22)
sample_train = sample(1:nrow(tablon), size = nrow(tablon)*porc, replace = FALSE)
train = tablon[sample_train, ]
test = tablon[-sample_train,]
sum(train$id %in% test$id) #comprobacion

# Modificamos el numero de unos que hay en el train, en este caso dejamos 100 unos y 253 ceros
# Balanceamos el train 50/50
if (balanceado == TRUE) {
  
  train_1 = train[train$target_90 == 1,]
  train_0 = train[train$target_90 == 0,]
  
  if (nrow(train_1) > nrow(train_0)) {
    print("Mas 1?s que 0?s")
    filas = sample(1:nrow(train_1), size = nrow(train_0), replace = FALSE)
    train_1 = train_1[filas,]
    train = rbind(train_1, train_0)
  } else {
    print("Mas 0?s que 1?s")
    filas = sample(1:nrow(train_0), size = nrow(train_1), replace = FALSE)
    train_0 = train_0[filas,]
    train = rbind(train_1, train_0)
  }
  
}

table(train$target_90)

# Balanceado no 50/50
table(train$target_90) # comprobacion

if (balanceado_no_50 == TRUE) {
  
  # Particion del train en target = 1 y target = 0
  train_1 = train[train$target_90 == 1,]
  train_0 = train[train$target_90 == 0,]
  
  # Seleccion aleatoria del numero de 1?s
  filas_1 = sample(1:nrow(train_1), size = nrow(train_0)*size_1, replace = FALSE)
  train_1 = train_1[filas_1, ]
  
  train = rbind(train_1, train_0)
  
}

table(train$target_90)

# Variables a quitar para Xgboost
id <- "id"
target <- "target_90"
vars_no <- c(id, target)

#==============================================================================
# TRAIN INITIAL MODEL WITH XGBOOST FOR XGBOOST SELECTION FEATURES
#==============================================================================

# - Remove id/target - #
train.x <- train[, !(names(train) %in% vars_no)]

# - Train model with XGboost - #
# seed <- sample(1:100, 1)
set.seed(22)
xgboost.model.ilocalizables <- xgboost(data = as.matrix(train.x),
                                       label = train$target_90,
                                       nround = 20,
                                       objective = "binary:logistic",
                                       print_every_n = 1,
                                       mdepth = 6, # 15,
                                       eta = 0.3, # 0.1,
                                       columnSample = 0.9, # 0.5,
                                       subsample = 0.9, # 0.5,
                                       maxStep = as.numeric(2.87258065561764),
                                       baseScore = 0.5,
                                       nthread = 5,
                                       missing = NaN)

# - Predict model - #
test$prediction_xg <- predict(xgboost.model.ilocalizables, as.matrix(test[,names(train.x)]), missing = NaN)
importance_ilocalizable <- xgb.importance(feature_names = names(train.x), model = xgboost.model.ilocalizables)

# - Estimate measures - #
test.roc <- test[ , c("id", "prediction_xg", "target_90")]
names(test.roc) <- c("Id", "Score", "TARGET")

# - Estimate Gini - #
ginis <- giniksauc(datamatrix = test.roc, invert = FALSE)$measures
ginis

# - Estimate results - #
results(datamatrix = test.roc,
        filepath = ruta_output,
        filename = "prueba_9.csv",
        data_path = NULL,
        ngroups = 10,
        invert = FALSE,
        decimal_out = ",",
        sep = ";")
