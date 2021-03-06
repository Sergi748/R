
# Functions to create dummies for the character variables or the variables given for the users
# Create Dummies by character variables
createDummiesBasics = function(tabla) {
  
  for (col in 1:ncol(tabla)) {
    if (class(tabla[,col]) == "character") {
      for (i in levels(as.factor(tabla[,col]))) {
        booleano = sapply(tabla[,col], function(x) ifelse(x == i, 1, 0))
        tabla = cbind(tabla, booleano)
        colnames(tabla)[grep("booleano", names(tabla))] = paste0(colnames(tabla)[col], ".", i)
      }
    }
  }
  
  return(tabla)
  
}

# Create Dummies by character variables or by given variables
createDummiesVars = function(tabla, vars) {
  
  for (col in 1:ncol(tabla)) {
    if (missing(vars)) {
      if (class(tabla[,col]) == "character") {
        for (i in levels(as.factor(tabla[,col]))) {
          booleano = sapply(tabla[,col], function(x) ifelse(x == i, 1, 0))
          tabla = cbind(tabla, booleano)
          colnames(tabla)[grep("booleano", names(tabla))] = paste0(colnames(tabla)[col], ".", i)
        }
      }
    } else {
      if (colnames(tabla)[col] %in% vars) {
        for (i in levels(as.factor(tabla[,col]))) {
          booleano = sapply(tabla[,col], function(x) ifelse(x == i, 1, 0))
          tabla = cbind(tabla, booleano)
          colnames(tabla)[grep("booleano", names(tabla))] = paste0(colnames(tabla)[col], ".", i)
        }
      }
    }
  }
  
  return(tabla)  
  
}

# Create Dummies by character variables or by given variables, the user can to decide if remove variables or not
createDummies = function(tabla, remove = FALSE, vars) {
  
  varsname = c()
  
  for (col in 1:ncol(tabla)) {
    if (missing(vars)) {
      if (class(tabla[,col]) == "character") {
        varsname = append(varsname, colnames(tabla)[col])
        for (i in levels(as.factor(tabla[,col]))) {
          booleano = sapply(tabla[,col], function(x) ifelse(x == i, 1, 0))
          tabla = cbind(tabla, booleano)
          colnames(tabla)[grep("booleano", names(tabla))] = paste0(colnames(tabla)[col], ".", i)
        }
      }
    } else {
      if (colnames(tabla)[col] %in% vars) {
        for (i in levels(as.factor(tabla[,col]))) {
          booleano = sapply(tabla[,col], function(x) ifelse(x == i, 1, 0))
          tabla = cbind(tabla, booleano)
          colnames(tabla)[grep("booleano", names(tabla))] = paste0(colnames(tabla)[col], ".", i)
        }
      }
    }
  }
  
  if (remove == TRUE && !missing(vars)) {
    tabla[, vars] = NULL
  } else if (remove == TRUE && missing(vars)) {
    tabla[, varsname] = NULL
  }
  
  return(tabla)
  
}


# Function to ipnut the NA´s by the elements given by the user.
# The user should to give two tables, one of this is a dataset with the values, the other table have two columns,
# in the first column the user put the name of the variable and the other column have to put how to input the NA´s
# (mean, mode...)
InputNA = function (tabla, imputaciones) {
  
  # Tabla: dataset con los datos a analizar
  # Tablon imputaciones: dataset con dos columnas, en una el nombre de la variable
  # en la otra columna indicamos a que queremos imputar los NA?s
  
  for (i in 1:ncol(tabla)) {
    
    tipo = as.character(imputaciones[i, "Imputacion"])
    tipo = tolower(tipo)
    
    # Obtencion de la funcion a la cual vamos a imputar los NA?s, moda o media o mediana
    if (tipo != "mode") {
      modo = eval(parse(text = as.character(imputaciones[i, "Imputacion"])))
    } else {
      modo = function(x){
        ux = unique(x)
        ux[which.max(tabulate(match(x, ux)))]
      }
    }
    
    if (class(tabla[,i]) == "Date") {
      tabla[,i] = as.character(tabla[,i])
    } 
    
    if (class(tabla[,i]) == "numeric") {
      tabla[,i] = ifelse(is.na(tabla[,i]), modo(tabla[,i], na.rm = TRUE), tabla[,i])
    } else if (class(tabla[,i]) == "character") {
      tabla[,i] = ifelse(is.na(tabla[,i]), modo(tabla[,i]), tabla[,i])
    } else {
      tabla[,i] = ifelse(is.na(tabla[,i]), modo(tabla[,i], na.rm = TRUE), tabla[,i])
    }
    
  }
  
  return(tabla)
  
}


selectVarCor = function(n_vars, target, id, vars_remove, path, name_table) {
  
  # Read table
  if (grepl(".rds", name_table)) {
    table = readRDS(paste0(path, name_table))
  } else if (grepl(".csv", name_table)) {
    table = read.csv(paste0(path, name_table), sep = ",")
  }
  
  # table = read.csv("~/PRUEBAS PERSONALES/1. Titanic/Resultados/tablon_completo.csv", sep = ",")
  if (!missing(vars_remove)) {
    table[, c(id, vars_remove)] = NULL
  }
  
  # Put target in the last column
  table = table[, c((1:ncol(table))[-grep(pattern = target, names(table))], grep(pattern = target, names(table)))]
  table = as.data.frame(sapply(table, function (x) as.numeric(x)))
  
  # Cor
  correlacion = cor(table)
  result = as.data.frame(correlacion[grep(pattern = target, names(table)), 1:ncol(table)-1])
  vars = row.names(result)
  result = cbind(vars, result)
  colnames(result) = c("Vars", "Results")
  row.names(result) = NULL
  result = result[order(result[,"Results"], decreasing = TRUE),]
  
  # Vars selected
  vars_select = as.character(result[1:n_vars, "Vars"])
  
  return(vars_select)
  
}

dependenciaChisqVcramer <- function(path, name_table, id, target, vars_remove, limite_chi, limite_cramer) {
  
  # Read table
  if (grepl(".rds", name_table)) {
    table = readRDS(paste0(path, name_table))
  } else if (grepl(".csv", name_table)) {
    table = read.csv(paste0(path, name_table), sep = ",")
  }
  
  # Filter table
  table_target = table[, target]
  dataset = table[, !colnames(table) %in% c(id, target, vars_remove)]
  
  # Create differents functions
  chiVcramer = function(variable, tablon_target){
    data = table(variable, tablon_target,useNA = "ifany")
    
    if (nrow(data) < 2 | ncol(data) < 2 ) {
      use = c(value_chi = 1, value_vcramer = 0)
    } else {
      chi_2 = chisq.test(data)
      prueba_chi = chi_2$p.value
      prueba_cramer <- sqrt(chi_2$statistic / (sum(data) * (min(c(nrow(data), ncol(data)) - 1))))
      use = c(value_chi = prueba_chi, value_vcramer = unname(prueba_cramer))
    }
    return(use) 
  }
  
  quantiles = function(q) {
    quantil = quantile(q, probs = seq(0, 1, 0.1))
    quantil = quantil[!duplicated(quantil)]
    
    if (length(quantil) > 1) {
      cortes = as.character(cut(q, breaks = quantil))
    } else {
      cortes = as.character(q)
    }
    return(cortes)
  }
  
  finalFunction = function(var, tablon_target) {
    if (class(var) %in% c("numeric", "integer")) {
      x1 = quantiles(var)
      return(chiVcramer(x1, table_target))
    } else {
      return(chiVcramer(x, table_target))
    }
  }
  
  # Go through the different variables of the data set
  result = data.frame()
  for (i in 1:ncol(dataset)) {
    result = rbind(result, finalFunction(dataset[,i], table_target))
  }
  
  # Create final data set
  row.names(result) = NULL
  colnames(result) = c("value_chi", "value_vcramer")
  result$dependencia_chi <- ifelse(result$value_chi > limite_chi, "independientes", "dependientes")
  result$dependencia_Cramer <- ifelse(result$value_vcramer > limite_cramer, "dependientes", "independientes")
  result$variable <- names(dataset)
  
  return(result[, c('variable', 'value_chi', 'value_vcramer', 'dependencia_chi', 'dependencia_Cramer')])
  
}

