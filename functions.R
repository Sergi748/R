
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
createDummiesVars = function(tabla, vars = FALSE) {
  
  for (col in 1:ncol(tabla)) {
    if (vars == FALSE) {
      if (class(tabla[,col]) == "character") {
        for (i in levels(as.factor(tabla[,col]))) {
          booleano = sapply(tabla[,col], function(x) ifelse(x == i, 1, 0))
          tabla = cbind(tabla, booleano)
          colnames(tabla)[grep("booleano", names(tabla))] = paste0(colnames(tabla)[col], ".", i)
        }
      }
    } else {
      if (colnames(tabla)[col] %in% varsDummy) {
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
createDummies = function(tabla, remove = FALSE, vars = FALSE) {
  
  varsname = c()
  
  for (col in 1:ncol(tabla)) {
    if (vars == FALSE) {
      if (class(tabla[,col]) == "character") {
        varsname = append(varsname, colnames(tabla)[col])
        for (i in levels(as.factor(tabla[,col]))) {
          booleano = sapply(tabla[,col], function(x) ifelse(x == i, 1, 0))
          tabla = cbind(tabla, booleano)
          colnames(tabla)[grep("booleano", names(tabla))] = paste0(colnames(tabla)[col], ".", i)
        }
      }
    } else {
      if (colnames(tabla)[col] %in% varsDummy) {
        for (i in levels(as.factor(tabla[,col]))) {
          booleano = sapply(tabla[,col], function(x) ifelse(x == i, 1, 0))
          tabla = cbind(tabla, booleano)
          colnames(tabla)[grep("booleano", names(tabla))] = paste0(colnames(tabla)[col], ".", i)
        }
      }
    }
  }
  
  if (remove == TRUE && vars != FALSE) {
    tabla[,varsDummy] = NULL
  } else if (remove == TRUE && vars == FALSE) {
    tabla[,varsname] = NULL
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
