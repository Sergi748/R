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
