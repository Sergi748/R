
findBetterLeaf = function(tree, Number1) {
  
  # Create table 
  tablon = as.data.frame(tree$frame$dev)
  colnames(tablon)[1] = "NumberOnes"
  tablon$NumberZeros = tree$frame$n - tablon$NumberOnes
  tablon$Orden = 1:nrow(tablon)
  tablon$Node = row.names(tree$frame)
  tablon$Pct = (tablon$NumberZeros/length(tree$where))*100
  tablon = tablon[order(tablon$NumberOnes, -tablon$NumberZeros),]
  tablon_filter = tablon[tablon$NumberOnes == Number1, ]
  tablon_filter = tablon_filter[order(tablon_filter$NumberZeros, decreasing = TRUE), ]
  # path of better node
  leaf_path = as.list(path.rpart(tree, nodes = tablon_filter[1, "Node"], print.it = FALSE)) 
  table_leaf = data.frame()
  
  for (i in 2:length(leaf_path[[1]])) {
  
    a = leaf_path[[1]][i]
    all_parts <- strsplit(a, split = "<|<=|>|>=|=", perl = T)
    all_parts <- all_parts[[1]][sapply(all_parts[[1]], function(x) x != "")]
    table_2 = data.frame(all_parts[1],
                         regmatches(a, regexec(">=|<=|>|<|=", a, perl = T))[[1]], 
                         gsub(" ", "", all_parts[2]))
    colnames(table_2) = c("Variable", "Signo", "Value")
    table_leaf = rbind(table_leaf, table_2)
    
  }
  
  vars = as.character(unique(tree$frame$var[tree$frame$var != "<leaf>"]))
  
  return (list(tablon, table_leaf, vars))

}

# Example of how to launch it 
leaf = findBetterLeaf(tree = tree, Number1 = 0)
table = leaf[[1]]
table_path = leaf[[2]]
vars = leaf[[3]]

                                       
