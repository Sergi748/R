
measurement = function(score, percentiles, path_output) {
  
  library(xlsx)
  
  # Order table by variable score
  score = score[order(score$Score, decreasing = TRUE), ]
  # Add percentil
  score$percentil = cut(1:nrow(score), breaks = 100, labels = FALSE)
  # print(table(score$percentil))
  table_final = data.frame()
  # Number of 1´s in the table
  totalIds = sum(score$TARGET)
  # Number of groups
  quantiles = seq(100/percentiles, 100, by = 100/percentiles)
  
  for (i in 1:length(quantiles)){
    
    # print(quantiles[i])
    if (quantiles[i] == quantiles[1]) {
      
      sumIds = nrow(score[score$percentil <= quantiles[i],])
      target = sum(score[score$percentil <= quantiles[i], "TARGET"])
      sumIdsAcum = nrow(score[score$percentil <= quantiles[i],])
      targetAcum = sum(score[score$percentil <= quantiles[i], "TARGET"])
      ratioTarget = target/sumIds
      ratioTargetAcum = targetAcum/sumIdsAcum
      pctTargetCapt = target/totalIds
      pctTargetCaptAcum = targetAcum/totalIds
      minScore = min(score[score$percentil <= quantiles[i], "Score"])
      maxScore = max(score[score$percentil <= quantiles[i], "Score"])
      avgScore = mean(score[score$percentil <= quantiles[i], "Score"])
      data = t(data.frame(c(quantiles[i], sumIds, target, sumIdsAcum, targetAcum, ratioTarget, ratioTargetAcum,
                              pctTargetCapt, pctTargetCaptAcum, minScore, maxScore, avgScore)))
      colnames(data) = c("Quantile", "Sum_Ids", "Target", "Sum_Ids_Acum", "Target_Acum", "Ratio_Target", "Ratio_Target_Acum",
                           "PctOfTargetCapt", "PctOfTargetCapt_Acum", "Min_Score", "Max_Score", "Average_Score")
      table_final = rbind(table_final, data)
      
    } else {
      
      sumIds = nrow(score[score$percentil > quantiles[i - 1] & score$percentil <= quantiles[i],])
      target = sum(score[score$percentil > quantiles[i - 1] & score$percentil <= quantiles[i], "TARGET"])
      sumIdsAcum = nrow(score[score$percentil <= quantiles[i],])
      targetAcum = sum(score[score$percentil <= quantiles[i], "TARGET"])
      ratioTarget = target/sumIds
      ratioTargetAcum = targetAcum/sumIdsAcum
      pctTargetCapt = target/totalIds
      pctTargetCaptAcum = targetAcum/totalIds
      minScore = min(score[score$percentil > quantiles[i - 1] & score$percentil <= quantiles[i], "Score"])
      maxScore = max(score[score$percentil > quantiles[i - 1] & score$percentil <= quantiles[i], "Score"])
      avgScore = mean(score[score$percentil > quantiles[i - 1] & score$percentil <= quantiles[i], "Score"])
      data = t(data.frame(c(quantiles[i], sumIds, target, sumIdsAcum, targetAcum, ratioTarget, ratioTargetAcum,
                              pctTargetCapt, pctTargetCaptAcum, minScore, maxScore, avgScore)))
      colnames(data) = c("Quantile", "Sum_Ids", "Target", "Sum_Ids_Acum", "Target_Acum", "Ratio_Target", "Ratio_Target_Acum",
                           "PctOfTargetCapt", "PctOfTargetCapt_Acum", "Min_Score", "Max_Score", "Average_Score")
      table_final = rbind(table_final, data)
      
    }
    
  }
  
  # Add uplift and uplift acumulate in the table
  table_final$Uplift = table_final$Ratio_Target/table_final$Ratio_Target_Acum[percentiles]
  table_final$Uplift_Acum = table_final$PctOfTargetCapt_Acum*100/table_final$Quantile*100/table_final$Quantile[percentiles]
  
  rownames(table_final) = NULL
  if (!missing(path_output)) {
    write.xlsx(table_final, file.path(path_output, paste0("measurements_", percentiles, "_groups.xlsx")), row.names = FALSE)  
  } else {
    return(table_final)
  }

}

# Example of how to launch it 
measurement(score = table, percentiles = 20, path_output = path_output)

