getEvaluationMetrics <- function(cm) {
  TP <- cm[2,2] 
  TN <- cm[1,1]
  FP <- cm[1,2] 
  FN <- cm[2,1]
  
  accuracy <- sum(diag(cm)) / sum(cm) 
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  F1 <- (2 * precision * recall) / (precision + recall)
  
  c(Accuracy = accuracy, 
    Precision = precision, 
    Recall = recall, 
    F1 = F1)
}
split.fun <- function(x, labs, digits, varlen, faclen)
{
  labs <- gsub(",", " ", labs)
  for(i in 1:length(labs)) {
    
    labs[i] <- paste(strwrap(labs[i], width=25), collapse="\n")
  }
  labs
}
