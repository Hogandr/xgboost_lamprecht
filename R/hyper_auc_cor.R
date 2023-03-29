library(readr)
path <- setwd()

xgboost_meta_data <- read_csv(paste(path, "Data/xgboost_meta_data.csv", sep = "/"))

#function to calculate average cor per tasks of hyperparameter and auc
cor_data <- function(data, mean = F) {
  id <- unique(data$data_id)
  features_names <- c("num_round", "eta", "subsample", "max_depth", "min_child_weight", 
                      "colsample_bytree", "colsample_bylevel", "lambda", "alpha", "gamma")
  #loop over 94 tasks to calculate cor
  temp_cor <- data.frame(row.names = c(features_names))
  for (i in id) {
    temp <- i
    temp_data <- data[data$data_id == i, ]
    temp_feature <- temp_data[features_names]
    temp_auc <- temp_data$auc
    temp_cor <- cbind(temp_cor, cor(temp_feature, temp_auc))
  }
  colnames(temp_cor) <- id
  
  #get raw data or mean over all tasks
  if (mean == F) {
    temp_cor
  } else {
    temp_cor <- rowMeans(temp_cor)
    temp_cor <- data.frame(temp_cor)
    rownames(temp_cor) <- features_names
    colnames(temp_cor) <- "Cor_AUC"
    temp_cor
  }
  
}
data_cor <- cor_data(data = xgboost_meta_data, mean = T)

write.csv(data_cor, paste(path, "Data/hyper_auc_cor.csv", sep = "/"), row.names = F)

