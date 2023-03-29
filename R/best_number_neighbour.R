library(readr)
library(tidyr)
library(dplyr)
path <- "/Users/hagenlamprecht/Documents/University/WS_22_23/Automated_ML/XGBoost"

#get evaluated tasks, meta features and meta feature weights
xgboost_meta_data <- read_csv(paste(path, "Data/xgboost_meta_data.csv", sep = "/"))
xgboost_meta_data <- xgboost_meta_data[-c(2,3,14,16)]
features_train <- read_csv(paste(path, "Data/features_imputed.csv", sep = "/"))
features_train <- features_train[-c(2:4)]
AUC_Meta_Feature_1 <- read_csv(paste(path, "Data/AUC_Meta_Feature_1.csv", sep = "/"))
AUC_Meta_Feature_1$Cor_AUC <- abs(AUC_Meta_Feature_1$Cor_AUC)

#normalize data
nor <-function(x) {  (x -min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T)) }

data_norm <- as.data.frame(features_train)
data_norm[,names(data_norm)[2:11]] <- as.data.frame(apply(as.data.frame(rbind(data_norm)[,names(data_norm)[2:11]]), 2, nor))


#create variable to delete duplicated values
dataf <- xgboost_meta_data %>%
  unite("Feature_id", data_id:colsample_bylevel, sep= "-", 
        remove = F)
#mean of duplicated values
setDT(dataf)
dataf <- as.data.frame(dataf[, lapply(.SD, mean), by = .(Feature_id)])

dataf$Feature_id <- NULL
dataf_xg <- dataf %>%
  unite("Feature", num_round:colsample_bylevel, sep= "-", 
        remove = T)

#distance measure of preevaluated tasks
eu_dis <- function(task = data_norm, index = 1, weight = rep(1, 10)) {
  #split in two datasets for task to measure the distance
  train_temp <- task[-index,]
  new_temp <- task[index,]
  train_id <- train_temp$data_id
  
  train_temp[, c("data_id")] <- NULL
  new_temp[, c("data_id")] <- NULL
  output <- data_frame()
  #loop over 93 tasks
  for (i in seq_len(dim(train_temp)[1])) {
    temp <- train_temp[i,]
    temp_dist <- abs((temp - new_temp)*abs(weight))^2
    temp_dist[is.na(temp_dist)] <- 0
    output <- rbind(output, sum(temp_dist))
  }
  #get weighted quadratic distance of meta features per task
  rownames(output) <- train_id
  colnames(output) <- "eucl_dist"
  output$id <- train_id
  output$eucl_dist <- -output$eucl_dist
  output
}

nearest_tasks <- function(k = 1, meta_data = data_norm, hyper_data = dataf_xg) {
  #ids
  task_ids <- meta_data$data_id
  #init output
  auc <- c()
  for (i in seq_len(94)) {
    #assign id of task to evaluate
    temp_task_id <- task_ids[i]
    #get k best values in sence of distance
    validation <- eu_dis(task = meta_data, index = i, weight = abs(AUC_Meta_Feature_1$Cor_AUC)) %>%
      arrange(desc(eucl_dist)) %>%
      slice(1:k)
    #filter hyper config, that is evaluated on temp_task_id
    temp_hyper <- hyper_data$Feature[hyper_data$data_id == temp_task_id]
    temp_hyper_data <- hyper_data[hyper_data$Feature %in% temp_hyper,]
    #filter nearest tasks
    temp_hyper_data <- temp_hyper_data[temp_hyper_data$data_id %in% validation$id,]
    #return "best" hyper config over nearest tasks
    best_average <- names(which.max(lapply(split(temp_hyper_data, temp_hyper_data$Feature), 
                                           function(y) if (length(unique(y$data_id)) == length(validation$id)) prod(y$auc)
                                           else 0)))
    #get best config for task to evaluate
    best_average_confi <- temp_hyper_data[temp_hyper_data$Feature == best_average,][1,]$Feature
    val_data <- hyper_data[c(hyper_data$data_id == temp_task_id & hyper_data$Feature == best_average_confi),]
    auc <- c(auc, val_data$auc)
  }
  mean(auc)
}

out <- c()
for (num in c(1:5)) {
  out <- c(out, nearest_tasks(k = num))
}
out


