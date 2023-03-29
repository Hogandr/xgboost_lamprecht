library(dplyr)
library(tidyr)
library(readr)
library(rpart)

path <- "/Users/hagenlamprecht/Documents/University/WS_22_23/Automated_ML/XGBoost"

#same function as given
eval_task <- function(task = 22, meta = meta_feature_names, hyper = default_config) {
  setwd(paste(path, "R", sep = "/"))
  source("utils.R")
  
  task_id = task
  cat(sprintf("Task ID: %i", task_id), "\n")
  
  train_test_tasks = get_train_test_tasks(task_id)
  
  learner = lrn("classif.xgboost")
  learner$param_set$values = insert_named(learner$param_set$values, hyper)
  learner$predict_type = "prob"
  
  meta_features = get_task_metafeatures(task_id, meta_feature_names = meta)
  cat(sprintf("The meta features of task ID %i are:", task_id), "\n")
  print(meta_features)
  
  learner$param_set$values$nthread <- 8
  learner$train(train_test_tasks$train_task)
  measure = if (meta_features$NumberOfClasses > 2L) msr("classif.mauc_aunp") else msr("classif.auc")
  auc = learner$predict(train_test_tasks$test_task)$score(measure)
  data.frame(ID = task_id, AUC = auc)
}


features_train <- read_csv(paste(path, "Data/features_imputed.csv", sep = "/"))
features_train$class <- "train"
features_train$status <- NULL
features_train$version <- NULL
features_new <- read_csv(paste(path, "Data/meta_feat.csv", sep = "/"))
features_new$class <- "new"
features_new$status <- NULL



nor <-function(x) {  (x -min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T)) }

#normalizing data (not necessary since tree is used)
data_norm <- as.data.frame(rbind(features_train, features_new))
data_norm[,names(features_train)[3:12]] <- as.data.frame(apply(as.data.frame(rbind(features_train, features_new)[,names(features_train)[3:12]]), 2, nor))


#get train data (evaluated tasks) and new data (test task)
train <- data_norm[data_norm$class == "train",]
new <- data_norm[data_norm$class == "new",]
train[, c("data_id", "class")] <- NULL
new[, c("data_id", "class")] <- NULL
new$name <- NULL


xgboost_meta_data <- read_csv(paste(path, "Data/xgboost_meta_data.csv", sep = "/"))

features_names <- c("num_round", "eta", "subsample", "max_depth", "min_child_weight", 
                    "colsample_bytree", "colsample_bylevel", "lambda", "alpha", "gamma", "data_id", "auc")

temp_feature <- xgboost_meta_data[features_names]
dataf <- temp_feature %>%
  unite("Feature", num_round:gamma, sep= "-", 
        remove = F)




#function to evaluate new task with best config over preevaluated taks in same leaf 
rpart_eval_mean <- function(i, default = TRUE, ran_for_temp = ran_for, new_temp = new, 
                            data_norm_temp = data_norm, dataf_temp = dataf) {

  #predict new task with given decision tree
  temp <- predict(ran_for_temp, newdata = new_temp[i,])
  #find all tasks with value of greater than zero in leaf
  temp_out <- colnames(temp)[colSums(temp) > 0]
  cat("Number of used tasks: ", length(temp_out), "    ")
  out <- data_norm_temp[data_norm_temp$class == "train",]
  #get ids of tasks
  id <- out[out$name %in% temp_out,]$data_id
  temp_new <- dataf_temp[dataf_temp$data_id %in% id,]
  
  
  features_names <- c("num_round", "eta", "subsample", "max_depth", "min_child_weight", 
                      "colsample_bytree", "colsample_bylevel", "lambda", "alpha", "gamma")
  default_config = list(
    nrounds = 464,
    eta = 0.0082,
    subsample = 0.982,
    max_depth = 11,
    min_child_weight = 3.30,
    colsample_bytree = 0.975,
    colsample_bylevel = 0.9,
    lambda = 0.06068,
    alpha = 0.00235,
    gamma = 0
  )
  #get best config by highest value for p norm 
  best_average <- names(which.max(lapply(split(temp_new, temp_new$Feature), function(y) if (length(unique(y$data_id)) == length(temp_out)) prod(y$auc)
                                         else 0)))
  print(temp_new[temp_new$Feature == best_average,][1,])
  #filter best hyper config
  best_average_confi <- temp_new[temp_new$Feature == best_average,][1,]
  best_average_config <- as.data.frame(best_average_confi[, features_names])
  
  #transform hyperparameter dataframe into list 
  hyper_list <- list()     
  for(j in 1:length(best_average_config)) {             
    hyper_list[[j]] <- unname(best_average_config)[[j]]
  }
  names(hyper_list) <- names(default_config)
  print(hyper_list)
  #evaluate new tasks with "best" hyperconfig
  test_ids = c(16, 22, 31, 2074, 2079, 3493, 3907, 3913)
  if (default == FALSE) default_config <- hyper_list
  eval_task(task = test_ids[i], hyper = default_config)
  
}


test_ids = c(16, 22, 31, 2074, 2079, 3493, 3907, 3913)
temp_out <- data_frame()
#decision tree model for evaluation of new task (parameters to differ are minbucket and input variables)
ran_for <- rpart(name~., data=train, control = list(maxdepth = 94, minsplit = 1, minbucket = 4))

set.seed(1175)
for (id_temp in c(1:8)) {
  temp <- list(new = rpart_eval_mean(ran_for_temp = ran_for, i = id_temp, default = F))
  temp_out <- rbind(temp_out, data.frame(temp$new$AUC, m = 4, id = id_temp))
    
}

