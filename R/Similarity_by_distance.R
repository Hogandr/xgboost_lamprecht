library(tidyr)
library(readr)
library(dplyr)
library(data.table)

path <- setwd()
 
#normal eval function, but on top differentiation between tasks, because task 168336 doesnt have all meta features
eval_task <- function(task = 22, hyper = default_config) {
  meta = c("data_id", "name", "status", "MajorityClassSize", "MaxNominalAttDistinctValues",
           "MinorityClassSize", "NumberOfClasses", "NumberOfFeatures", "NumberOfInstances",
           "NumberOfInstancesWithMissingValues", "NumberOfMissingValues", "NumberOfNumericFeatures",
           "NumberOfSymbolicFeatures")
  if (task == 168336) {
    meta <- meta[-5]
  }
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

xgboost_meta_data <- read_csv(paste(path, "Data/xgboost_meta_data.csv", sep = "/"))


#get features of pre evaluated tasks
features_train <- read_csv(paste(path, "Data/features_imputed.csv", sep = "/"))
features_train$class <- "train"
features_train$status <- NULL
features_train$version <- NULL
#get features of new tasks
features_new <- read_csv(paste(path, "Data/meta_feat.csv", sep = "/"))
features_new$class <- "new"
features_new$status <- NULL
#get correlation of meta features with auc (as weights later)
AUC_Meta_Feature_1 <- read_csv(paste(path, "Data/AUC_Meta_Feature_1.csv", sep = "/"))

#min max normalization
nor <-function(x) {  (x -min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T)) }


#normalizing data
data_norm <- as.data.frame(rbind(features_train, features_new))
data_norm[,names(features_train)[3:12]] <- as.data.frame(apply(as.data.frame(rbind(features_train, features_new)[,names(features_train)[3:12]]), 2, nor))


#split data 
train <- data_norm[data_norm$class == "train",]
new <- data_norm[data_norm$class == "new",]
train_id <- train$data_id
new_id <- new$data_id
new[, c("name", "class", "data_id")] <- NULL

#function to calculate distance of meta features of new tasks to the features of preevaluated
eu_dist <- function(train_temp = train, new_temp = new[1,], weight = rep(1, 10)) {
  
  new_temp <- new_temp
  train_id <- train_temp$data_id
  train_temp[, c("name", "class", "data_id")] <- NULL
  output <- data_frame()
  #loop over 94 tasks
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


#weights for distance function 
AUC_Meta_Feature_modi <- AUC_Meta_Feature_1

#function to evaluate new config proposed by distance measure
eval_euclidean <- function(l = 1, p = 1) {
  #get p best (lowest distance) preevaluated tasks 
  validation <- eu_dist(new_temp = new[l,], weight = abs(AUC_Meta_Feature_modi$Cor_AUC)) %>%
    arrange(desc(eucl_dist)) %>%
    slice(1:p) 
  
  
  features_names <- c("num_round", "eta", "subsample", "max_depth", "min_child_weight", 
                      "colsample_bytree", "colsample_bylevel", "lambda", "alpha", "gamma", "data_id", "auc")
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
  
  temp_feature <- xgboost_meta_data[features_names]
  #filter by p best tasks
  dataf <- temp_feature[temp_feature$data_id %in% validation$id,]
  #create variable to delete duplicated values
  dataf <- dataf %>%
    unite("Feature_id", num_round:data_id, sep= "-", 
          remove = F)
  
  #mean of duplicated values
  setDT(dataf)
  dataf <- as.data.frame(dataf[, lapply(.SD, mean), by = .(Feature_id)])
  
  dataf$Feature_id <- NULL
  dataf <- dataf %>%
    unite("Feature", num_round:gamma, sep= "-", 
          remove = F)
  
  #get best hyperparameter config over p tasks (best is definied as highest product over auc)
  best_average <- names(which.max(lapply(split(dataf, dataf$Feature), 
                                         function(y) if (length(unique(y$data_id)) == length(validation$id)) prod(y$auc)
                                         else 0))) 
  #filter hyper config
  best_average_confi <- dataf[dataf$Feature == best_average,][1,]
  best_average_config <- as.data.frame(best_average_confi[, features_names])
  best_average_config$data_id <- NULL
  best_average_config$auc <- NULL
  #transform hyper config into a list
  hyper_list <- list()     
  for(j in 1:length(best_average_config)) {             
    hyper_list[[j]] <- unname(best_average_config)[[j]]
  }
  names(hyper_list) <- names(default_config)
  
  test_ids = c(16, 22, 31, 2074, 2079, 3493, 3907, 3913, 9950, 9952, 9971, 10106, 14954, 14970, 146212, 146825, 167119, 167125, 168332, 168336)
  
  #if you want to have the dafault as hyper just put in default_config instead
  temp <- list(new = eval_task(task = test_ids[l], hyper = hyper_list))
  temp$hyper <- hyper_list
  temp
  
}

temp_test_ids = c(16, 22, 31, 2074, 2079, 3493, 3907, 3913, 9950, 9952, 9971, 10106, 14954, 14970, 146212, 146825, 167119, 167125, 168332, 168336)

test_n <- 4

com_test <- data.frame()

for (temp_p in test_n) {
  set.seed(1175)
  for (temp_l in seq_len(length(temp_test_ids))) {
    #loop over all 20 tasks 
    #calculate time to evaluate
    temp_time <- Sys.time()
    temp_eval <- eval_euclidean(l = temp_l,p = temp_p)
    temp_eval$eval_time <- Sys.time() - temp_time
    temp_eval$l <- temp_l
    temp_eval$p <- temp_p
    temp_eval <- as.data.frame(temp_eval)
    com_test <- rbind(com_test, temp_eval)
    write.csv(com_test, paste(path, "Data/Evaluation.csv", sep = "/"), row.names = F)
    
  }
}
