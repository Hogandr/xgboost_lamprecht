library(tidyr)
library(readr)
library(data.table)
path <- setwd()

#normal eval function
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

#xgboost data
xgboost_meta_data <- read_csv(paste(path, "Data/xgboost_meta_data.csv", sep = "/"))

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
#filter xgboost by sepcific columns
temp_feature <- xgboost_meta_data[features_names]
#get unique id for hyper config and data_id
dataf <- temp_feature %>%
  unite("Feature_id", num_round:data_id, sep= "-", 
        remove = F)


setDT(dataf)
#get mean value over all duplicated evaluated combinations
dataf <- as.data.frame(dataf[, lapply(.SD, mean), by = .(Feature_id)])
dim(dataf)
dataf$Feature_id <- NULL
#get unique column for hyperparameters
dataf <- dataf %>%
  unite("Feature", num_round:gamma, sep= "-", 
        remove = F)


#find best config in sence of p norm with at least 60 evaluated tasks
best_average <- names(which.max(lapply(split(dataf, dataf$Feature), 
                                       function(y) if (length(unique(y$data_id)) >  60) prod(y$auc)^(1/length(y$auc))
                                       else 0)))
#get on row with best config
best_average_confi <- dataf[dataf$Feature == best_average,][1,]
#get hyper values in right order
best_average_config <- as.data.frame(best_average_confi[, features_names])
best_average_config$data_id <- NULL
best_average_config$auc <- NULL
#transform hyper dataframe to list
hyper_list <- list()     
for(j in 1:length(best_average_config)) {             
  hyper_list[[j]] <- unname(best_average_config)[[j]]
}
names(hyper_list) <- names(default_config)

test_ids_new = c(16, 22, 31, 2074, 2079, 3493, 3907, 3913, 9950, 9952, 9971, 10106, 14954, 14970)
#eval all tasks
set.seed(1117)
out <- c()
for (i in test_ids_new) {
  out <- c(out, eval_task(task = i, hyper = hyper_list)$AUC)
  out
}

