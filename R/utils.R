library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
library(mlr3oml)
library(mlr3misc)
library(data.table)

# note: you may want to enable caching of the openml tasks via `options(mlr3oml.cache = TRUE)`

test_ids = c(16, 22, 31, 2074, 2079, 3493, 3907, 3913, 9950, 9952, 9971, 10106, 14954, 14970, 146212, 146825, 167119, 167125, 168332, 168336)

meta_feature_names = c("data_id", "name", "status", "MajorityClassSize", "MaxNominalAttDistinctValues",
                       "MinorityClassSize", "NumberOfClasses", "NumberOfFeatures", "NumberOfInstances",
                       "NumberOfInstancesWithMissingValues", "NumberOfMissingValues", "NumberOfNumericFeatures",
                       "NumberOfSymbolicFeatures")

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

#' Get meta features of a task
#' requires internet connection
#' @param task_id (`integer(1)`) task id
#' @param meta_feature_names (`character()`) names of meta features
get_task_metafeatures = function(task_id, meta_feature_names) {
  meta_features = list_oml_tasks(task_id)
  meta_features[, ..meta_feature_names]
}

#' Get the train and test task associated with a task
#' requires internet connection
#' only uses the first fold of the 10-fold CV of a task
#' @param task_id (`integer(1)`) task id
get_train_test_tasks = function(task_id) {
  task = tsk("oml", task_id = task_id)
  resampling = rsmp("oml", task_id = task_id)
  # we only use the first of the 10 CV folds
  train_idx = resampling$train_set(1L)
  test_idx = resampling$test_set(1L)

  train_task = task$clone()$filter(rows = train_idx)
  test_task = task$clone()$filter(rows = test_idx)

  # we preprocess externally here
  # you could also work with a `GraphLearner` (including the xgboost learner at the end) as the learner directly
  preprocessor = po("imputemode", affect_columns = selector_type(c("logical", "character", "factor", "ordered"))) %>>%
    po("imputemedian", affect_columns = selector_type(c("numeric", "integer"))) %>>%
    po("encode")

  train_task = preprocessor$train(train_task)[[1L]]
  test_task = preprocessor$predict(test_task)[[1L]]

  list(train_task = train_task, test_task = test_task)
}
