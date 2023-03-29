source("utils.R")

set.seed(2906)
task_id = sample(test_ids, size = 1L)
cat(sprintf("Task ID: %i", task_id), "\n")
task_id<-167125
train_test_tasks = get_train_test_tasks(task_id)

learner = lrn("classif.xgboost")
learner$param_set$values = insert_named(learner$param_set$values, default_config)
learner$predict_type = "prob"

meta_features = get_task_metafeatures(task_id, meta_feature_names = meta_feature_names)
cat(sprintf("The meta features of task ID %i are:", task_id), "\n")
print(meta_features)

learner$train(train_test_tasks$train_task)
measure = if (meta_features$NumberOfClasses > 2L) msr("classif.mauc_aunp") else msr("classif.auc")

auc = learner$predict(train_test_tasks$test_task)$score(measure)
cat(sprintf("The default config scored an AUC of %f in %fs on task ID %i.", auc, learner$state$train_time, task_id), "\n")
