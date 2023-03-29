library(readr)
library(dplyr)
path <- "/Users/hagenlamprecht/Documents/University/WS_22_23/Automated_ML/XGBoost"
setwd(paste(path, "R", sep = "/"))
source("utils.R")

features <- read_csv(paste(path, "Data/features.csv", sep = "/"))
features <- features[-3]
meta_feature_names = names(features)

test_ids = c(16, 22, 31, 2074, 2079, 3493, 3907, 3913, 9950, 9952, 9971, 10106, 14954, 14970, 146212, 146825, 167119, 167125, 168332, 168336)

meta_feature <- data.frame()
for (i in test_ids) {
  if (i == 168336) {
    meta_feature <- bind_rows(meta_feature, get_task_metafeatures(i, meta_feature_names = meta_feature_names[-5]))
  } else {
    meta_feature <- rbind(meta_feature, get_task_metafeatures(i, meta_feature_names = meta_feature_names))
  }
}

write.csv(meta_feature, paste(path, "Data/meta_feat.csv", sep = "/"), row.names = F)
