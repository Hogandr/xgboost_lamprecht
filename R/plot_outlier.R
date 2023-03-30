library(readr)
library(ggplot2)
library(tidyr)

path <- setwd()
features_train <- read_csv(paste(path, "Data/features_imputed.csv", sep = "/"))
xgboost_meta_data <- read_csv(paste(path, "Data/xgboost_meta_data.csv", sep = "/"))[c("data_id", "task_id")]
xgboost_meta_data <- xgboost_meta_data[!duplicated(xgboost_meta_data),]
features_train <- merge(features_train, xgboost_meta_data, by = "data_id")
features_train[c("name", "version", "status")] <- NULL
features_train$type <- "train"
features_new <- read_csv(paste(path, "Data/meta_feat.csv", sep = "/"))
features_new$task_id <- c(16, 22, 31, 2074, 2079, 3493, 3907, 3913, 9950, 9952, 9971, 10106, 14954, 14970, 146212, 146825, 167119, 167125, 168332, 168336)
features_new[c("name", "status")] <- NULL
features_new$type <- "new"

#min max normalization
nor <-function(x) {  (x -min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T)) }

#normalizing data
data_norm <- as.data.frame(rbind(features_new, features_train))
data_norm[,c(2:11)] <- as.data.frame(apply(as.data.frame(data_norm[,c(2:11)]), 2, nor))
data_norm$data_id <- NULL

#reformat data to get column values in row format
data_norm <- gather(data_norm, key="meta_feature", value="value", 1:10)

#color code
data_norm$task_id <- as.factor(data_norm$task_id)
data_temp <- data_norm[data_norm$task_id %in% features_new$task_id,]
data_temp$color <- "Rest"
data_temp$color[data_temp$task_id == 3913] <- "3913"
data_temp$color[data_temp$task_id == 3907] <- "3907"
data_temp$color[data_temp$task_id == 14954] <- "14954"

#plot meta features of new tasks (focus on bad performing tasks)
ggplot(data_temp, aes(meta_feature, value, group = task_id, colour = color, alpha = color)) + 
  geom_point() +
  geom_path() +   
  scale_color_manual(values=c("green", "yellow","orange", "black")) +
  scale_alpha_manual(values = c(1, 1, 1, 0.4)) +
  coord_cartesian(ylim=c(0, 0.025)) + 
  theme(axis.text.x = element_text(angle=90, hjust = 1, vjust = 0.5))



