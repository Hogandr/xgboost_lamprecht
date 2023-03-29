library(tidyr)
library(data.table)
library(readr)

path <- setwd()

#get meta features
features <- read_csv(paste(path, "Data/features.csv", sep = "/"))

features_names <- c("num_round", "eta", "subsample", "max_depth", "min_child_weight", 
                    "colsample_bytree", "colsample_bylevel", "lambda", "alpha", "gamma", "data_id", "auc")

#get preevaluated tasks
xgboost_meta_data <- read_csv(paste(path, "Data/xgboost_meta_data.csv", sep = "/"))

#unique id for tasks and hyper config
temp_feature <- xgboost_meta_data[features_names]
dataf <- temp_feature %>%
  unite("Feature_id", num_round:data_id, sep= "-", 
        remove = F)


setDT(dataf)
#mean over duplicted taks
dataf <- as.data.frame(dataf[, lapply(.SD, mean), by = .(Feature_id)])
dim(dataf)
dataf$Feature_id <- NULL



#all hyper configs as one identifier
dataf <- temp_feature %>%
  unite("Feature", num_round:gamma, sep= "-", 
        remove = TRUE)

#combine meta features and hyper data
dataf <- merge(dataf, features, by="data_id")

dataf <- dataf[rowSums(is.na(dataf)) == 0,]

#calculate cor between meta feature and auc for given hyper config
cor_data <- function(data) {
  #get hyperconfig
  id <- unique(data$Feature)
  meta_feature_names = c("MajorityClassSize", "MaxNominalAttDistinctValues",
                         "MinorityClassSize", "NumberOfClasses", "NumberOfFeatures", "NumberOfInstances",
                         "NumberOfInstancesWithMissingValues", "NumberOfMissingValues", "NumberOfNumericFeatures",
                         "NumberOfSymbolicFeatures")
  
  #initialize dataframe
  temp_cor <- data.frame(row.names = c(meta_feature_names))
  for (i in id) {
    #filter by hyper config
    temp <- i
    temp_data <- data[data$Feature == i, ]
    temp_feature <- temp_data[meta_feature_names]
    temp_auc <- temp_data$auc
    #add column with cor between meta features and auc
    temp_cor <- cbind(temp_cor, cor(temp_feature, temp_auc))
  }
  colnames(temp_cor) <- id
  
  temp_cor
  
}
#
temp <- cor_data(dataf)
temp_1 <- temp[,colSums(is.na(temp)) == 0]
#get mean cor per meta feature
temp_cor <- rowMeans(temp_1)
temp_cor <- data.frame(temp_cor)
colnames(temp_cor) <- "Cor_AUC"
#get var per feature cor calculation
temp_cor$var <- apply(temp_1, 1, FUN = var)[rownames(temp_cor)]
temp_cor$Meta_Features <- rownames(temp_cor)


write.csv(temp_cor, paste(path, "Data/AUC_Meta_Feature_1.csv", sep = "/"), row.names = F)

