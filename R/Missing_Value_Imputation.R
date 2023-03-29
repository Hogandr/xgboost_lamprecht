library(readr)
library(dplyr)

path <- "/Users/hagenlamprecht/Documents/University/WS_22_23/Automated_ML/XGBoost"
#get meta features of evaluated tasks
features <- read_csv(paste(path, "Data/features.csv", sep = "/"))



#split into na and non na
features_na <- features[rowSums(is.na(features)) > 0,]
features_re <- features[rowSums(is.na(features)) == 0,]

#delete column with missing values
features_na_in <- features_na[c(1, 5:14)]
features_na_in$MaxNominalAttDistinctValues <- NULL
features_na_in$na <- T
features_re_in <- features_re[c(1, 5:14)]
features_re_in$MaxNominalAttDistinctValues <- NULL
features_re_in$na <- F

#min max normalization
nor <-function(x) {  (x -min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T)) }

#normalizing data
data_norm <- as.data.frame(rbind(features_na_in, features_re_in))
data_norm[,c(2:10)] <- as.data.frame(apply(as.data.frame(data_norm[,c(2:10)]), 2, nor))

features_re_in <- data_norm[data_norm$na == 0, ]
features_na_in <- data_norm[data_norm$na > 0, ]

#function to calculate distance of tasks with na and without na
eu_dist_imput <- function(temp_rel, temp_na) {
  ids <- temp_rel$data_id
  temp_na$data_id <- NULL
  temp_rel$data_id <- NULL
  #loop over all non na tasks
  output <- data_frame()
  for (i in seq_len(dim(temp_rel)[1])) {
    temp <- temp_rel[i,]
    output <- rbind(output, sum((temp - temp_na)^2))
  }
  rownames(output) <- ids
  colnames(output) <- "eucl_dist"
  output$id <- ids
  #get 3 most similar tasks
  output$eucl_dist <- -output$eucl_dist
  output <- output %>%
    arrange(desc(eucl_dist)) %>%
    slice(1:3)
  #get ids of best tasks
  output$id
}


validation_1 <- eu_dist_imput(temp_na = features_na_in[1,], temp_rel = features_re_in) 
validation_2 <- eu_dist_imput(temp_na = features_na_in[2,], temp_rel = features_re_in) 

#impute value as mean of nearest tasks
features[features$data_id == features_na_in[1,]$data_id, 
         ]$MaxNominalAttDistinctValues <- mean(features[features$data_id %in% validation_1,]$MaxNominalAttDistinctValues)
features[features$data_id == features_na_in[2,]$data_id, 
         ]$MaxNominalAttDistinctValues <- mean(features[features$data_id %in% validation_2,]$MaxNominalAttDistinctValues)



write.csv(features, paste(path, "Data/features_imputed.csv", sep = "/"), row.names = F)
