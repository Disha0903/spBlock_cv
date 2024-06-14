library(dismo)
library(raster)
library(rgdal)
library(terra)
library(dplyr)
library(blockCV)
library(randomForest)
library(stringr)
library(gbm)
library(tidyr)
library(caret)
library(pROC)
library(stars)
library(sf)
library(ggplot2)
library(lightgbm)
library(caret)
library(xgboost)
library(dplyr)
library(readr)
library(purrr)
sessionInfo()

setwd('C:/Users/User/Desktop/proj_cv/train_2003')

#Shape files of Norway and Sweden
no_sh <- shapefile("C:/Users/User/Desktop/proj_cv/shapefiles/NOR_adm/NOR_adm0.shp")
sw_sh <- shapefile("C:/Users/User/Desktop/proj_cv/shapefiles/SWE_adm/SWE_adm0.shp")
sh <- rbind(no_sh, sw_sh, makeUniqueIDs = TRUE)

all_rasters <- list.files(path = "C:/Users/User/Desktop/proj_cv/train_2003", pattern='.asc', 
                          all.files=TRUE, full.names=FALSE)


env_data <- function(lst_rasters){
  bioclim <- lapply(lst_rasters, raster)
  stack1 <- stack(bioclim)
  change_names <- function(stack1){
    names(stack1[[1]]) <- 'bio1'
    names(stack1[[2]]) <- 'bio10'
    names(stack1[[3]]) <- 'bio11'
    names(stack1[[4]]) <- 'bio12'
    names(stack1[[5]]) <- 'bio13'
    names(stack1[[6]]) <- 'bio14'
    names(stack1[[7]]) <- 'bio15'
    names(stack1[[8]]) <- 'bio16'
    names(stack1[[9]]) <- 'bio17'
    names(stack1[[10]]) <- 'bio18'
    names(stack1[[11]]) <- 'bio19'
    names(stack1[[12]]) <- 'bio2'
    names(stack1[[13]]) <- 'bio3'
    names(stack1[[14]]) <- 'bio4'
    names(stack1[[15]]) <- 'bio5'
    names(stack1[[16]]) <- 'bio6'
    names(stack1[[17]]) <- 'bio7'
    names(stack1[[18]]) <- 'bio8'
    names(stack1[[19]]) <- 'bio9'
    return(stack1)
  }
  stack1 <- change_names(stack1)
  masked <- mask(x = stack1, mask = sh)
  cropped_env <- crop(x = masked, y = extent(sh))
  crs(cropped_env) <- '+proj=longlat +datum=WGS84'
  return(cropped_env)
}


cropped_env <- env_data(all_rasters)


setwd('C:/Users/User/Desktop/proj_cv/soil/res_changed')
soil <- list.files(path = "C:/Users/User/Desktop/proj_cv/soil/res_changed", pattern='.asc', 
                   all.files=TRUE, full.names=FALSE)

soil <- lapply(soil, raster)
soil <- stack(soil)

masked_soil <- mask(x = soil, mask = sh)
cropped_soil <- crop(x = masked_soil, y = extent(sh))
crs(cropped_soil) <- '+proj=longlat +datum=WGS84'



elev <- raster('C:/Users/User/Desktop/proj_cv/output/wc2.1_2.5m_elev.asc$')
masked_elev <- mask(x = elev, mask = sh)
cropped_elev <- crop(x = masked_elev, y = extent(sh))
crs(cropped_elev) <- '+proj=longlat +datum=WGS84'

final_stack <- stack(c(cropped_env, cropped_elev,cropped_soil))


#Species data
gen_cam_occ <- read.csv('C:/Users/User/Desktop/proj_cv/spThin/occ_gen.csv') #occurrence points 
gen_cam <- read.csv('C:/Users/User/Desktop/proj_cv/gen_2003_2018.csv') #all points 


gen_cam_past_abs <- read.csv('C:/Users/User/Desktop/proj_cv/gen_1994_2002.csv') #absence data for past time interval
gen_cam_past_occ <- read.csv('C:/Users/User/Desktop/proj_cv/spThin/past_occ_gen.csv') #occurrence data for past time interval
gen_cam_past_occ$occurrenceStatus <- 1
gen_cam_past_abs <- gen_cam_past_abs[gen_cam_past_abs$occurrenceStatus ==0,]

gen_cam_past_abs <- gen_cam_past_abs[,c('occurrenceStatus', 'decimalLongitude', 'decimalLatitude')]
gen_cam_past_occ <- gen_cam_past_occ[,c('occurrenceStatus', 'decimalLongitude', 'decimalLatitude')]
gen_cam_past <- rbind(gen_cam_past_abs, gen_cam_past_occ)

#Pre-process data 
preprocess_data <- function(occ_df, abs_df){
  #species data
  occ_df$occurrenceStatus <- 1
  abs_df <- abs_df[abs_df$occurrenceStatus == 0, ]
  occ_df <- occ_df[,c('occurrenceStatus', 'decimalLongitude', 'decimalLatitude')]
  abs_df <- abs_df[,c('occurrenceStatus', 'decimalLongitude', 'decimalLatitude')]
  train <- rbind(occ_df, abs_df)
  pa_data <- st_as_sf(train, coords = c("decimalLongitude", 'decimalLatitude'), crs = crs(cropped_env))
  
  return(pa_data)
}

pa_data <- preprocess_data(gen_cam_occ, gen_cam)


mydata <- raster::extract(final_stack, pa_data, df = TRUE)
mydata$occurrenceStatus <- as.numeric(pa_data$occurrenceStatus)
mydata <- mydata %>% 
  mutate_if(is.numeric, ~replace_na(.,mean(., na.rm = TRUE)))
mydata <- mydata[-1]






##Test of out
setwd("C:/Users/User/Desktop/proj_cv/test_2003")
all_rasters_test <- list.files(path = "C:/Users/User/Desktop/proj_cv/test_2003", pattern='.asc', 
                               all.files=TRUE, full.names=FALSE)
cropped_env_test <- env_data(all_rasters_test)
final_past <- stack(cropped_env_test, cropped_elev,cropped_soil)

gen_cam_past <- gen_cam_past[,c('occurrenceStatus', 'decimalLongitude', 'decimalLatitude')]
pa_data_test<- st_as_sf(gen_cam_past, coords = c("decimalLongitude", 'decimalLatitude'), crs = crs(cropped_env_test))

mydata_past <- raster::extract(final_past, pa_data_test, df = TRUE)
mydata_past$occurrenceStatus <- as.numeric(pa_data_test$occurrenceStatus)
mydata_past <- mydata_past %>% 
  mutate_if(is.numeric, ~replace_na(.,mean(., na.rm = TRUE)))
mydata_past <- mydata_past[-1]




############
#Hyperparameters setting

##gbm
n.trees <- c(50, 100, 250, 500, 750, 1000)
interaction.depth <- c(3, 4, 5, 6) 
shrinkage <- c(0.005,0.01) 
n.minobsinnode <- c(5,10,15,20)

hyperparams <- expand.grid(n.trees=n.trees, 
                           interaction.depth=interaction.depth,
                           shrinkage=shrinkage, 
                           n.minobsinnode=n.minobsinnode) 

unique_hyperparams_gbm <- unique(hyperparams)

set.seed(123)  # For reproducibility
hyperparams <- unique_hyperparams_gbm %>%
  sample_n(120)

##RF
n.trees_rf <- c(50, 100, 250, 500, 750, 900, 1000)
max_depth_rf <- c(3, 4, 5, 6)  
min_samples_split_rf <- c(5, 10, 15, 17, 20)


hyperparams_rf <- expand.grid(
  n.trees = n.trees_rf,
  max_depth = max_depth_rf,
  min_samples_split = min_samples_split_rf
)

unique_hyperparams_rf <- unique(hyperparams_rf)

set.seed(123)  
hyperparams_rf <- unique_hyperparams_rf %>%
  sample_n(120)



##XGBoost
nrounds <- c(50, 100, 500, 1000)
max_depth <- c(3, 5)
eta <- c(0.01, 0.05, 0.1)
subsample <- c(0.6, 0.7, 0.8)
min_child_weight <- c(1, 5, 10)
gamma <- c(0, 0.1, 1)
colsample_bylevel <- c(0.6, 0.7, 0.8)
tree_method <- c("auto", "exact", "approx", "hist")

xgb_hyperparams <- expand.grid(nrounds = nrounds,
                               max_depth = max_depth,
                               eta = eta,
                               subsample = subsample,
                               min_child_weight = min_child_weight,
                               gamma = gamma,
                               colsample_bylevel = colsample_bylevel,
                               tree_method = tree_method)

set.seed(123)  
hyperparams_xgb <- xgb_hyperparams %>%
  sample_n(120)




##LightGBM
num_iterations <- c(50, 100, 500, 1000)
num_leaves <- c(10, 20, 30, 40)
learning_rate <- c(0.01, 0.05, 0.1)
subsample <- c(0.6, 0.7, 0.8)
colsample_bytree <- c(0.6, 0.7, 0.8)

lgbm_hyperparams <- expand.grid(num_iterations = num_iterations,
                                num_leaves = num_leaves,
                                learning_rate = learning_rate,
                                subsample = subsample,
                                colsample_bytree = colsample_bytree)
set.seed(123)  
hyperparams_lgbm <- lgbm_hyperparams %>%
  sample_n(120)









##spatial cv
######################################
block_sizes <- c(200000, 422000, 600000)
model_names <- c("gbm_sp200","gbm_sp422", "gbm_sp600")

for (i in 1:length(block_sizes)) {
  sp_gbm_retrain(block_sizes[i], model_names[i], hyperparams, mydata, pa_data, cropped_env, mydata_past)
  sp_gbm_lastfold(block_sizes[i], model_names[i], hyperparams, mydata, pa_data, cropped_env, mydata_past)
}



 model_names <- c('rf_sp200', "rf_sp422", "rf_sp600")
for (i in 1:length(block_sizes)) {
  sp_rf_retrain(block_sizes[i], model_names[i], hyperparams_rf, mydata, pa_data, cropped_env, mydata_past)
  sp_rf_lastfold(block_sizes[i], model_names[i], hyperparams_rf, mydata, pa_data, cropped_env, mydata_past)
}


model_names <- c("xgb_sp200", "xgb_sp422", "xgb_sp600")
for (i in 1:length(block_sizes)) {
  sp_xgb_retrain(block_sizes[i], model_names[i], hyperparams_xgb, mydata, pa_data, cropped_env, mydata_past)
  sp_xgb_lastfold(block_sizes[i], model_names[i], hyperparams_xgb, mydata, pa_data, cropped_env, mydata_past)
}



model_names <- c("lgb_sp200", "lgb_sp422", "lgb_sp600")
for (i in 1:length(block_sizes)) {
  sp_lgb_retrain(block_sizes[i], model_names[i], hyperparams_lgbm, mydata, pa_data, cropped_env, mydata_past)
  sp_lgb_lastfold(block_sizes[i], model_names[i], hyperparams_lgbm, mydata, pa_data, cropped_env, mydata_past)
}




##Spatio-temporal part
gen_cam <- gen_cam %>%
  mutate(time = case_when(
    year <= 2006 ~ 1,
    year > 2006 & year <= 2010 ~ 2,
    year > 2010 & year <= 2014 ~ 3,
    TRUE ~ 4  # Default case
  ))


result_list <- spatiotemp_occ_data(cropped_elev, cropped_soil)
temp1 <- result_list[[1]]
temp2 <- result_list[[2]]
temp3 <- result_list[[3]]
temp4 <- result_list[[4]]

result_list <- spatiotemp_env_data(cropped_elev,cropped_soil,
                                   temp1,temp2,temp3,temp4, env_data)
# Access individual data frames
cropped_temp1 <- result_list[[1]]
cropped_temp2 <- result_list[[2]]
cropped_temp3 <- result_list[[3]]
cropped_temp4 <- result_list[[4]]

pa_data_temp1 <- create_pa_data(temp1, cropped_temp1, cropped_elev, cropped_soil)
pa_data_temp2 <- create_pa_data(temp2, cropped_temp2, cropped_elev, cropped_soil)
pa_data_temp3 <- create_pa_data(temp3, cropped_temp3, cropped_elev, cropped_soil)
pa_data_temp4 <- create_pa_data(temp4, cropped_temp4, cropped_elev, cropped_soil)


mydata_temp1 <- get_mydata(pa_data_temp1,cropped_temp1)
mydata_temp2 <- get_mydata(pa_data_temp2,cropped_temp2)
mydata_temp3 <- get_mydata(pa_data_temp3,cropped_temp3)
mydata_temp4 <- get_mydata(pa_data_temp4,cropped_temp4)


# Define the list of cropping datasets
time_cropped <- list(cropped_temp1, cropped_temp2, cropped_temp3, cropped_temp4)
time_pa_data <- list(pa_data_temp1, pa_data_temp2, pa_data_temp3, pa_data_temp4)
time_intervals_data <- list(mydata_temp1, mydata_temp2, mydata_temp3, mydata_temp4)

table(time_intervals_data[[1]]$occurrenceStatus)


#block_sizes <- c(200000,422000, 600000)
model_names <- c('gbm_sptemp200','gbm_sptemp422','gbm_sptemp600')

for (i in 1:length(block_sizes)) {
  temp_gbm_retrain(time_cropped, time_pa_data, time_intervals_data, block_sizes[i], model_names[i], hyperparams, mydata, mydata_past)
  temp_gbm_lastfold(time_cropped, time_pa_data, time_intervals_data, block_sizes[i], model_names[i], hyperparams, mydata_past)
}


model_names <- c('rf_sptemp200', 'rf_sptemp422', 'rf_sptemp600')
for (i in 1:length(block_sizes)) {
  temp_rf_retrain(time_cropped, time_pa_data, time_intervals_data, block_sizes[i], model_names[i], hyperparams_rf, mydata, mydata_past)
  temp_rf_lastfold(time_cropped, time_pa_data, time_intervals_data, block_sizes[i], model_names[i], hyperparams_rf, mydata_past)
}



model_names <- c("xgb_sptemp200", "xgb_sptemp422", "xgb_sptemp600")
for (i in 1:length(block_sizes)) {
  temp_xgb_retrain(time_cropped, time_pa_data, time_intervals_data, block_sizes[i], model_names[i], hyperparams_xgb, mydata, mydata_past)
  temp_xgb_lastfold(time_cropped, time_pa_data, time_intervals_data, block_sizes[i], model_names[i], hyperparams_xgb, mydata_past)
}



model_names <- c("lgb_sptemp200","lgb_sptemp422", "lgb_sptemp600")
for (i in 1:length(block_sizes)) {
  temp_lgb_retrain(time_cropped, time_pa_data, time_intervals_data, block_sizes[i], model_names[i], hyperparams_lgbm, mydata, mydata_past)
  temp_lgb_lastfold(time_cropped, time_pa_data, time_intervals_data, block_sizes[i], model_names[i], hyperparams_lgbm, mydata_past)
}





##Random
random_gbm_retrain(hyperparams, mydata, pa_data, cropped_env, mydata_past)
random_gbm_lastfold(hyperparams, mydata, pa_data, cropped_env, mydata_past)

random_rf_retrain(hyperparams_rf, mydata, pa_data, cropped_env, mydata_past)
random_rf_lastfold(hyperparams_rf, mydata, pa_data, cropped_env, mydata_past)

random_xgb_retrain(hyperparams_xgb, mydata, pa_data, cropped_env, mydata_past)
random_xgb_lastfold(hyperparams_xgb, mydata, pa_data, cropped_env, mydata_past)

random_lgb_retrain(hyperparams_lgbm, mydata, pa_data, cropped_env, mydata_past)
random_lgb_lastfold(hyperparams_lgbm, mydata, pa_data, cropped_env, mydata_past)







##Environmental blocking
env_data_ <- mydata[, 1:25]

set.seed(123)
kmeans_result <- kmeans(env_data_, centers = k)## need to find k
# Use the elbow method to find an appropriate value for k
wcss <- numeric(10)  # Adjust the maximum number of clusters to consider

for (i in 1:10) {
  kmeans_result <- kmeans(env_data_, centers = i)
  wcss[i] <- kmeans_result$tot.withinss
}

# Plot the within-cluster sum of squares (WCSS)
plot(1:10, wcss, type = 'b', xlab = 'Number of Clusters', ylab = 'WCSS')
set.seed(123)
kmeans_result <- kmeans(env_data_, centers = 4)
# Access cluster assignments
cluster_assignments <- kmeans_result$cluster


# Initialize an empty list to store data for each cluster
cluster_data <- list()
# Loop through each cluster and subset the data
for (i in 1:max(cluster_assignments)) {
  cluster_i_data <- env_data_[cluster_assignments == i, ]
  cluster_data[[i]] <- cluster_i_data
}


# Assuming you have already performed k-means clustering and stored cluster assignments in 'cluster_assignments'
cluster_count <- max(cluster_assignments)
cluster_data <- list()

#Define a threshold for the minimum number of instances per class in a cluster
min_class_count <- 4  #Adjust as needed

for (i in 1:cluster_count) {
  #Filter data for the current cluster
  cluster_i_data <- mydata[cluster_assignments == i, ]
  
  #Check the class distribution in the current cluster
  class_distribution <- table(cluster_i_data$occurrenceStatus)
  
  # Check if there are at least two classes with a minimum count in the cluster
  if (sum(class_distribution >= min_class_count) >= 2) {
    cluster_data[[i]] <- cluster_i_data
  } else {
    # Combine this cluster with another cluster that has the opposite class
    opposite_class <- ifelse(names(class_distribution) == 1, 0, 1)
    
    for (j in 1:cluster_count) {
      if (j == i) next  # Skip the same cluster
      
      #Filter data for the potential merging cluster
      cluster_j_data <- mydata[cluster_assignments == j, ]
      
      # Check if the potential merging cluster contains the opposite class
      if (sum(cluster_j_data$occurrenceStatus == opposite_class) > 0) {
        cat("Combining Cluster", i, "with Cluster", j, "\n")
        # Combine the data from cluster_i_data and cluster_j_data
        combined_data <- rbind(cluster_i_data, cluster_j_data)
        cluster_data[[i]] <- combined_data
        cluster_data[[j]] <- NULL  # Remove cluster j
        break  # Stop searching for a merging cluster
      }
    }
  }
}


cluster_data <- cluster_data[!sapply(cluster_data, is.null)]
cluster_count <- length(cluster_data)

cluster_data[4]


env_gbm_retrain(hyperparams, cluster_data, cluster_count, mydata, mydata_past)
env_gbm_lastfold(hyperparams, cluster_data, cluster_count, mydata, mydata_past)

env_rf_retrain(hyperparams_rf, cluster_data, cluster_count, mydata, mydata_past)
env_rf_lastfold(hyperparams_rf, cluster_data,cluster_count, mydata, mydata_past)

env_xgb_retrain(hyperparams_xgb, cluster_data,cluster_count, mydata, mydata_past)
env_xgb_lastfold(hyperparams_xgb, cluster_data,cluster_count, mydata, mydata_past)

env_lgb_retrain(hyperparams_lgbm, cluster_data, cluster_count, mydata, mydata_past)
env_lgb_lastfold(hyperparams_lgbm, cluster_data, cluster_count, mydata, mydata_past)


