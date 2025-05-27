#This script demonstrates loading and preprocessing data, defining hyperparameters, and running spatial domain adaptation models.

#Imprort libraries, to install necessary packages read READ.me
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


# ---- SETUP ----
data_path <- "/app/proj_cv/data"
source("functions/functions_spBlock.R")
source("functions/functions_preprocessing.R")
source("config/hyperparams.R")

# ---- SHAPEFILES ----
no_sh <- shapefile(file.path(data_path, "shapefiles/NOR_adm/NOR_adm0.shp"))
sw_sh <- shapefile(file.path(data_path, "shapefiles/SWE_adm/SWE_adm0.shp"))
sh <- rbind(no_sh, sw_sh, makeUniqueIDs = TRUE)

# ---- ENVIRONMENTAL STACKS ----
train_dir <- file.path(data_path, "train_2003")
all_rasters <- list.files(train_dir, pattern = ".asc", full.names = TRUE)
cropped_env <- env_data(all_rasters, sh)

soil_dir <- file.path(data_path, "soil")
soil_rasters <- list.files(soil_dir, pattern = ".asc", full.names = TRUE)
cropped_soil <- prepare_soil(soil_rasters, sh)

elev <- raster(file.path(data_path, "elevation/wc2.1_2.5m_elev.asc"))
cropped_elev <- crop_and_mask_raster(elev, sh)

final_stack <- stack(c(cropped_env, cropped_elev, cropped_soil))

# ---- SPECIES DATA ----
occ_path <- file.path(data_path, "species/occ_gen.csv")
all_path <- file.path(data_path, "species/gen_2003_2018.csv")
past_occ_path <- file.path(data_path, "species/past_occ_gen.csv")
past_abs_path <- file.path(data_path, "species/gen_1994_2002.csv")

gen_cam_occ <- read.csv(occ_path)
gen_cam <- read.csv(all_path)

gen_cam_past_abs <- read.csv(past_abs_path)
gen_cam_past_occ <- read.csv(past_occ_path)
gen_cam_past_occ$occurrenceStatus <- 1
gen_cam_past_abs <- gen_cam_past_abs[gen_cam_past_abs$occurrenceStatus == 0, ]

gen_cam_past <- bind_rows(
  gen_cam_past_abs[, c("occurrenceStatus", "decimalLongitude", "decimalLatitude")],
  gen_cam_past_occ[, c("occurrenceStatus", "decimalLongitude", "decimalLatitude")]
)

# ---- PREPROCESS DATA ----
pa_data <- preprocess_data(gen_cam_occ, gen_cam, crs(final_stack))
in_time_data <- get_in_time_data(pa_data, final_stack)

# ---- PAST DATA ----
test_dir <- file.path(data_path, "test_2003")
rasters_test <- list.files(test_dir, pattern = ".asc", full.names = TRUE)
cropped_env_test <- env_data(rasters_test, sh)
final_past <- stack(cropped_env_test, cropped_elev, cropped_soil)

pa_data_test <- st_as_sf(
  gen_cam_past[, c("occurrenceStatus", "decimalLongitude", "decimalLatitude")],
  coords = c("decimalLongitude", "decimalLatitude"),
  crs = crs(cropped_env_test)
)
out_of_time_data <- get_in_time_data(pa_data_test, final_past)

# ---- SPATIAL CV ----
block_sizes <- c(200000, 422000, 600000)
model_labels <- c("sp200", "sp422", "sp600")

for (i in seq_along(block_sizes)) {
  size <- block_sizes[i]
  label <- model_labels[i]

  sp_gbm(size, paste0("gbm_", label), hyperparams, in_time_data, pa_data, cropped_env, out_of_time_data)
  sp_rf(size, paste0("rf_", label), hyperparams_rf, in_time_data, pa_data, cropped_env, out_of_time_data)
  sp_xgb(size, paste0("xgb_", label), hyperparams_xgb, in_time_data, pa_data, cropped_env, out_of_time_data)
  sp_lgb(size, paste0("lgb_", label), hyperparams_lgbm, in_time_data, pa_data, cropped_env, out_of_time_data)
}



# ---- RANDOM BASELINES ----
random_gbm(hyperparams, in_time_data, pa_data, out_of_time_data, "")
random_rf(hyperparams_rf, in_time_data, pa_data, out_of_time_data, "")
random_xgb(hyperparams_xgb, in_time_data, pa_data, out_of_time_data, "")
random_lgb(hyperparams_lgbm, in_time_data, pa_data, out_of_time_data, "")




# ---- SPATIO-TEMPORAL CV ----
gen_cam <- gen_cam %>%
  mutate(time = case_when(
    year <= 2006 ~ 1,
    year > 2006 & year <= 2010 ~ 2,
    year > 2010 & year <= 2014 ~ 3,
    TRUE ~ 4  # Default case
  ))

# --- Load and merge spatiotemporal occurrence + background data ---
result_list <- spatiotemp_occ_data(gen_cam, path_prefix = "/app/proj_cv/data/spThin")
names(result_list) <- paste0("temp", 1:4)

# --- Load cropped environmental rasters for each time interval ---
cropped_list <- lapply(1:4, function(i) {
  dir_path <- paste0(".../temp", i)
  rasters <- list.files(path = dir_path, pattern = '.asc', full.names = FALSE)
  cropped_temp <- env_data(rasters, shape = sh)
  stack(c(cropped_temp, cropped_elev, cropped_soil))
})

# --- Create pa_data and in_time_data for each time interval ---
pa_data_list <- mapply(
  create_pa_data,
  temp_data = result_list,
  cropped_temp = cropped_list,
  MoreArgs = list(elevation_data = cropped_elev, soil_data = cropped_soil),
  SIMPLIFY = FALSE
)

in_time_data_list <- mapply(get_in_time_data, df = pa_data_list, cropped = cropped_list, SIMPLIFY = FALSE)

# --- Organize into time_* lists for modeling functions ---
time_cropped <- cropped_list
time_pa_data <- pa_data_list
time_intervals_data <- in_time_data_list

# --- Optional: View class distribution in first time split ---
cat("Occurrences in first interval:\n")
print(table(time_intervals_data[[1]]$occurrenceStatus))

# --- Spatio-temporal runs  ---
block_sizes <- c(200000, 422000, 600000)
model_names <- c("gbm_spt200", "gbm_spt422", "gbm_spt600")
spt_gbm(time_cropped, time_pa_data, time_intervals_data, block_sizes, model_names, hyperparams, in_time_data, out_of_time_data, k = 5) #example of gbm modelling

