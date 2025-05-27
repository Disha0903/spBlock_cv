env_data <- function(lst_rasters, shape) {
  bioclim <- lapply(lst_rasters, raster)
  stack1 <- stack(bioclim)

  names(stack1) <- paste0("bio", c(1, 10:19, 2:9))  # concise renaming

  masked <- mask(stack1, shape)
  cropped_env <- crop(masked, extent(shape))
  crs(cropped_env) <- '+proj=longlat +datum=WGS84'
  return(cropped_env)
}


preprocess_data <- function(occ_df, abs_df, crs_target) {
  occ_df$occurrenceStatus <- 1
  abs_df <- abs_df[abs_df$occurrenceStatus == 0, ]
  
  occ_df <- occ_df[, c('occurrenceStatus', 'decimalLongitude', 'decimalLatitude')]
  abs_df <- abs_df[, c('occurrenceStatus', 'decimalLongitude', 'decimalLatitude')]
  
  train <- rbind(occ_df, abs_df)
  pa_data <- st_as_sf(train, coords = c("decimalLongitude", 'decimalLatitude'), crs = crs_target)
  return(pa_data)
}



# Create raster-extracted environmental data
get_in_time_data <- function(df, cropped){
  in_time_data <- raster::extract(cropped, df, df = TRUE)
  in_time_data$occurrenceStatus <- as.numeric(df$occurrenceStatus)
  in_time_data <- in_time_data %>% 
    mutate_if(is.numeric, ~replace_na(.,mean(., na.rm = TRUE)))
  in_time_data <- in_time_data[-1]
  in_time_data <- na.omit(in_time_data)
  return(in_time_data)
}




# Create combined PA spatial data from environmental stack
create_pa_data <- function(temp_data, cropped_temp, elevation_data, soil_data) {
  stacked_data <- stack(c(cropped_temp, elevation_data, soil_data))
  pa_data_temp <- st_as_sf(temp_data, coords = c("decimalLongitude", 'decimalLatitude'), crs = crs(stacked_data))
  return(pa_data_temp)
}




# Load and prepare spatiotemporal occurrence data
spatiotemp_occ_data <- function(gen_cam) {
  temp1 <- read.csv(".../gen_spatiotemp_1.csv")
  temp2 <- read.csv(".../gen_spatiotemp_2.csv")
  temp3 <- read.csv(".../gen_spatiotemp_3.csv")
  temp4 <- read.csv(".../gen_spatiotemp_4.csv")

  for (temp in list(temp1, temp2, temp3, temp4)) temp$occurrenceStatus <- 1

  abs <- gen_cam[gen_cam$occurrenceStatus == 0,]
  abs_split <- split(abs, abs$time)
  occ_split <- list(temp1, temp2, temp3, temp4)

  combined_data_list <- lapply(1:4, function(i) {
    occ <- occ_split[[i]][, 2:4]
    ab <- abs_split[[as.character(i)]][, 1:3]
    rbind(occ, ab)})
  return(combined_data_list)
}




# Load and combine spatiotemporal environmental data
spatiotemp_env_data <- function(cropped_elev, cropped_soil, shape, env_data_func) {
  paths <- paste0(".../spatio_temp/temp", 1:4)
  cropped_list <- lapply(paths, function(path) {
    setwd(path)
    temp_rasters <- list.files(path, pattern = ".asc", full.names = FALSE)
    cropped <- env_data_func(temp_rasters, shape)
    stack(c(cropped, cropped_elev, cropped_soil))
  })
  return(cropped_list)
}


