library(raster)
library(rgdal)

combine_raster_stacks <- function(folder1, folder2, shp) {
  # Load shapefile
  shp <- fi_sh
  
  # Function to process raster stacks
  process_raster_stack <- function(raster_folder, shapefile) {
    rasters <- list.files(path = raster_folder, pattern = '.asc', full.names = TRUE)
    stack_env <- stack(rasters)
    names(stack_env) <- c('bio1', 'bio10', 'bio11', 'bio12', 'bio13', 'bio14', 'bio15', 'bio16', 'bio17', 'bio18', 'bio19',
                          'bio2', 'bio3', 'bio4', 'bio5', 'bio6', 'bio7', 'bio8', 'bio9')
    masked <- mask(stack_env, shapefile)
    cropped_env <- crop(masked, extent(shapefile))
    crs(cropped_env) <- crs(shapefile)
    return(cropped_env)
  }
  
  # Process the first raster stack
  stack1 <- process_raster_stack(folder1, shp)
  
  # Process the second raster stack
  stack2 <- process_raster_stack(folder2, shp)
  
  # Find the common extent between the two stacks
  common_extent <- intersect(extent(stack1), extent(stack2))
  
  # Crop the stacks to the common extent
  stack1_cropped <- crop(stack1, common_extent)
  stack2_cropped <- crop(stack2, common_extent)
  
  # Combine the cropped stacks
  combined_stack <- stack(stack1_cropped, stack2_cropped)
  
  # Set the projection of the combined stack
  projection(combined_stack) <- projection(shp)
  
  return(combined_stack)
}


# Example usage
folder1 <- "path_to_folder1"
folder2 <- "path_to_folder2"
shapefile_path <- "path_to_shapefile"

combined_stack <- combine_raster_stacks(folder1, folder2, shapefile_path)

r15 <- raster('2015/bio1.asc')
r19 <-raster('2019/bio1.asc')

stack(r15,r19)
extent(r15)
extent(union(extent(r15), extent(r19)))
plot(r3)
r3 <- crop(r15,extent(r19))
extent(r3)
extent(r15)


















setwd("C:/Users/User/Downloads/Downloads/phd_project/worldclim/bioclimatic")

# Create a vector of bioclimatic variable names
bioclim_names <- paste0("bio", 1:19)

# Read the reference raster layer (bio19)
bio_ref <- raster("2019/bio19.asc")

# Loop through each bioclimatic variable
for (bio in bioclim_names) {
  # Initialize an empty list to store the raster layers for each year
  bio_layers <- list()
  
  # Loop through each year from 2015 to 2021
  for (year in 2013:2015) {
    # Construct the file path for the current year and bioclimatic variable
    file_path <- paste0(year, "/", bio, ".asc")
    
    # Read the raster layer for the current year and crop it to the extent of bio_ref
    bio_layer <- crop(raster(file_path), extent(bio_ref))
    
    # Store the bio_layer in the list
    bio_layers[[as.character(year)]] <- bio_layer
  }
  
  # Combine the raster layers for each year into a single stack
  bio_stack <- stack(bio_layers)
  
  # Calculate the mean for each layer (2015-2021)
  mean_layer <- calc(bio_stack, mean, na.rm = TRUE)
  
  # Create the output file path
  output_file <- paste0("C:/Users/User/Downloads/Downloads/phd_project/worldclim/bioclimatic/averaged/norsw_test_2013_2015/", bio, "_norsw_test_2013_2015.asc")
  
  # Save the output raster
  writeRaster(mean_layer, filename = output_file, format = "ascii")
}










##SPATIOTEMP
setwd("C:/Users/User/Downloads/Downloads/phd_project/worldclim/bioclimatic")

# Create a vector of bioclimatic variable names
bioclim_names <- paste0("bio", 1:19)

# Read the reference raster layer (bio19)
bio_ref <- raster("2019/bio19.asc")

# Loop through each bioclimatic variable
for (bio in bioclim_names) {
  # Initialize an empty list to store the raster layers for each year
  bio_layers <- list()
  
  # Loop through each year from 2015 to 2021
  for (year in 2015:2018) {
    # Construct the file path for the current year and bioclimatic variable
    file_path <- paste0(year, "/", bio, ".asc")
    
    # Read the raster layer for the current year and crop it to the extent of bio_ref
    bio_layer <- crop(raster(file_path), extent(bio_ref))
    
    # Store the bio_layer in the list
    bio_layers[[as.character(year)]] <- bio_layer
  }
  
  # Combine the raster layers for each year into a single stack
  bio_stack <- stack(bio_layers)
  
  # Calculate the mean for each layer (2015-2021)
  mean_layer <- calc(bio_stack, mean, na.rm = TRUE)
  
  # Create the output file path
  output_file <- paste0("C:/Users/User/Downloads/Downloads/phd_project/worldclim/bioclimatic/averaged/spatio_temp/temp4/", bio, "_norsw_test_2015_2018.asc")
  
  # Save the output raster
  writeRaster(mean_layer, filename = output_file, format = "ascii")
}

merged_df[merged_df$time == 1,]
