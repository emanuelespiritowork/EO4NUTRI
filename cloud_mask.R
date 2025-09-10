#cloud_mask

mask_cloud <- function(master_path = "//10.0.1.243/nr_working/emanuele/Progetto_EO4NUTRI/Images_for_composite",
                       cm_path = "//10.0.1.243/nr_data/3_rs_data/PRISMA/JDS/Vectors/Cloud_masks/"){
  library(tidyverse)
  
  import_cloud_mask_2022 <- sf::st_read(paste0(cm_path,"Cloud_2022.shp"))
  import_cloud_mask_2023 <- sf::st_read(paste0(cm_path,"Cloud_2023.shp"))
  import_cloud_mask_2024 <- sf::st_read(paste0(cm_path,"Cloud_2024.shp"))
  
  #split in list
  only_images_2022 <- import_cloud_mask_2022 %>% select(starts_with("X"))
  
  list_of_images_2022 <- list()
  i <- 0
  for(i in 1:(ncol(only_images_2022)-1)){
    list_of_images_2022[[i]] <- only_images_2022[,i]
  }
  
  only_images_2023 <- import_cloud_mask_2023 %>% select(starts_with("X"))
  
  list_of_images_2023 <- list()
  i <- 0
  for(i in 1:(ncol(only_images_2023)-1)){
    list_of_images_2023[[i]] <- only_images_2023[,i]
  }
  
  only_images_2024 <- import_cloud_mask_2024 %>% select(starts_with("X"))
  
  list_of_images_2024 <- list()
  i <- 0
  for(i in 1:(ncol(only_images_2024)-1)){
    list_of_images_2024[[i]] <- only_images_2024[,i]
  }
  
  list_of_all_images <- c(list_of_images_2022,list_of_images_2023,list_of_images_2024)
  
  #put 0 outside
  put_0_outside <- function(df){
    df_prova <- df
    df_prova <- df_prova %>% tidytable::select(starts_with("X"))
    df_prova[is.na(df_prova[,1]),] <- 0
    df[,1] <- df_prova
    return(df)
  }
  
  list_of_all_images_with_0 <- lapply(list_of_all_images,put_0_outside)
  
  master_path <- 
  
  list_of_files <- list.files(master_path, pattern = "[0-9]{8}\\.tif$", full.names = T)
  
  cloud <- function(image_path, df_mask){
    image <- terra::rast(image_path)
    df_mask_only_1 <- df_mask[data.table::as.data.table(df_mask)[,1] == 1,]
    image_masked <- terra::mask(image,df_mask_only_1,inverse = T)
    return(image_masked)
  }
  
  cloud_mask <- NULL
  for(i in 1:length(list_of_files)){
    column_name <- paste0("X",gsub(".tif","",basename(list_of_files[[i]])))
    for(j in 1:length(list_of_all_images_with_0)){
      if(names(list_of_all_images_with_0[[j]])[1] == column_name){
        cloud_mask <- list_of_all_images_with_0[[j]]
      }
    }
    
    if(!is.null(cloud_mask)){
      masked <- cloud(list_of_files[[i]],cloud_mask)
      terra::writeRaster(masked,paste0(master_path,"/",gsub(".tif","_cloud.tif",basename(list_of_files[[i]]))), overwrite = T)
    }
    
    cloud_mask <- NULL
  }
  
  
  
}
















