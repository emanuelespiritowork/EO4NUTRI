#multitemporal composite

list_of_dates <- c(
  "20220401",
  "20220430",
  "20220512",
  "20220627",
  "20220703",
  "20220801",
  #"20221201", contaminata dall'ombra delle nuvole difficilmente distinguibili, foschia ovunque
  "20240407",
  "20240512",
  #"20240627",#brutta come la fame 
  "20240726",
  "20240830",
  "20240928",#prendiamo il dato a visibilita' fissa
  "20241113",
  "20230304",
  "20230407",
  "20230524",
  "20230703",
  "20230726",
  "20230807",
  "20230905",
  "20230911",
  #"20231004",#troppo nuvolosa
  "20231125"
)

bands <- read.table("//10.0.1.243/nr_working/emanuele/Progetto_EO4NUTRI/EO4NUTRI/bands.txt", quote="", comment.char="")$V1

create_image_path <- function(date){
  year <- substr(date,1,4)
  #path <- paste0("//10.0.1.243/nr_data/3_rs_data/PRISMA/JDS/",year,"/L1/","PRS_L1_STD_OFFL_",date,"/Atcor_regrid/PRISMA_resample.tif")
  path <- paste0("//10.0.1.243/nr_data/3_rs_data/PRISMA/JDS/",year,"/L1/","PRS_L1_STD_OFFL_",date,"/Atcor_regrid_crop_smooth/PRISMA_smoothed.tif")
  return(path)
}

list_of_images_path <- lapply(list_of_dates,create_image_path)

read_images <- function(image_path){
  image <- terra::rast(image_path)
  return(image)
}

list_of_images <- lapply(list_of_images_path, read_images)

rename_bands <- function(image){
  get_band_names <- terra::names(image)
  new_band_names <- bands
  terra::set.names(image,new_band_names)
  return(image)
}

list_of_renamed_images <- lapply(list_of_images,rename_bands)

save_images_function <- function(image,percorso = "//10.0.1.243/nr_working/emanuele/Progetto_EO4NUTRI/Images_for_composite/"){
  date <- stringr::str_match(terra::sources(image), "PRS_L1_STD_OFFL_\\s*(.*?)\\s*/Atcor_regrid")[,2]
  terra::writeRaster(image,paste0(percorso,date,".tif"),overwrite=T)
  return(date)
}

lapply(list_of_renamed_images,save_images_function)

source("//10.0.1.243/nr_working/emanuele/Progetto_EO4NUTRI/EO4NUTRI/cloud_mask.R")

mask_cloud()

list_of_images_path_cld <- list.files("//10.0.1.243/nr_working/emanuele/Progetto_EO4NUTRI/Images_for_composite/", pattern = "\\_cloud.tif$", full.names = T)

read_images_after_cloud <- function(image){
  read <- terra::rast(image)
  terra::set.names(read, bands)
  return(read)
}

list_of_renamed_images_after_cloud <- lapply(list_of_images_path_cld, read_images_after_cloud)

apply_filters <- function(image){
  #create NDVI layer
  #use the new bands 664 (ex 660) and 806 (ex 802)
  NDVI_layer <- (image$`0.806711` - image$`0.664894`) / (image$`0.806711` + image$`0.664894`)
  
  #apply threshold on NDVI
  NDVI_threshold <- 0.3
  
  NDVI_layer[NDVI_layer[] > NDVI_threshold] <- NA
  
  #use the NDVI mask over the whole image
  image_NDVI_mask <- terra::mask(image, NDVI_layer, maskvalues = NA, inverse = F)
  
  #create nCAI layer
  #use the new bands 2044 (ex 2036), 2206 (ex 2199)
  #and 2111 (ex 2103)
  nCAI_layer <- (0.5 * (image$`2.04468` + image$`2.20684`) - image$`2.11104`  )/(0.5 * (image$`2.04468` + image$`2.20684`) + image$`2.11104`  )
  
  #apply threshold on nCAI
  nCAI_threshold <- 0.014
  
  nCAI_layer[nCAI_layer[] > nCAI_threshold] <- NA
  
  #use the nCAI mask over the whole image
  image_nCAI_mask <- terra::mask(image_NDVI_mask, nCAI_layer, maskvalues = NA, inverse = F)
  
  #create NSMI layer
  #use the new bands 1765 (ex 1756) and 2127 (ex 2119)
  NSMI_layer <- (image$`1.76551` - image$`2.12734`)/(image$`1.76551` + image$`2.12734`)
  
  #apply threshold on NSMI
  NSMI_threshold <- 0.14
  
  NSMI_layer[NSMI_layer[] > NSMI_threshold] <- NA
  
  #use the NSMI mask over the whole image
  image_NSMI_mask <- terra::mask(image_nCAI_mask, NSMI_layer, maskvalues = NA, inverse = F,
                                 filename = gsub(".tif","_mask.tif",terra::sources(image)))
  
  #end
  return(image_NSMI_mask)
}

list_of_filtered_images <- lapply(list_of_renamed_images_after_cloud, apply_filters)

vector <- terra::vect("//10.0.1.243/nr_data/1_vector_layer/Italy/Bonifiche Ferraresi/Piani_colturali/Jolanda/2023/piano_colturale_2023.shp")

crop_around <- function(image){
  crop <- terra::crop(image, vector, filename = gsub(".tif","_crop.tif",terra::sources(image)))
  
  return(crop)
}

list_of_cropped_images <- lapply(list_of_filtered_images, crop_around)

convert_to_list <- function(image){
  list <- as.list(image)
  
  return(list)
}

list_of_splitted_layers <- lapply(list_of_cropped_images, convert_to_list)

n <- length(list_of_splitted_layers[[1]]) # assuming all lists in before have the same length
traspose_list <- lapply(1:n, function(i) lapply(list_of_splitted_layers, "[[", i))

median_each_layer <- function(image){
  raster <- terra::rast(image)
  median <- terra::median(raster, na.rm = T)
  name <- terra::names(image[[1]][[1]])
  terra::set.names(median, name)
  return(median)
}

list_of_median_of_bands <- lapply(traspose_list,median_each_layer)

composite <- terra::rast(list_of_median_of_bands)

terra::plot(composite)

terra::writeRaster(composite, "//10.0.1.243/nr_working/emanuele/Progetto_EO4NUTRI/Composite/composite.tif", overwrite = T)

#new request: ENVI and nm for bands
band_names <- as.character(as.numeric(terra::names(composite))*1000)
terra::set.names(composite,band_names)

terra::writeRaster(composite, "//10.0.1.243/nr_working/emanuele/Progetto_EO4NUTRI/Composite/composite.bsq", 
                   filetype = "ENVI", overwrite = T)


















