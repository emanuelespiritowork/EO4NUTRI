#multitemporal composite

list_of_dates <- c(
  #"20220401",
  #"20220430",
  #"20220512",
  #"20220627",
  #"20220703",
  #"20220801",
  #"20221201",
  "20240407",
  "20240512",
  #"20240627",
  "20240726",
  "20240830",
  #"20240928",
  "20241113",
  "20230304",
  "20230407",
  "20230524",
  #"20230703",
  "20230726",
  "20230807",
  "20230905",
  "20230911",
  "20231004",
  "20231125"
)

create_image_path <- function(date){
  year <- substr(date,1,4)
  path <- paste0("//10.0.1.243/nr_data/3_rs_data/PRISMA/JDS/",year,"/L1/","PRS_L1_STD_OFFL_",date,"/Atcor_regrid/PRISMA_resample.tif")
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
  new_band_names <- substr(get_band_names, 24, 30)
  terra::set.names(image,new_band_names)
  return(image)
}

list_of_renamed_images <- lapply(list_of_images,rename_bands)

save_images_function <- function(image,percorso = "//10.0.1.243/nr_working/emanuele/Progetto_EO4NUTRI/Images_for_composite/"){
  date <- stringr::str_match(terra::sources(image), "PRS_L1_STD_OFFL_\\s*(.*?)\\s*/Atcor_regrid")[,2]
  terra::writeRaster(image,paste0(percorso,date,".tif"),overwrite=T)
  return(date)
}

if(F){
  lapply(list_of_images,save_images_function)
}

apply_filters <- function(image){
  #create NDVI layer
  NDVI_layer <- 
  
  #apply threshold on NDVI
  NDVI_threshold <- 0.3
  
  #use the NDVI mask over the whole image
  
  #create nCAI layer
  
  #apply threshold on nCAI
  nCAI_threshold <- 0.014
  
  #use the nCAI mask over the whole image
  
  #create NSMI layer
  
  #apply threshold on NSMI
  NSMI_threshold <- 0.14
  
  #use the NSMI mask over the whole image
  
  
}