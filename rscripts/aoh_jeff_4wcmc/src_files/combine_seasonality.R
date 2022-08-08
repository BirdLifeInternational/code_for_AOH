#### process AOH by seasonality according to KBA guidelines ####
#' This script is part of the AOH calculation and merges aoh for different
#' seasonalities
#' If the species if non-migratory, we produce a single AOH for all seasonalities.
#' If the species migrates we produce a breeding (seasons 1, 2, 5) and 
#' non-breeding (seasons 1, 3, 5) AOH map.

if(str_length(check_combo) == 0){
  msg <- paste0("Species ", taxon_id, " does not have any polygon to draw the breeding AOH")
  print(msg)
  not_processed_species[[paste0(taxon_id)]] <- msg
} else if(str_length(check_combo) == 1){
  target_file <- files_aoh[str_detect(files_aoh, paste0("_", check_combo, ".tif$"))]
  #file.rename(target_file, paste0(updated_path, taxon_id, "_", text_file, ".tif"))
  one <- rast(target_file)
  writeRaster(one, paste0(updated_path, taxon_id, "_", text_file, ".tif"), 
                overwrite = TRUE)
} else if(str_length(check_combo) == 2) {
  one <- str_sub(check_combo, 1, 1)
  two <- str_sub(check_combo, 2, 2)
  target_file_1 <- files_aoh[str_detect(files_aoh, paste0("_", one, ".tif$"))]
  target_file_2 <- files_aoh[str_detect(files_aoh, paste0("_", two, ".tif$"))]
  one <- rast(target_file_1)
  two <- rast(target_file_2)
  rlist <- list(one, two)
  rsrc <- sprc(rlist)
  mer <- merge(rsrc, filename = paste0(updated_path, taxon_id, "_", text_file, ".tif"), overwrite = T)
  if(type == "LR"){
    print("the values of the merged file are between:")
    print(unique(values(mer)))
  }
  rm(one, two, rlist, rsrc, target_file_1, target_file_2)
} else if(str_length(check_combo) == 3) {
  one <- str_sub(check_combo, 1, 1)
  two <- str_sub(check_combo, 2, 2)
  three <- str_sub(check_combo, 3, 3)
  target_file_1 <- files_aoh[str_detect(files_aoh, paste0("_", one, ".tif$"))]
  target_file_2 <- files_aoh[str_detect(files_aoh, paste0("_", two, ".tif$"))]
  target_file_3 <- files_aoh[str_detect(files_aoh, paste0("_", three, ".tif$"))]
  one <- rast(target_file_1)
  two <- rast(target_file_2)
  three <- rast(target_file_3)
  rlist <- list(one, two, three)
  rsrc <- sprc(rlist)
  mer <- merge(rsrc, filename = paste0(updated_path, taxon_id, "_", text_file, ".tif"), overwrite = T)
  rm(one, two, three, rlist, rsrc, target_file_1, target_file_2, target_file_3)
} else if(str_length(check_combo) == 4) {
  one <- str_sub(check_combo, 1, 1)
  two <- str_sub(check_combo, 2, 2)
  three <- str_sub(check_combo, 3, 3)
  four <- str_sub(check_combo, 4, 4)
  target_file_1 <- files_aoh[str_detect(files_aoh, paste0("_", one, ".tif$"))]
  target_file_2 <- files_aoh[str_detect(files_aoh, paste0("_", two, ".tif$"))]
  target_file_3 <- files_aoh[str_detect(files_aoh, paste0("_", three, ".tif$"))]
  target_file_4 <- files_aoh[str_detect(files_aoh, paste0("_", four, ".tif$"))]
  one <- rast(target_file_1)
  two <- rast(target_file_2)
  three <- rast(target_file_3)
  four <- rast(target_file_4)
  rlist <- list(one, two, three, four)
  rsrc <- sprc(rlist)
  mer <- merge(rsrc, filename = paste0(updated_path, taxon_id, "_", text_file, ".tif"), overwrite = T)
  rm(one, two, three, four, rlist, rsrc, target_file_1, target_file_2, target_file_3, target_file_4)
} else if(str_length(check_combo) == 5) {
  one <- str_sub(check_combo, 1, 1)
  two <- str_sub(check_combo, 2, 2)
  three <- str_sub(check_combo, 3, 3)
  four <- str_sub(check_combo, 4, 4)
  five <- str_sub(check_combo, 5, 5)
  target_file_1 <- files_aoh[str_detect(files_aoh, paste0("_", one, ".tif$"))]
  target_file_2 <- files_aoh[str_detect(files_aoh, paste0("_", two, ".tif$"))]
  target_file_3 <- files_aoh[str_detect(files_aoh, paste0("_", three, ".tif$"))]
  target_file_4 <- files_aoh[str_detect(files_aoh, paste0("_", four, ".tif$"))]
  target_file_5 <- files_aoh[str_detect(files_aoh, paste0("_", five, ".tif$"))]
  one <- rast(target_file_1)
  two <- rast(target_file_2)
  three <- rast(target_file_3)
  four <- rast(target_file_4)
  five <- rast(target_file_5)
  rlist <- list(one, two, three, four, five)
  rsrc <- sprc(rlist)
  mer <- merge(rsrc, filename = paste0(updated_path, taxon_id, "_", text_file, ".tif"), overwrite = T)
  rm(one, two, three, four, five, rlist, rsrc, 
     target_file_1, target_file_2, target_file_3, target_file_4, target_file_5)
}
print(paste0("Written raster file ", paste0(updated_path, taxon_id, "_", text_file, ".tif")))

source("rscripts/aoh_jeff_4wcmc/src_files/calculate_area.R")