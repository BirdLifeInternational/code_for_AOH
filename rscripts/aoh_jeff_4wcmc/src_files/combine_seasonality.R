#### process AOH by seasonality according to KBA guidelines ####
#' This script is part of the AOH calculation and merges aoh for different
#' seasonalities
#' If the species if non-migratory, we produce a single AOH for all seasonalities.
#' If the species migrates we produce a breeding (seasons 1, 2, 5) and 
#' non-breeding (seasons 1, 3, 5) AOH map.
#'   calculate_area(type = type, wd_path = wd_path, updated_path = updated_path, 
combine_seasonality <- function(check_combo = check_combo, 
                                taxon_id = taxon_id, 
                                updated_path = updated_path, 
                                text_file = text_file, 
                                files_aoh = files_aoh, 
                                type = type,
                                output_errors = output_errors, 
                                spp_summary_data = spp_summary_data,
                                output_area = output_area,
                                output_df = output_df,
                                spp_aoh = spp_aoh){
  
  prt_msg <- paste0("Processed ", paste0(updated_path, taxon_id, "_", text_file))
  prt_msg <- str_replace_all(prt_msg, "\\/", "_")

  if(str_length(check_combo) == 0){
    msg <- paste0("Species ", taxon_id, " does not have any polygon to draw the breeding AOH")
    fwrite(data.table(msg = msg, taxon_id = taxon_id), file = paste0(output_errors, "msg_", i, ".csv"))
    not_processed_species[[paste0(taxon_id)]] <- msg
  } else if(str_length(check_combo) == 1){
    if(text_file != "Breeding_and_Nonbreeding"){
    target_file <- files_aoh[str_detect(files_aoh, paste0("_", check_combo, ".tif$"))]
    # make a copy of original file in different directory
    R.utils::copyFile(target_file, output_aoh_tif, skip = FALSE, overwrite=TRUE, 
    	validate=TRUE,  verbose=FALSE)
    target_file <- str_replace_all(target_file, "\\/\\/", "\\/")
    file_nam <- paste0(updated_path, taxon_id, "_", text_file, ".tif")
    file_nam <- str_replace_all(file_nam, "\\/\\/", "\\/")
    if(file.exists(file_nam)) unlink(file_nam, recursive = T)
    #file.rename(target_file, file_nam)
    R.utils::renameFile(target_file, file_nam, overwrite=TRUE, verbose=TRUE)
    } else if(text_file == "Breeding_and_Nonbreeding"){
    	msg <- paste0("Taxon_id ", taxon_id, 
    	" is migratory; but the only seasonality in check_combo is: ", check_combo)
    	print(msg)
    	fwrite(data.table(msg = msg), file = paste0(output_errors, "msg_", i, "_within_combine_seasonality.csv"))
    	
    	files_in_copy <- list.files(normalizePath(output_aoh_tif), pattern = paste0(taxon_id, "_"), full.names =TRUE)
    	target_file <- files_in_copy[1]
    	# make a copy of original file in different directory
    	target_file <- str_replace_all(target_file, "\\/\\/", "\\/")
    	file_nam <- paste0(normalizePath(updated_path), "/", taxon_id, "_", text_file, ".tif")
    	file_nam <- str_replace_all(file_nam, "\\/\\/", "\\/")
    	if(file.exists(file_nam)) unlink(file_nam, recursive = T)
    	R.utils::renameFile(target_file, file_nam, overwrite=TRUE, verbose=TRUE)
    }
    print(prt_msg)
    # one <- terra::rast(target_file)
    # terra::writeRaster(one, paste0(updated_path, taxon_id, "_", text_file, ".tif"), 
    #               overwrite = TRUE)
  } else if(str_length(check_combo) == 2) {
    if(text_file == "Breeding_and_Nonbreeding"){
     
      file_nam <- paste0(updated_path, taxon_id, "_Breeding.tif")
      file_nam <- str_replace_all(file_nam, "\\/\\/", "\\/")
      if(!file.exists(file_nam)) print("breeding tif missing")
      x <- file_nam

      file_nam <- paste0(updated_path, taxon_id, "_Nonbreeding.tif")
      file_nam <- str_replace_all(file_nam, "\\/\\/", "\\/")
      if(!file.exists(file_nam)) print("non_breeding tif missing")
      y <- file_nam
     
      file_nam <- paste0(updated_path, taxon_id, "_", text_file, "_tocompress.tif")
      #print(paste0("file_nam is ", file_nam))
      if(file.exists(file_nam)) unlink(file_nam, recursive = T)
      if(file.exists(file_nam)) unlink(file_nam)

      gdalUtilities::gdalwarp(dstfile = file_nam, srcfile = c(x, y),
                              t_srs = "ESRI:54017", 
                              r = "max", of = "GTiff",
                              ot = "Byte",
                              dryrun = F)
 
      compressed_file_nam <- paste0(updated_path, taxon_id, "_", text_file, ".tif")
      print(paste0("file_nam is ", compressed_file_nam))
      if(file.exists(compressed_file_nam)) unlink(compressed_file_nam, recursive = T)
      
      # writeRaster(rast(file_nam), 
      #             paste0(updated_path, taxon_id, "_", text_file, "_test.tif"),  
      #             gdal=c("COMPRESS=LZW"))
      gdalUtilities::gdal_translate(src_dataset = file_nam,
                                    dst_dataset = compressed_file_nam,
                                    co = "COMPRESS=LZW", #ot = "Byte",
                                    dryrun = F)
      
      if(file.exists(file_nam)) unlink(file_nam, recursive = T)
      if(file.exists(file_nam)) unlink(file_nam)

      print(paste0("combined seasonalities for ", taxon_id))
      
    } else {
      print("two seasonal polygons from spp_aoh but the type is not breeding+nonbreeding")
    }

  } else if(str_length(check_combo) >= 3) {
    print("3+ seasonal polygons")
  }
  
  
  if(stringr::str_detect(type, "LR")){
    area_calc <- frequency_gdalinfo_raster(file_nam)
  
    freq_0 <- area_calc[code_values == 0, "frequency"]
    freq_1 <- area_calc[code_values == 1, "frequency"]
    short_nam = str_remove(file_nam, updated_path)
    area_dt <- data.table(raster_name = short_nam, unsuitable = freq_0, suitable = freq_1, taxon_id = taxon_id,
               seasonality = text_file, type = "LR")
    fwrite(area_dt, file = paste0(output_area, taxon_id, "_", text_file, ".csv"))
    
    print("area calc successful")
  }
  # check habitat codes within the unsuitable pixels for those aoh clipped by habitat only 
  # and with low model prevalence
  if(
    (model_validation && habitat_only && ((freq_1/(freq_0 + freq_1)) < 0.25) ) |
     (!model_validation && str_detect(type, "LR") &&
      unique(spp_summary_data$elevation_from_iucn) == "no" &&
      ((freq_1/(freq_0 + freq_1)) < 0.25))
     ){
    summarize_habitat_code_unsuitable_pixels(
             updated_path = normalizePath(updated_path),
             taxon_id = taxon_id,
             crosswalk = crosswalk_data,
             path_habitat_rast = normalizePath(path_habitat_tif),
             output_df = normalizePath(output_df),
             model_validation = model_validation,
             habitat_only = habitat_only,
             file_nam = file_nam,
             text_file = text_file # seasonality combination
             )
    print("checking unsuitable habitat codes successful")
  }
  return(0)
}








