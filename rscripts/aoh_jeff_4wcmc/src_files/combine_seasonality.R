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
                                output_area = output_area){

  if(str_length(check_combo) == 0){
    msg <- paste0("Species ", taxon_id, " does not have any polygon to draw the breeding AOH")
    fwrite(data.table(msg = msg), file = paste0(output_errors, "msg_", i, ".csv"))
    not_processed_species[[paste0(taxon_id)]] <- msg
  } else if(str_length(check_combo) == 1){
    if(text_file != "Breeding_and_Nonbreeding"){
    target_file <- files_aoh[str_detect(files_aoh, paste0("_", check_combo, ".tif$"))]
    file.rename(target_file, paste0(updated_path, taxon_id, "_", text_file, ".tif"))
    } else if(text_file == "Breeding_and_Nonbreeding"){
    	msg <- paste0("Taxon_id ", taxon_id, 
    	" is migratory; but the only seasonality in check_combo is: ", check_combo, ". Why?")
    	print(msg)
    	fwrite(data.table(msg = msg), file = paste0(output_errors, "msg_", i, "_within_combine_seasonality.csv"))
    }
    # one <- terra::rast(target_file)
    # terra::writeRaster(one, paste0(updated_path, taxon_id, "_", text_file, ".tif"), 
    #               overwrite = TRUE)
  } else if(str_length(check_combo) == 2) {
    if(text_file == "Breeding_and_Nonbreeding"){
      spp_aoh$seasonal <- 23
      both_bred_nonb <- spp_aoh %>%  group_by(seasonal) %>%
        summarise(geometry = sf::st_union(geometry), 
                  sci_name = unique(spp_range_data$sci_name),
                  id_no = unique(spp_range_data$id_no),
                  seasonal = 23
        ) %>% ungroup()
      
      both_bred_nonb <- stars::st_rasterize(both_bred_nonb %>% dplyr::select(id_no, geometry))
      
      # export as tiff
      stars::write_stars(both_bred_nonb, 
                         dsn = paste0(updated_path, taxon_id, "_", text_file, ".tif"),
                         normalize_path = TRUE)
      
    } else {
      stop("two seasonal polygons from spp_aoh but the type is not breeding+nonbreeding")
    }

  } else if(str_length(check_combo) >= 3) {
    stop("3+ seasonal polygons")
  }
  prt_msg <- paste0("Written_raster_file_", paste0(updated_path, taxon_id, "_", text_file))
  prt_msg <- str_replace_all(prt_msg, "\\/", "_")
  #print(prt_msg)
  fwrite(data.table(taxon_id = taxon_id), file = paste0(output_errors, prt_msg, ".csv"))
  calculate_area(type = type, wd_path = wd_path, updated_path = updated_path,
                 output_area = output_area,
                 taxon_id = taxon_id, text_file = text_file,
                 spp_summary_data = spp_summary_data, check_combo = check_combo)
  return(0)
}
