####### Script to run the checks and the actual aoh ######
aoh_computation <- function(X = i, ordered_gpkg = ordered_gpkg, path_lookup = path_lookup,
         spp_summary_birds_mammals = spp_summary_birds_mammals,
         spp_habitat_birds_mammals = spp_habitat_birds_mammals,
         updated_path = updated_path,
         model_validation = model_validation,
         cache_dir = cache_dir,
         elevation_data = elevation_data,
         output_dir = output_dir, 
         habitat_data = habitat_data,
         crosswalk_data = crosswalk_data,
         n_threads = n_threads,
         
         cache_limit = cache_limit,
         verbose_value = verbose_value,
         eng = eng,
         taxon_id = -10){
  i <- X
  skip_to_next <- FALSE
  
  #print("================================================================")
  #print("================================================================")
  if(taxon_id != -10){
    file_to_process <- ""
  } else {
    file_to_process <- ordered_gpkg[i]
    taxon_id <- str_replace(file_to_process, paste0(path_lookup, "id_no_"), "")
    taxon_id <- str_replace(taxon_id, paste0(path_lookup, "/id_no_"), "")
    taxon_id <- as.numeric(str_replace(taxon_id, ".gpkg", ""))
  }
  if(!any(spp_summary_birds_mammals$id_no == taxon_id)){
    msg <- paste0("Skipping taxon id ", taxon_id, " because excluded a priori from the calculations")
    fwrite(data.table(msg = msg), file = paste0(output_errors, "msg_", i, ".csv"))
    skip_to_next <<- TRUE
    if(skip_to_next) { return(taxon_id) }
  }
  if(!any(spp_habitat_birds_mammals$id_no == taxon_id)){
    msg <- paste0("Skipping taxon id ", taxon_id, " because no habitat preferences are available.")
    fwrite(data.table(msg = msg), file = paste0(output_errors, "msg_", i, ".csv"))
    skip_to_next <<- TRUE
    if(skip_to_next) { return(taxon_id) }
  }
  
  file_bred <- paste0(updated_path, taxon_id,  "_Breeding.tif")
  file_res <- paste0(updated_path, taxon_id, "_Resident.tif")
  file_nonb <- paste0(updated_path, taxon_id, "_Nonbreeding.tif")
  file_both <- paste0(updated_path, taxon_id, "_Breeding_and_Nonbreeding.tif")
  
  
  if(!model_validation && type == "LR" && 
     (file.exists(file_bred) & file.exists(file_nonb) & file.exists(file_both))
     ){
    msg <- paste0("Skipping taxon id ", taxon_id, " with index ", i, "; species already processed.")
    fwrite(data.table(msg = msg), file = paste0(output_errors, "msg_", i, ".csv"))
    return(taxon_id);
  } else if(!model_validation && type == "FRC" && (file.exists(file_bred) & file.exists(file_nonb))){
    msg <- paste0("Skipping taxon id ", taxon_id, " with index ", i, "; species already processed.")
    fwrite(data.table(msg = msg), file = paste0(output_errors, "msg_", i, ".csv"))
    return(taxon_id);
  } else if(model_validation && (file.exists(file_both))){
    msg <- paste0("Skipping taxon id ", taxon_id, " with index ", i, "; species already processed.")
    fwrite(data.table(msg = msg), file = paste0(output_errors, "msg_", i, ".csv"))
    return(taxon_id);
  } # else {
  #   stop("check combinations of type and validation to assert if species were already processed")
  # }
  
  if(file.exists(file_res)){
    msg <- paste0("Skipping taxon id ", taxon_id, " with index ", i, "; species already processed.")
    fwrite(data.table(msg = msg), file = paste0(output_errors, "msg_", i, ".csv"))
    return(taxon_id);
  }
  
  msg <- paste0("Processing taxon id ", taxon_id, " with index ", i)

  spp_summary_data <- subset(spp_summary_birds_mammals, id_no == taxon_id)
  spp_habitat_data <- subset(spp_habitat_birds_mammals, id_no == taxon_id)
  #### fix seasonality, by allowing habitat coded to match multiple seasonalities
  
  spp_habitat_data <- correct_season_mismatch(spp_habitat_data)
  
  if(nrow(spp_summary_data) == 0 | nrow(spp_habitat_data) == 0){
    msg <- paste0("Cannot process taxon id ", taxon_id, " | ", 
                  " The id_no is missing from the data: 
                      either nrow(spp_summary_data) == ",
                  nrow(spp_summary_data), "; or nrow(spp_habitat_data) == ", nrow(spp_habitat_data))
    fwrite(data.table(msg = msg), file = paste0(output_errors, "msg_", i, ".csv"))
    if(is.null(not_processed_species[[paste0(taxon_id)]])){
      not_processed_species[[paste0(taxon_id)]] <- msg
    } else {
      tmp <- not_processed_species[[paste0(taxon_id)]]
      not_processed_species[[paste0(taxon_id)]] <- list(tmp, msg)
    }
    return(taxon_id)
  }
  
  #############################################################   
  
  if(any(spp_summary_data$marine == "true")){
    msg <- paste0("Cannot process taxon id ", taxon_id, " | ", 
                  unique(spp_summary_data$binomial), " with index ", i, 
                  " because the species is marine")
    fwrite(data.table(msg = msg), file = paste0(output_errors, "msg_", i, ".csv"))
    if(is.null(not_processed_species[[paste0(taxon_id)]])){
      not_processed_species[[paste0(taxon_id)]] <- msg
    } else {
      tmp <- not_processed_species[[paste0(taxon_id)]]
      not_processed_species[[paste0(taxon_id)]] <- list(tmp, msg)
    }
    skip_to_next <<- TRUE;
    if(skip_to_next) {return(taxon_id)}
  }
  spp <- paste0(path_lookup, "id_no_", taxon_id, ".gpkg")
  if(!file.exists(spp)) {
    msg <- paste0("Range map not available for ", taxon_id, "; the file ", 
                  paste0(path_lookup, "id_no_", taxon_id, ".gpkg"), 
                  " does not exist")
    fwrite(data.table(msg = msg), file = paste0(output_errors, "msg_", i, ".csv"))
    if(is.null(not_processed_species[[paste0(taxon_id)]])){
      not_processed_species[[paste0(taxon_id)]] <- msg
    } else {
      tmp <- not_processed_species[[paste0(taxon_id)]]
      not_processed_species[[paste0(taxon_id)]] <- list(tmp, msg)
    }
    skip_to_next <<- TRUE;
  }
  if(skip_to_next) {return(taxon_id)}
  # import polygons data
  spp_range_data <- read_sf(spp)
  st_geometry(spp_range_data) <- "geometry"
  
  if(any(names(spp_range_data) == "terrestial")) names(spp_range_data)[names(spp_range_data) == "terrestial"] <- "terrestrial"
  
  # spp_range_data$terrestrial <- rep("true", nrow(spp_range_data))
  # spp_range_data$marine <- rep("false", nrow(spp_range_data))
  # spp_range_data$freshwater <- rep("false", nrow(spp_range_data))
  spp_range_data <- subset(spp_range_data, presence == 1 | presence == 2)
  spp_range_data <- subset(spp_range_data, origin == 1 | origin == 2 | origin == 6)

  
  if((!"binomial" %in% colnames(spp_range_data)) & ("sci_name"  %in% colnames(spp_range_data))){
    spp_range_data$binomial <- spp_range_data$sci_name
  } else if (("binomial" %in% colnames(spp_summary_data)) & (!"sci_name"  %in% colnames(spp_range_data))){
    spp_range_data$binomial <- spp_range_data$sci_name <- unique(spp_summary_data$binomial)
  } else if ((!"binomial" %in% colnames(spp_range_data)) & (!"sci_name"  %in% colnames(spp_range_data))){
    msg <- paste0("Cannot process taxon id ", taxon_id, " | no scientific name available for taxon id ", taxon_id)
    fwrite(data.table(msg = msg), file = paste0(output_errors, "msg_", i, ".csv"))
    if(is.null(not_processed_species[[paste0(taxon_id)]])){
      not_processed_species[[paste0(taxon_id)]] <- msg
    } else {
      tmp <- not_processed_species[[paste0(taxon_id)]]
      not_processed_species[[paste0(taxon_id)]] <- list(tmp, msg)
    }
    skip_to_next <<- TRUE;
  }
  if(length(unique(spp_habitat_data$code)) == 1 && ((unique(spp_habitat_data$code) == "17") | (unique(spp_habitat_data$code) == "18"))){
    msg <- paste0("Warning for taxon id ", taxon_id, " | there is only a recorded habitat type ", unique(spp_habitat_data$code),
                  ". Note that 17 corresponds to other and 18 to unknown habitat.")
    fwrite(data.table(msg = msg), file = paste0(output_errors, "msg_", i, ".csv"))
    if(is.null(not_processed_species[[paste0(taxon_id)]])){
      not_processed_species[[paste0(taxon_id)]] <- msg
    } else {
      tmp <- not_processed_species[[paste0(taxon_id)]]
      not_processed_species[[paste0(taxon_id)]] <- list(tmp, msg)
    }
  }
  if(is.na(unique(spp_habitat_data$migrant_in_iucn))){
    msg <- paste0("Cannot process taxon id ", taxon_id, " | we do not have information 
                    to tell if it's migratory")
    fwrite(data.table(msg = msg), file = paste0(output_errors, "msg_", i, ".csv"))
    if(is.null(not_processed_species[[paste0(taxon_id)]])){
      not_processed_species[[paste0(taxon_id)]] <- msg
    } else {
      tmp <- not_processed_species[[paste0(taxon_id)]]
      not_processed_species[[paste0(taxon_id)]] <- list(tmp, msg)
    }
    skip_to_next <<- TRUE;
    
  } else {
    if(unique(spp_habitat_data$migrant_in_iucn) == "not_migrant" | unique(spp_habitat_data$migrant_in_iucn) == "not-migrant"){
      migratory_status <- FALSE
    } else if (unique(spp_habitat_data$migrant_in_iucn) == "migrant"){
      migratory_status <- TRUE
    } 
  }
  if(skip_to_next) {return(taxon_id)}
  if(!(any(names(spp_range_data) == "sci_name")) | !(any(names(spp_range_data) == "id_no"))) {
    msg <- paste0("Warning for taxon id ", taxon_id, " sci_name and/or id_no not available from range map")
    fwrite(data.table(msg = msg), file = paste0(output_errors, "msg_", i, ".csv"))
    return(0)
  }

  # combine seasonalities according to KBA guidelines
  if(migratory_status){
    bred <- subset(spp_range_data, seasonal == 1 | seasonal == 2 | seasonal == 5)
    bred$seasonal <- 2
    nonbred <- subset(spp_range_data, seasonal == 1 | seasonal == 3 | seasonal == 5)
    nonbred$seasonal <- 3
    bred <- bred %>% 
      group_by(seasonal) %>%
      summarise(geometry = sf::st_union(geometry), 
                sci_name = unique(spp_range_data$sci_name),
                id_no = unique(spp_range_data$id_no),
                seasonal = 2,
                presence = 1, origin = 1, 
                terrestrial = "true", marine = "false", freshwater = "false"
      ) %>% ungroup()
    
    nonbred <- nonbred %>% 
      group_by(seasonal) %>%
      summarise(geometry = sf::st_union(geometry), 
                sci_name = unique(spp_range_data$sci_name),
                id_no = unique(spp_range_data$id_no),
                seasonal = 3,
                presence = 1, origin = 1, 
                terrestrial = "true", marine = "false", freshwater = "false") %>%  ungroup()
    
    spp_range_data <- rbind(bred, nonbred)
    rm(bred, nonbred)
    
  } else if(!migratory_status){
    resi <- spp_range_data
    resi$seasonal <- 1
    
    resi <- resi %>% 
      group_by(seasonal) %>%
      summarise(geometry = sf::st_union(geometry), 
                sci_name = unique(spp_range_data$sci_name),
                id_no = unique(spp_range_data$id_no),
                seasonal = 1,
                presence = 1, origin = 1, 
                terrestrial = "true", marine = "false", freshwater = "false"
      ) %>% ungroup()
    spp_range_data <- resi
    rm(resi)
  }
  #print("summary merged spp_range_data")
  #print(unique(st_drop_geometry(spp_range_data)[, c("id_no", "seasonal", "presence", "origin")]))
  
  # run the aoh code
  #### create data to read within aoh calculation ####
  if(unique(st_geometry_type(spp_range_data)) == "MULTISURFACE"){
    ## S3 method for class 'POLYGON'
    msg <- paste0("Range map with potential error for ", taxon_id, "; the ", 
                  paste0(path_lookup, "id_no_", taxon_id, ".gpkg"), 
                  " is a multisurface object")
    fwrite(data.table(msg = msg), file = paste0(output_errors, "msg_", i, ".csv"))
    if(is.null(not_processed_species[[paste0(taxon_id)]])){
      not_processed_species[[paste0(taxon_id)]] <- msg
    } else {
      tmp <- not_processed_species[[paste0(taxon_id)]]
      not_processed_species[[paste0(taxon_id)]] <- list(tmp, msg)
    }
    skip_to_next <<- TRUE;
    if(skip_to_next) {next}
  }

  # process migratory and not-migratory species differently
  if(!migratory_status){
    spp_info_data <- create_spp_info_data(spp_range_data, 
                                          spp_summary_data = spp_summary_data,
                                          spp_habitat_data = spp_habitat_data,
                                          cache_dir = cache_dir,
                                          keep_iucn_rl_presence = c(1, 2),
                                          keep_iucn_rl_origin = c(1, 2, 6),
                                          keep_iucn_rl_seasonal = c(1, 2, 3, 4, 5),
                                          verbose = verbose_value)
    
  } else {
    spp_info_data <- create_spp_info_data(spp_range_data, 
                                          spp_summary_data = spp_summary_data,
                                          spp_habitat_data = spp_habitat_data,
                                          cache_dir = cache_dir,
                                          keep_iucn_rl_presence = c(1, 2),
                                          keep_iucn_rl_origin = c(1, 2, 6),
                                          keep_iucn_rl_seasonal = c(1, 2, 3, 5),
                                          verbose = verbose_value)
  }
  
  ####### When doing model validation set parameters #####    
  if(model_validation == TRUE && habitat_only == TRUE){
    spp_info_data$full_habitat_code <- hb_codes
  }
  # Pick correct function if we're doing fractional aoh, or low resolution aoh
  if(str_detect(type, "FRC")){
    spp_aoh <- create_spp_frc_data(x = spp_info_data,
                                   res = 1000,
                                   elevation_data = elevation_data,
                                   output_dir = output_dir, 
                                   habitat_data = habitat_data,
                                   crosswalk_data = crosswalk_data,
                                   n_threads = n_threads,
                                   cache_dir = cache_dir, 
                                   cache_limit = cache_limit,
                                   verbose = verbose_value,
                                   engine = eng)
  } else if (str_detect(type, "LR")){
    spp_aoh <- create_spp_aoh_data(x = spp_info_data,
                                   elevation_data = elevation_data,
                                   output_dir = output_dir, 
                                   habitat_data = habitat_data,
                                   crosswalk_data = crosswalk_data,
                                   n_threads = n_threads,
                                   cache_dir = cache_dir, 
                                   cache_limit = cache_limit,
                                   verbose = verbose_value,
                                   engine = eng)
  } else {
    stop("No type specified")
  }
  st_write(spp_aoh, paste0(output_shp, taxon_id, ".gpkg"),
           append=FALSE,
           driver = driver,
           quiet = TRUE,
           layer_options = "OVERWRITE=true")
  save_res_aoh <- st_drop_geometry(spp_aoh)

  check_codes <- str_replace_all(save_res_aoh$full_habitat_code,  
                                 paste0(save_res_aoh$habitat_code), "") 
  check_codes <- str_replace_all(check_codes, "\\|", "")
  check_codes <- unique(check_codes)
  if(model_validation != TRUE){
    if(any(is.na(str_length(check_codes))) | str_length(check_codes) > 0){
      msg <- paste0("Warning for taxon id ", taxon_id, " | ! argument to crosswalk_data is missing 
                        the following habitat classification codes: ", check_codes,
                    ". Note that spp_habitat_data$code = ", paste0(spp_habitat_data$code, collapse = ","),
                    "; save_res_aoh$full_habitat_code = ", paste0(save_res_aoh$full_habitat_code, collapse = ","),
                    "; save_res_aoh$habitat_code = ", paste0(save_res_aoh$habitat_code, collapse = ","),
                    ". Is there an NA in the habitat code within the spp_aoh? ", any(is.na(str_length(check_codes))))
      fwrite(data.table(msg = msg), file = paste0(output_errors, "msg_", i, ".csv"))
      if(is.null(not_processed_species[[paste0(taxon_id)]])){
        not_processed_species[[paste0(taxon_id)]] <- msg
      } else {
        tmp <- not_processed_species[[paste0(taxon_id)]]
        not_processed_species[[paste0(taxon_id)]] <- list(tmp, msg)
      }
    }
  }  
  write_csv(save_res_aoh, file = paste0(output_df, "id_no_", taxon_id, ".csv"))
  # combine and rename files according to KBA guidelines
  if(str_detect(paste0(output_dir), "_FRC")){
    files_aoh <- sort(list.files(path = paste0(output_dir), pattern = paste0("_", taxon_id, "_"), full.names = T))
    #updated_path <- paste0(output_dir, "FRC_")
  } else {
    lf <- list.files(path = paste0(output_dir), full.names = T)
    keep <- str_detect(lf,
                       paste0("\\/", taxon_id, "_"))
    
    files_aoh <- sort(lf[keep])
    files_aoh <- str_replace(files_aoh, "\\/\\/", "\\/")
    #updated_path <- paste0(output_dir)
    rm(lf, keep)
  }

  string_1 <- str_sub(files_aoh[1], - 5, -5)
  string_2 <- str_sub(files_aoh[2], - 5, -5)
  string_3 <- str_sub(files_aoh[3], - 5, -5)
  string_4 <- str_sub(files_aoh[4], - 5, -5)
  string_5 <- str_sub(files_aoh[5], - 5, -5)
  combo <- paste0(string_1, string_2, string_3, string_4, string_5)
  combo <- str_replace_all(combo, "NA", "")
  combo <- str_replace_all(combo, "t", "")
  combo <- str_replace_all(combo, "g", "")
  combo <- subset(seas, combinations == combo)
  if(nrow(combo) == 0){
    msg <- paste("Error in reading the tif files when combining by season! The seasonalities are: ", combo)
    fwrite(data.table(msg = msg), file = paste0(output_errors, "combo_msg_", i, ".csv"))
    return(taxon_id)
  }
  if(!migratory_status){
    check_combo <- combo$resident
    text_file <- "Resident"
    combine_seasonality(check_combo = check_combo, 
                        taxon_id = taxon_id, 
                        updated_path = updated_path, 
                        text_file = text_file, 
                        files_aoh = files_aoh, 
                        type = type,
                        output_errors = output_errors, 
                        spp_summary_data = spp_summary_data,
                        output_area = output_area)
	    print("resident done")
    
  }
  if(migratory_status && !model_validation){
    check_combo <- combo$breeding
    text_file <- "Breeding"
    combine_seasonality(check_combo = check_combo, 
                        taxon_id = taxon_id, 
                        updated_path = updated_path, 
                        text_file = text_file, 
                        files_aoh = files_aoh, 
                        type = type,
                        output_errors = output_errors, 
                        spp_summary_data = spp_summary_data,
                        output_area = output_area)

    print("breeding done")
    check_combo <- combo$non_breeding
    text_file <- "Nonbreeding"
    combine_seasonality(check_combo = check_combo, 
                        taxon_id = taxon_id, 
                        updated_path = updated_path, 
                        text_file = text_file, 
                        files_aoh = files_aoh, 
                        type = type,
                        output_errors = output_errors, 
                        spp_summary_data = spp_summary_data,
                        output_area = output_area)
	    print("non breeding done")
    if(str_detect(type, "LR")){
      check_combo <- combo$validation_migrant 
      text_file <- "Breeding_and_Nonbreeding"
      combine_seasonality(check_combo = check_combo, 
                          taxon_id = taxon_id, 
                          updated_path = updated_path, 
                          text_file = text_file, 
                          files_aoh = files_aoh, 
                          type = type,
                          output_errors = output_errors, 
                          spp_summary_data = spp_summary_data,
                          output_area = output_area)
                              print("breeding +nonbreeding done")
    }
    
  } else if(migratory_status && model_validation){
    check_combo <- combo$validation_migrant 
    text_file <- "Breeding_and_Nonbreeding"
    combine_seasonality(check_combo = check_combo, 
                        taxon_id = taxon_id, 
                        updated_path = updated_path, 
                        text_file = text_file, 
                        files_aoh = files_aoh, 
                        type = type,
                        output_errors = output_errors, 
                        spp_summary_data = spp_summary_data,
                        output_area = output_area)
  } 
  # clean up
  #if(file.exists(files_aoh[1]))  unlink(files_aoh, recursive = T)
  #if(file.exists(files_aoh[1])){
  #  files_aoh <- str_replace_all(files_aoh, "\\/\\/", "\\/")
   # unlink(files_aoh, recursive = T)
 # }
  rm(files_aoh, save_res_aoh)
  rm(spp_aoh, spp, spp_range_data)
  rm(check_combo, combo, spp_info_data, string_1, string_2, string_3, string_4, string_5)    
  rm(file_bred, file_nonb, file_res, file_to_process)
  save(not_processed_species, file = paste0(output_errors, "not_processed_species", tmp_name, ".RData"))
  if(exists("msg")) rm(msg)
  return(taxon_id)
}
