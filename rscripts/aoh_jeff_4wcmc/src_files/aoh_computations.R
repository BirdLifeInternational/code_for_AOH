####### Script to run the checks and the actual aoh ######

skip_to_next <- FALSE

print("================================================================")
print("================================================================")
file_to_process <- ordered_gpkg[i]
taxon_id <- str_replace(file_to_process, paste0(path_lookup, "id_no_"), "")
taxon_id <- str_replace(taxon_id, paste0(path_lookup, "/id_no_"), "")
taxon_id <- as.numeric(str_replace(taxon_id, ".gpkg", ""))
if(!any(spp_summary_birds_mammals$id_no == taxon_id)){
  msg <- paste0("Skipping taxon id ", taxon_id, " because excluded a priori from the calculations")
  print(msg)
  skip_to_next <<- TRUE
  if(skip_to_next) { stop(print(msg)) }
}
if(!any(spp_habitat_birds_mammals$id_no == taxon_id)){
  msg <- paste0("Skipping taxon id ", taxon_id, " because no habitat preferences are available.")
  print(msg)
  skip_to_next <<- TRUE
  if(skip_to_next) { stop(print(msg)) }
}

file_bred <- paste0(updated_path, taxon_id,  "_Breeding.tif")
file_res <- paste0(updated_path, taxon_id, "_Resident.tif")
file_nonb <- paste0(updated_path, taxon_id, "_Nonbreeding.tif")
file_both <- paste0(updated_path, taxon_id, "_Breeding_and_Nonbreeding.tif")


if(!model_validation && (file.exists(file_bred) & file.exists(file_nonb))){
  msg <- paste0("Skipping taxon id ", taxon_id, " with index ", i, "; species already processed.")
  print(msg)
  skip_to_next <<- TRUE;
} else if(model_validation && (file.exists(file_both))){
  msg <- paste0("Skipping taxon id ", taxon_id, " with index ", i, "; species already processed.")
  print(msg)
  skip_to_next <<- TRUE;
}

if(file.exists(file_res)){
  msg <- paste0("Skipping taxon id ", taxon_id, " with index ", i, "; species already processed.")
  print(msg)
  skip_to_next <<- TRUE;
}
if(skip_to_next) { stop(print(msg)) }
msg <- paste0("Processing taxon id ", taxon_id, " with index ", i)
print(msg)
spp_summary_data <- subset(spp_summary_birds_mammals, id_no == taxon_id)
spp_habitat_data <- subset(spp_habitat_birds_mammals, id_no == taxon_id)
#### fix seasonality, by allowing habitat coded to match multiple seasonalities
source("rscripts/aoh_jeff_4wcmc/src_files/correct_season_mismatch.R")

if(nrow(spp_summary_data) == 0 | nrow(spp_habitat_data) == 0){
  msg <- paste0("Cannot process taxon id ", taxon_id, " | ", 
                " The id_no is missing from the data: 
                    either nrow(spp_summary_data) == ",
                nrow(spp_summary_data), "; or nrow(spp_habitat_data) == ", nrow(spp_habitat_data))
  print(msg)
  if(is.null(not_processed_species[[paste0(taxon_id)]])){
    not_processed_species[[paste0(taxon_id)]] <- msg
  } else {
    tmp <- not_processed_species[[paste0(taxon_id)]]
    not_processed_species[[paste0(taxon_id)]] <- list(tmp, msg)
  }
  stop(print(msg))
}

#############################################################   

if(any(spp_summary_data$marine == "true")){
  msg <- paste0("Cannot process taxon id ", taxon_id, " | ", 
                unique(spp_summary_data$binomial), " with index ", i, 
                " because the species is marine")
  print(msg)
  if(is.null(not_processed_species[[paste0(taxon_id)]])){
    not_processed_species[[paste0(taxon_id)]] <- msg
  } else {
    tmp <- not_processed_species[[paste0(taxon_id)]]
    not_processed_species[[paste0(taxon_id)]] <- list(tmp, msg)
  }
  skip_to_next <<- TRUE;
  if(skip_to_next) {stop(print(msg))}
}
spp <- paste0(path_lookup, "id_no_", taxon_id, ".gpkg")
if(!file.exists(spp)) {
  msg <- paste0("Range map not available for ", taxon_id, "; the file ", 
                paste0(path_lookup, "id_no_", taxon_id, ".gpkg"), 
                " does not exist")
  print(msg)
  if(is.null(not_processed_species[[paste0(taxon_id)]])){
    not_processed_species[[paste0(taxon_id)]] <- msg
  } else {
    tmp <- not_processed_species[[paste0(taxon_id)]]
    not_processed_species[[paste0(taxon_id)]] <- list(tmp, msg)
  }
  skip_to_next <<- TRUE;
}
if(skip_to_next) {stop(print(msg))}
# import polygons data
spp_range_data <- read_sf(spp)
st_geometry(spp_range_data) <- "geometry"

if(any(names(spp_range_data) == "terrestial")) names(spp_range_data)[names(spp_range_data) == "terrestial"] <- "terrestrial"

spp_range_data$terrestrial <- rep("true", nrow(spp_range_data))
spp_range_data$marine <- rep("false", nrow(spp_range_data))
spp_range_data$freshwater <- rep("false", nrow(spp_range_data))

if((!"binomial" %in% colnames(spp_range_data)) & ("sci_name"  %in% colnames(spp_range_data))){
  spp_range_data$binomial <- spp_range_data$sci_name
} else if ((!"binomial" %in% colnames(spp_range_data)) & (!"sci_name"  %in% colnames(spp_range_data))){
  msg <- paste0("Cannot process taxon id ", taxon_id, " | no scientific name available for taxon id ", taxon_id)
  print(msg)
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
  print(msg)
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
  print(msg)
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
if(skip_to_next) {stop(print(msg))}
# run the aoh code
source("rscripts/aoh_jeff_4wcmc/src_files/create_aoh_obj.R")
source("rscripts/aoh_jeff_4wcmc/src_files/apply_aoh_function.R") 

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
    print(msg)
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
  print("Error in reading the tif files when combining by season! The seasonalities are: ")
  print(combo)
  skip_to_next <<- TRUE;
  if(skip_to_next) { stop(print(msg)) }
}
if(!migratory_status){
  check_combo <- combo$resident
  text_file <- "Resident"
  source("rscripts/aoh_jeff_4wcmc/src_files/combine_seasonality.R")
  
}
if(migratory_status && !model_validation){
  check_combo <- combo$breeding
  text_file <- "Breeding"
  source("rscripts/aoh_jeff_4wcmc/src_files/combine_seasonality.R")
  
  check_combo <- combo$non_breeding
  text_file <- "Nonbreeding"
  source("rscripts/aoh_jeff_4wcmc/src_files/combine_seasonality.R")
  
  if(str_detect(type, "LR")){
    check_combo <- combo$validation_migrant 
    text_file <- "Breeding_and_Nonbreeding"
    source("rscripts/aoh_jeff_4wcmc/src_files/combine_seasonality.R")
  }
  
} else if(migratory_status && model_validation){
  check_combo <- combo$validation_migrant 
  text_file <- "Breeding_and_Nonbreeding"
  source("rscripts/aoh_jeff_4wcmc/src_files/combine_seasonality.R")
} 
# clean up
unlink(files_aoh, recursive = T)
if(file.exists(files_aoh[1])){
  files_aoh <- str_replace_all(files_aoh, "\\/\\/", "\\/")
  unlink(files_aoh, recursive = T)
}
rm(files_aoh, save_res_aoh)
rm(spp_aoh, spp, spp_range_data)
rm(check_combo, combo, spp_info_data, taxon_id, string_1, string_2, string_3, string_4, string_5)    
rm(file_bred, file_nonb, file_res, file_to_process)
save(not_processed_species, file = paste0(output_errors, "not_processed_species", tmp_name, ".RData"))

