#' Script to calculate AOH
#' We read model choices from the "run_aoh_....R" files
#' These choices indicate if the aoh shoould be:
#' either fractional or low resolution
#' either do model validation (habitat only or elevation only) or not!
#'
#'
#'
# Pick directories and input files
source("rscripts/aoh_jeff_4wcmc/src_files/set_directories.R")
###### Check species information #####
#  FROM the tutorial:
#' the spp_range_data is the species range
#  in the spp_summary_data you only need id_no elevation_lower elevation_upper category as a tibble
#  in the habitat data (spp_habitat_data) you need id_no and habitat preferences:
#' "id_no", "code", "habitat", "suitability", "season", "majorimportance"
#' 
#' 
# We subset spp_summary_birds_mammals to calculate model prevalence
# e.g. subset(spp_summary_birds_mammals, elevation_from_iucn == "yes") also subset for terrestrial only to be faster


spp_summary_birds_mammals <- read_csv(paste0(cache_dir, "manual_download/spp_summary_birds_mammals.csv"),
                                      col_names = T, 
                                      col_types = cols(
                                        binomial = col_character(), category= col_character(), 
                                        systems = col_character(), elevation_from_iucn = col_character(), bird_or_mammal = col_character(),
                                        id_no= col_double(), elevation_lower= col_double(), 
                                        elevation_upper = col_double(), occasional_lower_altitude= col_double(), 
                                        occasional_upper_altitude= col_double(),realm = col_character(),
                                        terrestrial = col_character(), freshwater = col_character(), marine = col_character()
                                      ),
                                      show_col_types =  F)  %>% janitor::clean_names()
species_exluded_because_marine <- spp_summary_birds_mammals 
species_exluded_because_marine <- subset(species_exluded_because_marine, 
                                         (marine == "true"))
write_csv(species_exluded_because_marine, file = paste0(output_dir, "species_with_NO_aoh_because_marine.csv"))
# These line below might cause some errors in the code, as we are removing species a priori
#spp_summary_birds_mammals <- subset(spp_summary_birds_mammals, terrestrial == "true")
#spp_summary_birds_mammals <- subset(spp_summary_birds_mammals, freshwater != "true")
spp_summary_birds_mammals <- subset(spp_summary_birds_mammals, marine != "true")

if(process_birds){
  spp_summary_birds_mammals <- subset(spp_summary_birds_mammals, bird_or_mammal = "bird")
} else {
  spp_summary_birds_mammals <- subset(spp_summary_birds_mammals, bird_or_mammal = "mammal")
}
spp_habitat_birds_mammals <- read_csv(paste0(cache_dir, "manual_download/spp_habitat_data_birds_mammals.csv"),
                                      col_names = T,  
                                      col_types = 
                                        cols(
                                          id_no = col_double(),
                                          scientific_name =col_character(), code =col_character(), 
                                          suitability =col_character(), major_importance =col_character(), season =col_character(), 
                                          habitats_level_1 =col_character(), habitats_level_2 =col_character(), seabird =col_character(), 
                                          waterbird=col_character(), landbird=col_character(), migratory_status=col_character(), 
                                          
                                          bird_or_mammal=col_character(), habitat=col_character(), migrant_in_iucn=col_character()
                                        ),show_col_types =  F)  %>% janitor::clean_names()
spp_habitat_birds_mammals[is.na(spp_habitat_birds_mammals$season), "season"] <- "Seasonal Occurrence Unknown"
spp_habitat_birds_mammals[spp_habitat_birds_mammals$season=="", "season"] <- "Seasonal Occurrence Unknown"

hb_iucn_code <- data.frame(season = 
                             sort(unique(spp_habitat_birds_mammals$season)),
                           code = c(2,3,4,1,5))

####### When doing model validation set parameters #####
if(model_validation == TRUE){
  spp_summary_birds_mammals <- subset(spp_summary_birds_mammals, elevation_from_iucn == "yes")
  if(elevation_only == TRUE){
    spp_summary_birds_mammals$elevation_lower <- -500
    spp_summary_birds_mammals$elevation_upper <- 9000
  } 
  hb_codes <- unique(crosswalk_data$code)
  hb_codes <- paste(hb_codes, collapse= "|")
}

####### Perform computations of AOH ######
source("rscripts/aoh_jeff_4wcmc/src_files/calculate_seasonality_combinations.R")
bird_ids <- unique(subset(spp_summary_birds_mammals, bird_or_mammal == "bird")$id_no)
mammal_ids <- unique(subset(spp_summary_birds_mammals, bird_or_mammal == "mammal")$id_no)
i <- counter <- 11680
if(process_birds == TRUE){
  ids <- bird_ids
  path_lookup <- "../IUCN_data/NOT_to_be_committed/output_gpkg/birds/"
  
} else {
  ids <- mammal_ids
  path_lookup <- "../IUCN_data/NOT_to_be_committed/output_gpkg/mammals/"
}
all_gpkg <- list.files(path_lookup, pattern = ".gpkg$", full.names = T)
ordered_gpkg <- file.info(all_gpkg)
ordered_gpkg$file_name <- row.names(ordered_gpkg)
ordered_gpkg <- ordered_gpkg[order(ordered_gpkg$size), ]
ordered_gpkg <- ordered_gpkg$file_name
not_processed_species <- check_errors <- list()
tmp_name <- paste0(tempdir())
tmp_name <- str_replace_all(tmp_name, "/", "_")
#########################################################################
#"================================================================"
#"================================================================"
#"================================================================"
#"================================================================"
#########################################################################
#"================================================================"
#"================================================================"
#"================================================================"
#"================================================================"
##### start for loop#####
for_loop <- foreach(i=1:length(ordered_gpkg),
                    .errorhandling='pass') %do% {
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
  
  
  
  
}  


save(for_loop, file = paste0(output_errors, "foeach_out_", tmp_name, ".RData"))
print("done")
print(warnings())

#citation("aoh")


