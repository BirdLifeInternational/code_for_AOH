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