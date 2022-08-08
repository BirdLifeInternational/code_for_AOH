#### create data to read within aoh calculation ####
if(unique(st_geometry_type(spp_range_data)) == "MULTISURFACE"){
  ## S3 method for class 'POLYGON'
  msg <- paste0("Range map with potential error for ", taxon_id, "; the ", 
                paste0(path_lookup, "id_no_", taxon_id, ".gpkg"), 
                " is a multisurface object")
  print(msg)
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
