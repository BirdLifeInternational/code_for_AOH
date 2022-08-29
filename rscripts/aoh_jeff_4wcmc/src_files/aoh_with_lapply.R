#' Script to calculate AOH
#' We read model choices from the "run_aoh_....R" files
#' These choices indicate if the aoh should be:
#' either fractional or low resolution
#' either birds or mammals
#' either do model validation (habitat only or elevation only) or not!
#'
#'
#'
# Pick directories and input files
wd_path <- paste0(getwd(), "/")
source(paste0(wd_path, "rscripts/aoh_jeff_4wcmc/src_files/set_directories.R"))
source(paste0(wd_path, "rscripts/aoh_jeff_4wcmc/src_files/read_data_aoh.R"))
#"================================================================"
# Read in functions
source(paste0(wd_path, "rscripts/aoh_jeff_4wcmc/src_files/correct_season_mismatch.R"))
source(paste0(wd_path, "rscripts/aoh_jeff_4wcmc/src_files/calculate_area.R"))
source(paste0(wd_path, "rscripts/aoh_jeff_4wcmc/src_files/combine_seasonality.R"))
source(paste0(wd_path, "rscripts/aoh_jeff_4wcmc/src_files/aoh_computations.R"))
#"================================================================"
#"================================================================"
#"================================================================"
#########################################################################
#"================================================================"
#"================================================================"
#"================================================================"
#"================================================================"
##### apply codein parallel ####
##### apply codein parallel ####
n_cores = 20
library(foreach)
res <- foreach(X = 1:length(ordered_gpkg), .errorhandling= "pass") %do% {
  print(paste0("X = ", X))
  aoh_computation(X, ordered_gpkg = ordered_gpkg, path_lookup = path_lookup,
                  spp_summary_birds_mammals = spp_summary_birds_mammals,
                  spp_habitat_birds_mammals = spp_habitat_birds_mammals,
                  updated_path = updated_path,
                  model_validation = model_validation,
                  cache_dir = cache_dir,
                  verbose_value = verbose_value,
                  elevation_data = elevation_data,
                  output_dir = output_dir,
                  habitat_data = habitat_data,
                  crosswalk_data = crosswalk_data,
                  n_threads = n_threads,
                  cache_limit = cache_limit,
                  eng = eng)
  
}




# still doesn't work with dopar 
# files ending with _1.tif etc are NOT deleted automatically

save(res, file = paste0(output_errors, "lapply_out_", tmp_name, ".RData"))

lf <- list.files(output_errors, pattern = "msg_", full.names = T)
msgs <- rbindlist(lapply(lf, fread), fill = TRUE)
fwrite(msgs, file = paste0(output_dir, "all_messages.csv"))

lf <- list.files(output_errors, pattern = "Written_raster_file", full.names = T)
msgs <- rbindlist(lapply(lf, fread), fill = TRUE)
fwrite(msgs, file = paste0(output_dir, "all_written_rasters.csv"))
print("done")
print(warnings())

#citation("aoh")


