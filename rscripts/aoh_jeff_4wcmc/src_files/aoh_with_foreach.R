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
source("rscripts/aoh_jeff_4wcmc/src_files/set_directories.R")
source("rscripts/aoh_jeff_4wcmc/src_files/read_data_aoh.R")
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
  source("rscripts/aoh_jeff_4wcmc/src_files/aoh_computations.R")
}  


save(for_loop, file = paste0(output_errors, "foeach_out_", tmp_name, ".RData"))
print("done")
print(warnings())

#citation("aoh")


