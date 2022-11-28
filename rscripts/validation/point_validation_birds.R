#1. Loading the point localities: 
rm(list=ls())
library(foreach)
nthreads = 1
source("rscripts/list_libraries.R")
#source("rscripts/validation/pre_processing_validation/write_vect_point_localities.R")
class <- "birds"
projection = "esri54017"
type = "lumbierres_LR"
template_ras = "../aoh_out/cache_dir/lumbierres_v2.0/lumbierres-10-5281_zenodo-5146073-v2.tif"
# compute pts prevalence
source("rscripts/validation/src_files/compute_pts_prevalence.R")

print("done")


print("done")
unlink(paste0(normalizePath(tempdir()), "/"), recursive = TRUE)










