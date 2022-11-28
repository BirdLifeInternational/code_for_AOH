#1. Loading the point localities: 
rm(list=ls())
library(foreach)
nthreads = 55
source("rscripts/list_libraries.R")
#source("rscripts/validation/pre_processing_validation/write_vect_point_localities.R")

#### mammals ####
class <- "mammals"
projection = "esri54017"
type = "lumbierres_LR"
# compute pts prevalence
source("rscripts/validation/src_files/compute_pts_prevalence.R")

print("done")


print("done")
unlink(paste0(normalizePath(tempdir()), "/"), recursive = TRUE)










