#1. Loading the point localities: 
rm(list=ls())
nthreads = parallel::detectCores() - 1
source("rscripts/list_libraries.R")
#source("rscripts/validation/pre_processing_validation/write_vect_point_localities.R")
class <- "birds"
# compute pts prevalence
source("rscripts/validation/src_files/compute_pts_prevalence.R")
#### mammals ####
class <- "mammals"
# compute pts prevalence
source("rscripts/validation/src_files/compute_pts_prevalence.R")
print("done")
















