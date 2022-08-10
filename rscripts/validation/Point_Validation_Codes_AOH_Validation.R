#1. Loading the point localities: 
rm(list=ls())
nthreads = parallel::detectCores() - 1
source("rscripts/list_libraries.R")
#source("rscripts/validation/pre_processing_validation/write_vect_point_localities.R")
class <- "birds"
type = "lumbierres_LR/"
# compute pts prevalence
source("rscripts/validation/src_files/compute_pts_prevalence.R")
#### mammals ####
class <- "mammals"
type = "lumbierres_LR/"
# compute pts prevalence
source("rscripts/validation/src_files/compute_pts_prevalence.R")
#### AOH_elevation for birds ####
class <- "birds"
type = "lumbierres_LR_validation_elevation_only/"
# compute pts prevalence
source("rscripts/validation/src_files/compute_pts_prevalence.R")
#### AOH_habitat for birds ####
class <- "birds"
type = "lumbierres_LR_validation_habitat_only/"
# compute pts prevalence
source("rscripts/validation/src_files/compute_pts_prevalence.R")

print("done")













