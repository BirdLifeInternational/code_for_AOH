# Choose if you want to process birds or mammals
rm(list = ls())
process_birds <- FALSE # set to false to process mammals
#type <- "LR" # FRC stands for fractional; LR stands for low-resolution
# If doing model validation set the following parameter model_validation to TRUE
# At one run set elevation_only to TRUE and habitat_only to FALSE
# At the next run the other way around
model_validation <- TRUE
elevation_only <- FALSE
habitat_only <- TRUE
#type <- "LR_validation_elevation_only"
type <- "LR_validation_habitat_only"
###### load libraries #####
source("rscripts/list_libraries.R")
##### Specify parameters ##########
# You need a machine with 8GB+ for small range maps and 16GB+ RAM for large range maps
verbose_value <- FALSE # set to FALSE if you don't want aoh pckg to display progress
# Technical details #
n_threads <- 1 # use multiple threads within aoh package
n_cores <- 10 # set the number of cores for mclapply
cache_limit <- 5000 # set to 5000 when running the whole species. 
###### Source the code #######
source("rscripts/aoh_jeff_4wcmc/src_files/aoh_with_lapply.R")

