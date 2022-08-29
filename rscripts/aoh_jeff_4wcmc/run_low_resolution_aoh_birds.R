# Choose if you want to process birds or mammals
rm(list = ls())
process_birds <- TRUE # set to false to process mammals
type <- "LR" # FRC stands for fractional; LR stands for low-resolution
# If doing model validation set the following parameters to TRUE
model_validation <- FALSE
elevation_only <- FALSE
habitat_only <- FALSE
###### load libraries #####
source("rscripts/list_libraries.R")
##### Specify parameters ##########
# You need a machine with 8GB+ for small range maps and 16GB+ RAM for large range maps
verbose_value <- FALSE # set to FALSE if you don't want aoh pckg to display progress
# Technical details #
n_threads <- 1 # use multiple threads within aoh package
n_cores <- 20 # set the number of cores for mclapply
cache_limit <- 5000 # set to 5000 when running the whole species. 
###### Source the code #######
source("rscripts/aoh_jeff_4wcmc/src_files/aoh_with_lapply.R")

