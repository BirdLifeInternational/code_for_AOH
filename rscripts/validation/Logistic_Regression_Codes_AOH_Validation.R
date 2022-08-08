# Logistic_Model:: 

# Load the regression data (Species, no of suitable pixels, unsuitable pixels, mid point of 
# elevation, elevation range, realm and number of habitats )

source("rscripts/list_libraries.R")
source("rscripts/validation/pre_processing_validation/pre_processing_4_validation.R")
rm(list=ls())
data_model_prevalence <- fread("rscripts/validation/pre_processing_validation/data_model_prevalence.csv")

data_model_prevalence[is.na(binomial),]
data_model_prevalence[is.na(realm),]
data_model_prevalence[is.na(family_name),]
data_model_prevalence[is.na(order_name),]
data_model_prevalence[is.na(bird_or_mammal),]
data_model_prevalence[is.na(elevation_from_iucn),]
# ADJUST to run only birds or only mammals!
class <- "birds_and_mammals"
random_variable <- "family_name"
source("rscripts/validation/src_files/glmm_model_prevalence.R")

# to run the alternative model with order as random effect uncomment the following:
#random_variable <- "order_name"
#source("rscripts/validation/src_files/glmm_model_prevalence.R")
    
# Why to include species as random if you have mainly only one and sometimes two values per species?
# I see that actually it was not included because poorly performing!
# How did you calculate the number of habitats,
#' (potential or found within the AOH, do you count by breeding/non-breeding for migratory species)
# From paper:
#   "We merged resident seasonality to
# breeding and non-breeding seasonalities to have AOH maps
# with only two seasonalities (breeding and non-breeding)." Maybe you meant that you merged breeding and non-breeding together?
#   
#   run birds and mammals separately! Why? I nested them instead
#   
#   Only include species with elevation data from IUCN!
#  preferred family as random effect!
#' check model with lowest residual variance
  


