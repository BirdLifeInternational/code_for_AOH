# Read in the data for aoh calculations
###### Check species information #####
#  FROM the tutorial:
#' the spp_range_data is the species range
#  in the spp_summary_data you only need id_no elevation_lower elevation_upper category as a tibble
#  in the habitat data (spp_habitat_data) you need id_no and habitat preferences:
#' "id_no", "code", "habitat", "suitability", "season", "majorimportance"
#' 
#' 
# We subset spp_summary_birds_mammals to calculate model prevalence
# e.g. subset(spp_summary_birds_mammals, elevation_from_iucn == "yes") also subset for terrestrial only to be faster


spp_summary_birds_mammals <- read_csv(paste0(cache_dir, "manual_download/spp_summary_birds_mammals.csv"),
                                      col_names = T, 
                                      col_types = cols(
                                        binomial = col_character(), category= col_character(), 
                                        systems = col_character(), elevation_from_iucn = col_character(), bird_or_mammal = col_character(),
                                        id_no= col_double(), elevation_lower= col_double(), 
                                        elevation_upper = col_double(), occasional_lower_altitude= col_double(), 
                                        occasional_upper_altitude= col_double(),realm = col_character(),
                                        terrestrial = col_character(), freshwater = col_character(), marine = col_character()
                                      ),
                                      show_col_types =  F)  %>% janitor::clean_names()
species_exluded_because_marine <- spp_summary_birds_mammals 
species_exluded_because_marine <- subset(species_exluded_because_marine, 
                                         (marine == "true"))
write_csv(species_exluded_because_marine, file = paste0(output_dir, "species_with_NO_aoh_because_marine.csv"))
# These line below might cause some errors in the code, as we are removing species a priori
#spp_summary_birds_mammals <- subset(spp_summary_birds_mammals, terrestrial == "true")
#spp_summary_birds_mammals <- subset(spp_summary_birds_mammals, freshwater != "true")
spp_summary_birds_mammals <- subset(spp_summary_birds_mammals, marine != "true")

if(process_birds){
  spp_summary_birds_mammals <- subset(spp_summary_birds_mammals, bird_or_mammal = "bird")
} else {
  spp_summary_birds_mammals <- subset(spp_summary_birds_mammals, bird_or_mammal = "mammal")
}
spp_habitat_birds_mammals <- read_csv(paste0(cache_dir, "manual_download/spp_habitat_data_birds_mammals.csv"),
                                      col_names = T,  
                                      col_types = 
                                        cols(
                                          id_no = col_double(),
                                          scientific_name =col_character(), code =col_character(), 
                                          suitability =col_character(), major_importance =col_character(), season =col_character(), 
                                          habitats_level_1 =col_character(), habitats_level_2 =col_character(), seabird =col_character(), 
                                          waterbird=col_character(), landbird=col_character(), migratory_status=col_character(), 
                                          
                                          bird_or_mammal=col_character(), habitat=col_character(), migrant_in_iucn=col_character()
                                        ),show_col_types =  F)  %>% janitor::clean_names()
spp_habitat_birds_mammals[is.na(spp_habitat_birds_mammals$season), "season"] <- "Seasonal Occurrence Unknown"
spp_habitat_birds_mammals[spp_habitat_birds_mammals$season=="", "season"] <- "Seasonal Occurrence Unknown"

hb_iucn_code <- data.frame(season = 
                             sort(unique(spp_habitat_birds_mammals$season)),
                           code = c(2,3,4,1,5))

####### When doing model validation set parameters #####
if(model_validation == TRUE){
  spp_summary_birds_mammals <- subset(spp_summary_birds_mammals, elevation_from_iucn == "yes")
  if(habitat_only == TRUE){
    spp_summary_birds_mammals$elevation_lower <- -500
    spp_summary_birds_mammals$elevation_upper <- 9000
  } 
  hb_codes <- unique(crosswalk_data$code)
  hb_codes <- paste(hb_codes, collapse= "|")
}

####### Perform computations of AOH ######
source(paste0(wd_path, "rscripts/aoh_jeff_4wcmc/src_files/calculate_seasonality_combinations.R"))
bird_ids <- unique(subset(spp_summary_birds_mammals, bird_or_mammal == "bird")$id_no)
mammal_ids <- unique(subset(spp_summary_birds_mammals, bird_or_mammal == "mammal")$id_no)
i <- counter <- 11680
if(process_birds == TRUE){
  ids <- bird_ids
  path_lookup <- "../IUCN_data/NOT_to_be_committed/output_gpkg/birds/"
  
  
} else {
  ids <- mammal_ids
  path_lookup <- "../IUCN_data/NOT_to_be_committed/output_gpkg/mammals/"
}
path_lookup <- paste0(R.utils::getAbsolutePath(path_lookup), "/")
all_gpkg <- list.files(path_lookup, pattern = ".gpkg$", full.names = T)
ordered_gpkg <- file.info(all_gpkg)
ordered_gpkg$file_name <- str_replace(row.names(ordered_gpkg), "\\/\\/", "\\/")
ordered_gpkg <- ordered_gpkg[order(ordered_gpkg$size), ]
ordered_gpkg <- ordered_gpkg$file_name
not_processed_species <- check_errors <- list()
tmp_name <- paste0(tempdir())
tmp_name <- str_replace_all(tmp_name, "/", "_")
#########################################################################

