source("rscripts/list_libraries.R")
rm(list = ls())
#' The scripts read in input data and reformats them for the AOH analyses
#' 
#' The writtedn files are:
#' "cache_dir/list_of_ids_names.csv" a list of taxonomy for 2021
#  "cache_dir/errors_elevation_found_during_pre_processing.csv" list of species with error in elevation
#  "report_code_chunks/missing_info_about_terrestrial.csv" list of species without information about beig it terrestrial or not
#  "cache_dir/manual_download/spp_summary_birds_mammals.csv" cleaned info for aoh
#  "report_code_chunks/imputed_movement_patterns.csv" list of species with no info about movement patterns
#  "cache_dir/manual_download/spp_habitat_data_birds_mammals.csv" cleaned info about habitat
#' 
#' 
#' 
#' 
#' 
#' 
#' 
# specify cache directory
cache_dir <- "../aoh_out/cache_dir/"
##################read taxonomy######################3
tax_o <- read.csv(paste0(cache_dir, "manual_download/habitat_iucn/redlist_species_data_birds_except_passeriformes/taxonomy.csv"))
tax_o <- tax_o[,c("internalTaxonId","className","orderName","familyName",
                  "genusName")]
tax_p <- read.csv(paste0(cache_dir, "manual_download/habitat_iucn/redlist_species_data_birds_passeriformes/taxonomy.csv"))
tax_p <- tax_p[,c("internalTaxonId","className","orderName","familyName",
                  "genusName")]
tax_m <- read.csv(paste0(cache_dir, "manual_download/habitat_iucn/redlist_species_data_mammals_terrestrial/taxonomy.csv"))
tax_m <- tax_m[,c("internalTaxonId","className","orderName","familyName",
                  "genusName")]
taxonomy <- rbind(tax_o, tax_p, tax_m)
names(taxonomy)[names(taxonomy)=="internalTaxonId"] <- "id_no"
###############read in realm ######################
realm_o <- read.csv(paste0(cache_dir, "manual_download/habitat_iucn/redlist_species_data_birds_except_passeriformes/assessments.csv"))
realm_o <- unique(realm_o[, c("internalTaxonId", "realm")])


realm_p <- read.csv(paste0(cache_dir, "manual_download/habitat_iucn/redlist_species_data_birds_passeriformes/assessments.csv"))
realm_p <- unique(realm_p[, c("internalTaxonId", "realm")])
realm_m <- read.csv(paste0(cache_dir, "manual_download/habitat_iucn/redlist_species_data_mammals_terrestrial/assessments.csv"))
realm_m <- unique(realm_m[, c("internalTaxonId", "realm")])


realms <- unique(rbind(realm_o, realm_p, realm_m))
names(realms)[names(realms) ==  "internalTaxonId"] <- "id_no"
############ write a file with unique ids ##########
birds_habitat <- readxl::read_excel("../IUCN_data/habitat_preferences/BL_Habitats_2021.xlsx",
                                    na = "NA") %>% janitor::clean_names()
birds_nams <- unique(birds_habitat[, c("sis_id", "scientific_name",  "common_name")])
birds_nams$id_no <- birds_nams$sis_id
birds_nams$order <- rep("birds", nrow(birds_nams))
hab_mammals <- read.csv(paste0(cache_dir, 
                               "manual_download/habitat_iucn/redlist_species_data_mammals_terrestrial/habitats.csv")) %>% 
  janitor::clean_names()
names(hab_mammals)[names(hab_mammals) == "internal_taxon_id"] <- "id_no"
mammals_nams <- unique(hab_mammals[, c("id_no", "scientific_name", "name")])
names(mammals_nams)[names(mammals_nams)=="name"] <- "common_name"
mammals_nams$order <- rep("mammal", nrow(mammals_nams))
all_names <- unique(rbind(birds_nams[, names(mammals_nams)], mammals_nams))
attr_table <- read.csv("../IUCN_original_shapefiles/NOT_to_be_committed/BOTW/attribute_table/attribute_table.csv")
attr_table <-  attr_table[, c("id_no", "sci_name")]
tmp <- merge(all_names, attr_table, all = T)
tmp[is.na(tmp$scientific_name) & tmp$order == "bird",]
write_csv(all_names, file = paste0(cache_dir, "list_of_ids_names.csv"))


########## process elevation data #########
el_mammals <- read.csv(paste0(cache_dir, 
                              "manual_download/habitat_iucn/redlist_species_data_mammals_terrestrial/all_other_fields.csv")) %>% janitor::clean_names()
el_mammals <- el_mammals[, c("internal_taxon_id", "elevation_upper_limit", "elevation_lower_limit",
                             "movement_patterns_pattern")]
names(el_mammals) <- c("id_no", "elevation_upper","elevation_lower", "migrant")
cat_mammals <- read.csv(paste0(cache_dir, 
                               "manual_download/habitat_iucn/redlist_species_data_mammals_terrestrial/assessments.csv")) %>% janitor::clean_names()

el_passeriformes <- read.csv(paste0(cache_dir, 
                                    "manual_download/habitat_iucn/redlist_species_data_birds_passeriformes/all_other_fields.csv")) %>% janitor::clean_names()
cat_passeriformes <- read.csv(paste0(cache_dir, 
                                     "manual_download/habitat_iucn/redlist_species_data_birds_passeriformes/assessments.csv")) %>% janitor::clean_names()

el_other_birds <- read.csv(paste0(cache_dir, 
                                  "manual_download/habitat_iucn/redlist_species_data_birds_except_passeriformes/all_other_fields.csv")) %>% janitor::clean_names()
cat_other_birds <- read.csv(paste0(cache_dir, 
                                   "manual_download/habitat_iucn/redlist_species_data_birds_except_passeriformes/assessments.csv")) %>% janitor::clean_names()

el_all_birds_api <- rbind(el_passeriformes, el_other_birds)
movement_api_birds <- unique(el_all_birds_api[, c("internal_taxon_id", "movement_patterns_pattern")])
names(movement_api_birds)[names(movement_api_birds) == "internal_taxon_id"] <- "id_no"
#  elevation data from Stu
birds_elevation <- readxl::read_excel("../IUCN_data/habitat_preferences/Updated_elevation_data_for_birds_30_Nov_2021_for_WCMC.xlsx",
                                      col_types = c("guess", rep("numeric", 5)),
                                      na = "NA") %>% janitor::clean_names()
birds_elevation$occasional_lower_altitude <- birds_elevation$occasional_lower_altitude_new 
birds_elevation$occasional_upper_altitude <- birds_elevation$occasional_upper_altitude_new
birds_elevation <- birds_elevation[, c("sis_rec_id", "min_alt_new_rounded", "max_alt_new_rounded",
                                       "occasional_lower_altitude", "occasional_upper_altitude")]
names(birds_elevation)[names(birds_elevation) == "sis_rec_id"] <- "id_no"
names(birds_elevation)[names(birds_elevation) == "min_alt_new_rounded"] <- "elevation_lower" 
names(birds_elevation)[names(birds_elevation) == "max_alt_new_rounded"] <- "elevation_upper" 
birds_elevation$migrant <- rep(NA, nrow(birds_elevation))
birds_elevation$bird_or_mammal <- rep("bird", nrow(birds_elevation))

##### substitute occasional lower/upper elevation as the lower/upper values for birds #######
unique(birds_elevation$occasional_lower_altitude > birds_elevation$elevation_lower)
unique(birds_elevation$occasional_upper_altitude < birds_elevation$elevation_upper)
birds_elevation$original_lower_value <- birds_elevation$elevation_lower
birds_elevation$original_upper_value <- birds_elevation$elevation_upper
birds_elevation[!is.na(birds_elevation$occasional_lower_altitude), "elevation_lower"] <- 
    birds_elevation[!is.na(birds_elevation$occasional_lower_altitude), "occasional_lower_altitude"]

birds_elevation[!is.na(birds_elevation$occasional_upper_altitude), "elevation_upper"] <-
  birds_elevation[!is.na(birds_elevation$occasional_upper_altitude), "occasional_upper_altitude"]
el_mammals$occasional_lower_altitude <- rep(NA, nrow(el_mammals))
el_mammals$occasional_upper_altitude <- rep(NA, nrow(el_mammals))
el_mammals$original_lower_value <- el_mammals$elevation_lower
el_mammals$original_upper_value <- el_mammals$elevation_upper
el_mammals$bird_or_mammal <- rep("mammal", nrow(el_mammals))
# merge together birds and mammals
names(el_mammals)
names(birds_elevation)
el_all <- rbind(el_mammals[, c(names(birds_elevation))], birds_elevation)

############ RL category ##############
cat_all <- rbind(cat_mammals, cat_passeriformes, cat_other_birds)[, c("internal_taxon_id", "scientific_name", 
                                                                      "redlist_category", "systems")]


categ <- data.frame(redlist_category = c("Not Evaluated", "Data Deficient", "Least Concern", 
                                         "Near Threatened", "Vulnerable", "Endangered", 
                                         "Critically Endangered", "Extinct in the Wild", "Extinct"),
                    category = c("NE", "DD","LC", "NT", "VU", "EN", "CR", "EW", "EX")) 
cat_all_2 <- merge(cat_all, categ, all = T)
any(is.na(cat_all_2$category))
names(cat_all_2)[names(cat_all_2) == "internal_taxon_id"] <- "id_no"

####### merge RL category to elevation #######
ensemb <- merge(el_all, cat_all_2, all = T)
names(ensemb)[names(ensemb) == "internal_taxon_id"] <- "id_no"
names(ensemb)[names(ensemb) == "elevation_upper_limit"] <- "elevation_upper"
names(ensemb)[names(ensemb) == "elevation_lower_limit"] <- "elevation_lower"
ensemb$elevation_from_iucn <- ifelse(!is.na(ensemb$elevation_lower) & !is.na(ensemb$elevation_upper), "yes", "no")

##### check wrong/missing elevations #####
any(ensemb$elevation_upper < ensemb$elevation_lower)
any(ensemb$elevation_lower < -500)
any(ensemb$elevation_upper > 9000)
any((ensemb$elevation_upper - ensemb$elevation_lower) < 50)
a <- ensemb[which(ensemb$elevation_upper < ensemb$elevation_lower), ]
a$error <- rep("Lower elevation > Upper elevation", nrow(a))
b <- ensemb[which(ensemb$elevation_lower < -500),]
b$error <- rep("Lower elevation < -500 m", nrow(b))
c <- ensemb[which(ensemb$elevation_upper > 9000),]
c$error <- rep("Upper elevation > 9000 m", nrow(c))
d <- ensemb[which((ensemb$elevation_upper - ensemb$elevation_lower) < 50), ]
d$error <- rep("(Upper elevation - Lower elevation) < -50 m", nrow(d))
# write to file all ids with potential errors in elevation
el_errors <- rbind(a, b, c, d)
el_errors <- el_errors[, c("id_no", "scientific_name", "original_lower_value","original_upper_value",
                           "occasional_lower_altitude","occasional_upper_altitude", "bird_or_mammal",
                           "error")]
el_errors <- el_errors[order(el_errors$error, el_errors$bird_or_mammal),]
write_csv(el_errors, file = paste0(cache_dir, "errors_elevation_found_during_pre_processing.csv"))

ensemb[is.na(ensemb$elevation_upper), "elevation_upper"] <- 9000
ensemb[is.na(ensemb$elevation_lower), "elevation_lower"] <- -500

ensemb[which((ensemb$elevation_upper - ensemb$elevation_lower) < 50), "elevation_difference"] <-  
  ceiling((ensemb[which((ensemb$elevation_upper - ensemb$elevation_lower) < 50), "elevation_upper"] - 
  ensemb[which((ensemb$elevation_upper - ensemb$elevation_lower) < 50), "elevation_lower"]) / 2)

ensemb[!is.na(ensemb$elevation_difference), "elevation_lower"] <- 
  ensemb[!is.na(ensemb$elevation_difference), "elevation_lower"] -
  ensemb[!is.na(ensemb$elevation_difference), "elevation_difference"]

ensemb[!is.na(ensemb$elevation_difference), "elevation_upper"] <- 
  ensemb[!is.na(ensemb$elevation_difference), "elevation_upper"] +
  ensemb[!is.na(ensemb$elevation_difference), "elevation_difference"]



ensemb[which(ensemb$elevation_upper < ensemb$elevation_lower), c("elevation_lower", "elevation_upper")] <- c(-500, 9000)
ensemb[which(ensemb$elevation_lower < -500), "elevation_lower"] <- -500
ensemb[which(ensemb$elevation_upper > 9000), "elevation_upper"] <- 9000


names(ensemb)[names(ensemb) == "scientific_name"] <- "binomial"
migrant_info <- ensemb[which(ensemb$migrant != ""), c("id_no", "migrant")]
migrant_info <- unique(migrant_info)
ensemb <- ensemb[, c("id_no", "binomial", "category", "elevation_lower", "elevation_upper", 
                     "occasional_lower_altitude", "occasional_upper_altitude", "systems", 
                     "elevation_from_iucn", "bird_or_mammal")]

ensemb$terrestrial <- ifelse(str_detect(ensemb$systems, "Terrestrial"), "true", "false")
ensemb$freshwater <- ifelse(str_detect(ensemb$systems, "Freshwater"), "true", "false")
ensemb$marine <- ifelse(str_detect(ensemb$systems, "Marine"), "true", "false")
names(ensemb)
ensemb <- ensemb[!is.na(ensemb$id_no),]

all_nams <- read_csv(paste0(cache_dir, "list_of_ids_names.csv"))
missing_marine <- unique(ensemb[is.na(ensemb$terrestrial), c("id_no", "bird_or_mammal", "terrestrial", "freshwater", "marine")])
missing_marine <- unique(merge(missing_marine, all_nams[,c("id_no",	"scientific_name",	"common_name")], all.x = T))


birds_elevation <- readxl::read_excel("../IUCN_data/habitat_preferences/Updated_elevation_data_for_birds_30_Nov_2021_for_WCMC.xlsx",
                                      col_types = c("guess", rep("numeric", 5)),
                                      na = "NA") %>% janitor::clean_names()
birds_elevation <- birds_elevation[, c("sis_rec_id", "bird_life_scientific_name_2020")]
tmp2 <- merge(birds_elevation, ensemb, by.x = "bird_life_scientific_name_2020", by.y = "binomial", all = T)
tmp <- merge(missing_marine, birds_elevation, all.x =T, by.x = "id_no", by.y = "sis_rec_id")
tmp3 <- tmp2[tmp2$bird_life_scientific_name_2020 %in% tmp$bird_life_scientific_name_2020, c("bird_life_scientific_name_2020", "id_no", "sis_rec_id")]
tmp3[is.na(tmp3$id_no),]
# These are changed taxonomy!
write_csv(missing_marine, file = "report_code_chunks/missing_info_about_terrestrial.csv")
# for the species with missing information assume they are not terrestrial

ensemb[is.na(ensemb$terrestrial), "terrestrial"] <- "false"
ensemb[is.na(ensemb$freshwater), "freshwater"] <- "false"
ensemb[is.na(ensemb$marine), "marine"] <- "false"
ensemb1 <- ensemb
ensemb2 <- merge(ensemb1, taxonomy, by = "id_no", all.x = T)
ensemb <- merge(ensemb2, realms, by = "id_no", all.x = T)
dim(ensemb)
dim(ensemb1)
dim(ensemb2)
write_csv(ensemb, paste0(cache_dir, "manual_download/spp_summary_birds_mammals.csv"), col_names = T)
############habitats ############
#  Habitat data from internal Birdlife drive

birds_habitat <- readxl::read_excel("../IUCN_data/habitat_preferences/BL_Habitats_2021.xlsx",
                                    na = "NA") %>% janitor::clean_names()
birds_habitat <- birds_habitat %>% select(c("sis_id",   "scientific_name" ,       
                                            "seabird", "waterbird", "landbird",       
                                            "migratory_status", 
                                            "habitats_level_1", "habitats_level_2",
                                            "major_importance", "suitability", "season",
                                            "seabird", "waterbird", "landbird"))

        
names(birds_habitat)[names(birds_habitat) == "sis_id"] <- "id_no"

hab_mammals <- read.csv(paste0(cache_dir, 
                               "manual_download/habitat_iucn/redlist_species_data_mammals_terrestrial/habitats.csv")) %>% janitor::clean_names()
names(hab_mammals)[names(hab_mammals) == "internal_taxon_id"] <- "id_no"

hab_mammals2 <- merge(hab_mammals, migrant_info, all.x = T)
hab_mammals <- hab_mammals2 
names(hab_mammals)
hab_mammals$migratory_status <- hab_mammals$migrant
hab_birds1 <- read.csv(paste0(cache_dir,
                               "manual_download/habitat_iucn/redlist_species_data_birds_except_passeriformes/habitats.csv")) %>% janitor::clean_names()
hab_birds2 <- read.csv(paste0(cache_dir,
                              "manual_download/habitat_iucn/redlist_species_data_birds_passeriformes/habitats.csv")) %>% janitor::clean_names()

birds_habitat[is.na(birds_habitat$habitats_level_2), "habitats_level_2"] <- birds_habitat[is.na(birds_habitat$habitats_level_2), "habitats_level_1"]
hab_all1 <- rbind(hab_birds1, hab_birds2)
names(hab_all1)[names(hab_all1)== "internal_taxon_id"] <- "id_no"
hab_all1$migratory_status <- rep(NA, nrow(hab_all1))
hab_all <- rbind(hab_all1, hab_mammals[, c(names(hab_all1))])
hab_unique_codes <- unique(hab_all[, c("code", "name")])
hab_unique_codes <- hab_unique_codes[order(hab_unique_codes$code),]
hab_unique_codes[hab_unique_codes$name == "Wetlands (inland) - Tundra Wetlands (incl. pools and temporary waters from snowmelt)", "code"]<- "5.10"

hab_unique_codes <- unique(hab_unique_codes)
if(nrow(hab_unique_codes)!=length(unique(hab_unique_codes$code))) stop("Error: code habitats do not match")
class(hab_unique_codes$code)
birds_habitat2 <- merge(birds_habitat, hab_unique_codes, all.x = T, by.x = "habitats_level_2", by.y = "name")
unique(birds_habitat2$code)
birds_habitat2[is.na(birds_habitat2$code), c("habitats_level_2", "habitats_level_1")]

birds_habitat <- birds_habitat2[, c("id_no", "code", "scientific_name" ,
                   "suitability", "major_importance", "season", 
                   "habitats_level_1", "habitats_level_2",
                   "seabird", "waterbird", "landbird",
                   "migratory_status")]
hab_mammals$habitats_level_2 <- hab_mammals$name
hab_mammals <- hab_mammals[, c("id_no", "code", "scientific_name" ,
                               "suitability", "major_importance", "season",
                               "habitats_level_2", "migratory_status")]
hab_mammals$habitats_level_1 <- hab_mammals$seabird <- rep(NA, nrow(hab_mammals))
hab_mammals$waterbird <- hab_mammals$landbird <- rep(NA, nrow(hab_mammals))
#hab_mammals$id_no <- hab_mammals$internal_taxon_id
hab_mammals <- hab_mammals[, c("id_no", "code", "scientific_name" ,
                               "suitability", "major_importance", "season", 
                               "habitats_level_1", "habitats_level_2",
                               "seabird", "waterbird", "landbird",
                               "migratory_status")]
birds_habitat$bird_or_mammal <- rep("bird", nrow(birds_habitat))
hab_mammals$bird_or_mammal <- rep("mammal", nrow(hab_mammals))
habitats_all <- rbind(birds_habitat[, c(names(birds_habitat))], hab_mammals[, c(names(birds_habitat))])
habitats_all[which(habitats_all$season == "resident"), "season"] <- "Resident"
habitats_all[which(habitats_all$season == "non-breeding"), "season"] <- "Non-Breeding Season"
habitats_all[which(habitats_all$season == "breeding"), "season"] <- "Breeding Season"
habitats_all[which(habitats_all$season == "unknown"), "season"] <- "Seasonal Occurrence Unknown"
habitats_all[which(habitats_all$season == "passage"), "season"] <- "Passage"
habitats_all$habitat <- habitats_all$habitats_level_2
unique(habitats_all$migratory_status)
table(habitats_all$migratory_status)
habitats_all$is_migrant <- rep(NA, nrow(habitats_all))
habitats_all[which(stringr::str_detect(habitats_all$migratory_status, pattern = "Not a Migrant")), "is_migrant"] <- 0
habitats_all[which(stringr::str_detect(habitats_all$migratory_status, pattern = "Nomadic")), "is_migrant"] <- 0
#habitats_all[which(stringr::str_detect(habitats_all$migratory_status, pattern = "Unknown")), "is_migrant"] <- 0
habitats_all[which(stringr::str_detect(habitats_all$migratory_status, pattern = "Full Migrant")), "is_migrant"] <- 1
habitats_all[which(stringr::str_detect(habitats_all$migratory_status, pattern = "Altitudinal Migrant")), "is_migrant"] <- 1
unique(habitats_all[,c("migratory_status", "is_migrant")])
table(habitats_all$migratory_status)
length(is.na(habitats_all$migratory_status))
habitats_all[which(habitats_all$is_migrant == 0), "migrant_in_iucn"] <- "not-migrant"
habitats_all[which(habitats_all$is_migrant == 1), "migrant_in_iucn"] <- "migrant"


my_fun_paste <- function(x){
  x <- unique(sort(x))
  tmp <- NULL
  for(i in 1:length(x)){
    tmp <- paste0(tmp, "+", x[i])
  }
  return(tmp)
}
my_fun_paste(x = c("z", "z", "zz", "a"))
habitats_all <- habitats_all %>% group_by(id_no) %>% mutate(combined_season_habitat = my_fun_paste(season))
habitats_all$combined_season_habitat


######### read and determine if species migrates from polygons info #########

birds_attribute_table <- read_csv("../IUCN_data/NOT_to_be_committed/BOTW/attribute_table/attribute_table.csv") %>% 
  janitor::clean_names()
birds_attribute_table <- unique(birds_attribute_table[,c("sci_name", "seasonal", "id_no")])
names(birds_attribute_table)[names(birds_attribute_table)=="sci_name"] <- "binomial"
mammals_attribute_table <- read_csv("../IUCN_data/NOT_to_be_committed/MAMMALS_TERRESTRIAL_ONLY/attribute_table/attribute_table.csv") %>% 
  janitor::clean_names()
mammals_attribute_table <- unique(mammals_attribute_table[,c("binomial", "seasonal", "id_no")])

attribute_table <- unique(rbind(birds_attribute_table, mammals_attribute_table[, names(birds_attribute_table)]))
attribute_table[attribute_table$id_no==503146,]


attribute_table <- attribute_table %>% group_by(binomial)  %>% mutate(length = n())
attribute_table$migrant_polygons <- rep(NA, nrow(attribute_table))

attribute_table <- attribute_table %>% group_by(id_no) %>% 
  mutate(combined_season_polygon = my_fun_paste(seasonal))
attribute_table$combined_season_polygon
levels_combined <- data.frame(combined = unique(attribute_table$combined_season_polygon))
levels_combined[levels_combined$combined == "+2+3+4" |
                  levels_combined$combined == "+1+2+3+4"|
                  levels_combined$combined =="+1+2+3+5" |
                  levels_combined$combined == "+1+2+3"|
                  levels_combined$combined == "+1+2+3+4+5"|
                  levels_combined$combined == "+2+3+4+5" |
                  levels_combined$combined == "+2+3+5"  |
                  levels_combined$combined == "+2+3"
                  , "migrant_poly_combined"] <- "migrant"

levels_combined[levels_combined$combined == "+1" |
                  levels_combined$combined == "+1+3"|
                  levels_combined$combined =="+1+5" |
                  levels_combined$combined == "+1+4"|
                  levels_combined$combined == "+1+2+4"|
                  levels_combined$combined == "+1+2" |
                  levels_combined$combined == "+5"  |
                  levels_combined$combined == "+1+3+4"  |
                  levels_combined$combined == "+2+4"  |
                  levels_combined$combined == "+2"  |
                  levels_combined$combined == "+1+3+5"  |
                  levels_combined$combined == "+3"  |
                  levels_combined$combined == "+1+2+5"  |
                  levels_combined$combined == "+1+3+4+5"  |
                  levels_combined$combined == "+1+4+5"  
                , "migrant_poly_combined"] <- "not_migrant"

    

attribute_table <- unique(attribute_table[, c("id_no", "combined_season_polygon")])
test <- merge(attribute_table, levels_combined,by.x = "combined_season_polygon", by.y="combined", all = T)
dim(attribute_table)
dim(test)
unique(test$migrant_poly_combined == test$migrant_polygons)
test[which(test$migrant_poly_combined != test$migrant_polygons),]
test[is.na(test$migrant_polygons),]
attribute_table <- test
########### compare polygons wiyh IUCN movement patterns ######
habitats1<- merge(movement_api_birds, habitats_all, all.y = T)
habitats2 <- merge(habitats1, attribute_table, all.x = T)
dim(habitats1)
dim(habitats2)
dim(habitats_all)
unique(habitats2[is.na(habitats2$migrant_in_iucn), "movement_patterns_pattern"])
unique(habitats2[, 
                 c(
                   "migratory_status", "migrant_in_iucn", 
                   "migrant_poly_combined"#, "combined_season_habitat", "combined_season_polygon"
                 )])
all_nams <- read_csv(paste0(cache_dir, "list_of_ids_names.csv"))
imputing <- unique(habitats2[is.na(habitats2$migrant_in_iucn) & !is.na(habitats2$combined_season_polygon), c("migrant_poly_combined","id_no", "bird_or_mammal")])
imputing <- unique(merge(imputing, all_nams[,c("id_no",	"scientific_name",	"common_name")], all.x =  T))
# species with no polygons no range map
habitats2[is.na(habitats2$migrant_in_iucn) & is.na(habitats2$migrant_poly_combined),]
write_csv(imputing, "report_code_chunks/imputed_movement_patterns.csv")
# note that marine mammals have habitats2$combined_season_polygon == NA
unique(habitats2[is.na(habitats2$migrant_in_iucn) & is.na(habitats2$migrant_poly_combined) & !is.na(habitats2$combined_season_polygon), 
                 c(
                   "id_no", "migratory_status",  "bird_or_mammal", "migrant_in_iucn", 
                   "migrant_poly_combined",
                   #"combined_season_habitat", 
                   "combined_season_polygon"
                 )])

# replace the missing values
habitats2[is.na(habitats2$migrant_in_iucn) & !is.na(habitats2$combined_season_polygon), "migrant_in_iucn"] <-
  habitats2[is.na(habitats2$migrant_in_iucn) & !is.na(habitats2$combined_season_polygon), "migrant_poly_combined"]

habitats2[is.na(habitats2$migrant_in_iucn) & is.na(habitats2$combined_season_polygon),]



names(habitats2)

habitats2 <- habitats2[, c("id_no", "scientific_name" ,"code","suitability","major_importance","season",
                           "habitats_level_1","habitats_level_2","seabird",                
                            "waterbird","landbird","migratory_status", "bird_or_mammal",         
                            "habitat","migrant_in_iucn" )]
habitats_all <- habitats2
unique(habitats2$migrant_in_iucn)


habitats2[which(habitats2$migrant_in_iucn== "not-migrant"), "migrant_in_iucn"] <- "not_migrant"
write_csv(habitats2, 
          file = paste0(cache_dir, "manual_download/spp_habitat_data_birds_mammals.csv"),
          col_names = T, append = FALSE)

unique(habitats2$migrant_in_iucn)

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

unique(spp_habitat_birds_mammals$migrant_in_iucn)
