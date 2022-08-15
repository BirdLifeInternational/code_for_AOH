library(stringr)
library(dplyr)
library(janitor)

#### shapefile ####

attribute_table <- read.csv("../IUCN_data/NOT_to_be_committed/BOTW/attribute_table/attribute_table.csv")
names(attribute_table)
attribute_table <- unique(attribute_table[, c("sci_name", "id_no")])
attribute_table <- attribute_table[order(attribute_table$id_no),]
names(attribute_table) <- c("shp_binomial", "shp_id_no")


######## API ##########

el_passeriformes <- read.csv(paste0(cache_dir, 
                                    "manual_download/habitat_iucn/redlist_species_data_birds_passeriformes/all_other_fields.csv")) %>% 
  janitor::clean_names()
cat_passeriformes <- read.csv(paste0(cache_dir, 
                                     "manual_download/habitat_iucn/redlist_species_data_birds_passeriformes/assessments.csv")) %>% janitor::clean_names()

el_other_birds <- read.csv(paste0(cache_dir, 
                                  "manual_download/habitat_iucn/redlist_species_data_birds_except_passeriformes/all_other_fields.csv")) %>% janitor::clean_names()
cat_other_birds <- read.csv(paste0(cache_dir, 
                                   "manual_download/habitat_iucn/redlist_species_data_birds_except_passeriformes/assessments.csv")) %>% janitor::clean_names()

el_all_birds_api <- rbind(el_passeriformes, el_other_birds)
api_elevation <-  rbind(el_passeriformes, el_other_birds)
rm(el_passeriformes)
rm(el_other_birds)
api_elevation <-  unique(api_elevation[, c("assessment_id","scientific_name","internal_taxon_id")])
api_cat <- rbind(cat_passeriformes, cat_other_birds)
rm(cat_other_birds)
rm(cat_passeriformes)
api_cat <- unique(api_cat[, c("assessment_id","scientific_name","internal_taxon_id")])
api_cat <- api_cat[order(api_cat$internal_taxon_id),]

api_elevation <- api_elevation[order(api_elevation$internal_taxon_id),]

unique(api_cat$internal_taxon_id == api_elevation$internal_taxon_id)
unique(api_cat$scientific_name == api_elevation$scientific_name)

api <- unique(api_cat)
rm(api_cat); rm(api_elevation)
names(api) <- c("api_assessment_id", "api_binomial", "api_id")
########### compare shapefiles and api ###########

# expecting missing sensitive species
#attribute_table$shp_id_no == api$api_id
dim(attribute_table)
dim(api)
api_shp <- merge(attribute_table, api, all = TRUE, by.x = "shp_id_no", by.y = "api_id")
api_shp[is.na(api_shp$api_binomial) | is.na(api_shp$shp_binomial),]
summary(api_shp)
api_shp2 <- api_shp[!is.na(api_shp$shp_binomial),]
unique(api_shp2$api_binomial == api_shp2$shp_binomial)

#  differences between the two.
# In the shapefiles some species are missing because they are sensitive or extinct.
# All the rest matches!
# 
# SENSITIVE SPECIES:
# Gracula robusta
# Tanygnathus everetti
# Pterorhinus courtoisi
# The Caloenas maculata is extinct!



############ j folder ##############
j_folder_habitat <- readxl::read_excel("../IUCN_data/habitat_preferences/BL_Habitats_2021.xlsx",
                                       na = "NA") %>% janitor::clean_names()
j_folder_habitat <- unique(j_folder_habitat[, c("sis_id", "scientific_name",  "common_name")])
j_folder_habitat <- j_folder_habitat[order(j_folder_habitat$sis_id),]
names(j_folder_habitat) <- c("hab_sis_id", "hab_binomial", "hab_common")

j_folder_elevation <- readxl::read_excel("../IUCN_data/habitat_preferences/Updated_elevation_data_for_birds_30_Nov_2021_for_WCMC.xlsx",
                                         col_types = c("guess", rep("numeric", 5)),
                                         na = "NA") %>% janitor::clean_names()
j_folder_elevation <- unique(j_folder_elevation[, c("bird_life_scientific_name_2020","sis_rec_id")])
j_folder_elevation <- j_folder_elevation[order(j_folder_elevation$sis_rec_id),]
names(j_folder_elevation) <- c("el_binomial", "el_sis_id")


#unique(j_folder_elevation$el_sis_id == j_folder_habitat$hab_sis_id)

j_folder <- merge(j_folder_elevation, j_folder_habitat, all = T, by.x = "el_sis_id", by.y = "hab_sis_id")

j_folder[is.na(j_folder$el_binomial) | is.na(j_folder$hab_binomial),]

j_folder[which(j_folder$el_binomial != j_folder$hab_binomial),]

#j_folder[,]


############# compare j_folder_habitat with api ############

names(api)
names(j_folder_habitat)

api_habitat <- merge(api, j_folder_habitat, all = T, by.x = "api_id", by.y = "hab_sis_id")
api_habitat[is.na(api_habitat$hab_binomial) | is.na(api_habitat$api_binomial), ]
unique(api_habitat$hab_binomial == api_habitat$api_binomial)

# so the habitat data and api have the same ids!
############### synonyms taxonomy ############

syn <- read.csv(
  "../aoh_out/cache_dir/manual_download/habitat_iucn/redlist_species_data_birds_except_passeriformes/synonyms.csv")

syn$synonym <- str_c(syn$genusName, syn$speciesName, sep = " ")
syn[which(str_sub(syn$synonym,-1,-1)== " "), "synonym"] <- 
substr(syn[which(str_sub(syn$synonym,-1,-1)== " "), "synonym"],1,nchar(syn[which(str_sub(syn$synonym,-1,-1)== " "), "synonym"])-1)
syn$synonym
syn <- syn[,c("internalTaxonId","scientificName", "synonym")]
names(syn) <- c("syn_id", "syn_binomial", "syn_synonym")
###############
names(j_folder_elevation)
names(api)
api_syn <- merge(api, syn, by.x = "api_binomial", by.y = "syn_binomial", all.x = T)
api_elevation <- merge(api, j_folder_elevation, all = T, by.x = "api_binomial", by.y = "el_binomial")
dim(api_elevation[is.na(api_elevation$api_id) | is.na(api_elevation$el_sis_id),])


api_elevation_by_id <- merge(api, j_folder_elevation, all = T, by.x = "api_id", by.y = "el_sis_id")
dim(api_elevation_by_id[is.na(api_elevation_by_id$api_binomial) | is.na(api_elevation_by_id$el_binomial),])
missing_species <- api_elevation_by_id[is.na(api_elevation_by_id$api_binomial), "el_binomial"]
unique(api_elevation_by_id$api_binomial == api_elevation_by_id$el_binomial)
# some scientific names are different, so check if they are synonyms
different_names <- api_elevation_by_id[which(api_elevation_by_id$api_binomial != api_elevation_by_id$el_binomial),]
syn1 <- syn[,c("syn_id","syn_binomial")]
names(syn1)[2] <- "syn_synonym"
syn2 <- syn[,c("syn_id", "syn_synonym")]
syn3 <- unique(rbind(syn1, syn2))
check <- merge(different_names, syn3, by.x = "api_id", by.y = "syn_id", all.x = T)
check <- check[order(check$api_id),]
check[!is.na(check$syn_synonym),]

different_names <- unique(different_names)
dim(different_names)
length(unique(different_names$api_id))
write.csv(different_names[, c("api_binomial",	"api_id",	"el_binomial")], file = "../aoh_out/cache_dir/elevation_file_inconsistencies/different_scientific_names_but_same_ids.csv", row.names = F)



sort(setdiff(missing_species, api$api_binomial))
sort(setdiff(api$api_binomial, missing_species))

tmp <- api_elevation[api_elevation$api_binomial %in% missing_species,]

tmp <- tmp[order(tmp$api_binomial),]
tmp_na <- tmp[is.na(tmp$api_id),]
tmp_comp <- tmp[!is.na(tmp$api_id),]
tmp_comp$el_binomial <- rep("same scientific name", nrow(tmp_comp))
write.csv(tmp_comp[, c("api_binomial",	"api_id",	"el_binomial",	"el_sis_id")], file = "../aoh_out/cache_dir/elevation_file_inconsistencies/same_scientific_name_but_different_ids.csv",
          row.names = F)
tmp_na$el_binomial <- tmp_na$api_binomial
api_na <- api[api$api_binomial %in% c("Calendulauda africanoides", "Mirafra somalica", "Acridotheres melanopterus"),]
api_na <- api_na[order(api_na$api_binomial),]
api_na[api_na$api_binomial== "Calendulauda africanoides", c("el_binomial", "el_sis_id")] <- 
    c("Calendulauda alopex", subset(j_folder_elevation, el_binomial == "Calendulauda alopex")[,"el_sis_id"])
api_na[api_na$api_binomial== "Mirafra somalica", c("el_binomial", "el_sis_id")] <- 
  c("Mirafra ashi", subset(j_folder_elevation, el_binomial == "Mirafra ashi")[,"el_sis_id"])
api_na[api_na$api_binomial== "Acridotheres melanopterus", c("el_binomial", "el_sis_id")] <- 
  c("Acridotheres tertius",
    subset(j_folder_elevation, el_binomial == "Acridotheres tertius")[,"el_sis_id"])


 

adding <- data.frame(api_assessment_id = NA,
  api_binomial="Acridotheres melanopterus",
  api_id = api_na[api_na$api_binomial== "Acridotheres melanopterus", "api_id"],
  el_binomial = "Acridotheres tricolor", 
    el_sis_id = subset(j_folder_elevation, el_binomial == "Acridotheres tricolor")[,"el_sis_id"]) 
api_na <- rbind(api_na, adding)
write.csv(api_na[, c("api_binomial",	"api_id",	"el_binomial",	"el_sis_id")], file = "../aoh_out/cache_dir/elevation_file_inconsistencies/different_scientific_name_and_different_ids.csv",
          row.names = F)

j_folder_elevation[j_folder_elevation$el_binomial== "Acridotheres melanopterus",]
new <- rbind(tmp_comp, api_na[, names(tmp_comp)])
#write.csv(new, "missing_ids_elevation.csv")
j_folder_elevation[j_folder_elevation$el_binomial %in% missing_species,]



#################### reorder and correct the mistakes found ##############
j_folder_elevation <- readxl::read_excel("../IUCN_data/habitat_preferences/Updated_elevation_data_for_birds_30_Nov_2021_for_WCMC.xlsx",
                                         col_types = c("guess", rep("numeric", 5)),
                                         na = "NA") %>% janitor::clean_names()

name_ids <- read.csv("../aoh_out/cache_dir/elevation_file_inconsistencies/different_scientific_name_and_different_ids.csv",
      stringsAsFactors = F) %>% janitor::clean_names()

name <- read.csv("../aoh_out/cache_dir/elevation_file_inconsistencies/different_scientific_names_but_same_ids.csv", 
                 stringsAsFactors = F) %>% janitor::clean_names()
name$el_sis_id <- name$api_id

ids <- read.csv("../aoh_out/cache_dir/elevation_file_inconsistencies/same_scientific_name_but_different_ids.csv",
                stringsAsFactors = F) %>% janitor::clean_names()
one <- rbind(name, name_ids[, names(name)])
two <- rbind(one, ids[, names(one)])
two
res <- merge(j_folder_elevation, two, by.x = "sis_rec_id", by.y = "el_sis_id", all.x = T)
dim(j_folder_elevation)
dim(res)
j_folder_elevation[1:3,]
# api_binomial api_id
res[!is.na(res$api_binomial), "bird_life_scientific_name_2020"] <- res[!is.na(res$api_binomial), "api_binomial"]

res[!is.na(res$api_id), "sis_rec_id"] <- 
  res[!is.na(res$api_id), "api_id"]
res <- unique(res[,c("sis_rec_id","bird_life_scientific_name_2020",
                     "min_alt_new_rounded", "max_alt_new_rounded",
                     "occasional_lower_altitude_new",
                     "occasional_upper_altitude_new")])

names(res)[names(res)=="bird_life_scientific_name_2020"] <- "bird_life_scientific_name"
api <- unique(api)
 test1 <- merge(res, api, by.x ="sis_rec_id", by.y = "api_id", all.x = T)
 
 test2 <- merge(res, api, by.x ="bird_life_scientific_name", by.y = "api_binomial", all.x = T)
dim(res)
dim(test1)
dim(test2)
test1[is.na(test1$api_binomial),      ]
unique(test1$api_binomial == test1$bird_life_scientific_name)
test2[is.na(test2$api_id),]
unique(test2$api_id==test2$sis_rec_id)
names(res)
openxlsx::write.xlsx(res, overwrite = TRUE, 
file = "../aoh_out/cache_dir/elevation_file_inconsistencies/corrected_elevation_data.xlsx")

res[!is.na(res$min_alt_new_rounded) & !is.na(res$max_alt_new_rounded), "available_elevation"] <- "yes"
res[is.na(res$min_alt_new_rounded) | is.na(res$max_alt_new_rounded), "available_elevation"] <- "no"

openxlsx::write.xlsx(res, overwrite = TRUE, 
                     file = "../IUCN_data/habitat_preferences/corrected_elevation_data.xlsx")


############ compare final habitat data with iucn habitat codes ###############

habitats_all<- read.csv(paste0(cache_dir, "manual_download/spp_habitat_data_birds_mammals.csv"))

unique(habitats_all[habitats_all$habitats_level_2==
                      "Artificial/Aquatic - Karst and Other Subterranean Hydrological Systems (human-made)",
                    "code"])
sort(unique(habitats_all$code))
       
       dim(habitats_all)
#habitats_all<- hab
habitats_all<- subset(habitats_all, bird_or_mammal == "bird")
hab_birds1 <- read.csv(paste0(cache_dir,
                              "manual_download/habitat_iucn/redlist_species_data_birds_except_passeriformes/habitats.csv")) %>% janitor::clean_names()
hab_birds2 <- read.csv(paste0(cache_dir,
                              "manual_download/habitat_iucn/redlist_species_data_birds_passeriformes/habitats.csv")) %>% janitor::clean_names()
bir <- rbind(hab_birds1, hab_birds2)[,c("internal_taxon_id", "scientific_name",   "code")]
names(bir) <- c("iucn_id", "iucn_sci_nam", "iucn_code")
mer <- merge(habitats_all[,c("id_no", "code", "bird_or_mammal", "scientific_name")], bir, by.x = c("id_no", "code"), 
             by.y = c("iucn_id", "iucn_code"), all = T)
mer2 <- merge(mer, api, by.x ="scientific_name", by.y="api_binomial", all=T)
dim(mer)
dim(habitats_all)
unique(mer[is.na(mer$iucn_sci_nam),])
unique(mer[is.na(mer$bird_or_mammal), "id_no"])
unique(mer[is.na(mer$iucn_sci_nam), "id_no"])
unique(mer[is.na(mer$iucn_sci_nam), "code"])
bir[bir$iucn_sci_nam=="Emberiza schoeniclus", ]
# error in the habitat coding downloaded with api! the file i created is correct

