###### get IUCN general info #########
spp_summary_birds_mammals <- read_csv(paste0(cache_dir, "manual_download/spp_summary_birds_mammals.csv"),
                                      col_names = T, 
                                      col_types = cols(
                                        binomial = col_character(), category= col_character(), 
                                        systems = col_character(), elevation_from_iucn = col_character(), bird_or_mammal = col_character(),
                                        id_no= col_double(), elevation_lower= col_double(), 
                                        elevation_upper = col_double(), occasional_lower_altitude= col_double(), 
                                        occasional_upper_altitude= col_double(),
                                        realm = col_character(),
                                        terrestrial = col_character(), freshwater = col_character(), marine = col_character()
                                      ),
                                      show_col_types =  F)  %>% janitor::clean_names()

unique(spp_summary_birds_mammals$realm)
names(spp_summary_birds_mammals)
spp_summary_birds_mammals[is.na(spp_summary_birds_mammals$realm), "realm"] <- "unknown"
spp_summary_birds_mammals[str_detect(spp_summary_birds_mammals$realm, "\\|"), "realm"] <- "multiple"
spp_summary_birds_mammals <- unique(spp_summary_birds_mammals[, c("id_no", "binomial", "realm", "terrestrial", "freshwater",
                                                                  "marine",
                                                                  "order_name", "family_name", "elevation_from_iucn",
                                                                  "category", "bird_or_mammal", "elevation_lower","elevation_upper",         
                                                                  "occasional_lower_altitude", "occasional_upper_altitude")])

spp_summary_birds_mammals <- as.data.table(spp_summary_birds_mammals)
spp_summary_birds_mammals[, used_elevation_upper := elevation_upper]
spp_summary_birds_mammals[!is.na(occasional_upper_altitude), used_elevation_upper := occasional_upper_altitude]
spp_summary_birds_mammals[, used_elevation_lower := elevation_lower]
spp_summary_birds_mammals[!is.na(occasional_lower_altitude), used_elevation_lower := occasional_lower_altitude]

spp_summary_birds_mammals[, elevation_range := used_elevation_upper - used_elevation_lower]
spp_summary_birds_mammals[, mid_point_elevation := elevation_range/2 ]
spp_summary_birds_mammals <- spp_summary_birds_mammals[, -"used_elevation_upper"]
spp_summary_birds_mammals <- spp_summary_birds_mammals[, -"used_elevation_lower"]
spp_summary_birds_mammals[ order_name == "PASSERIFORMES" |
    order_name == "CUCULIFORMES" |
    order_name == "GALLIFORMES" |
    order_name == "COLUMBIFORMES" |
    order_name == "CAPRIMULGIFORMES" |
    order_name == "STRIGIFORMES" |
    order_name == "PSITTACIFORMES", bird_or_mammal := "bird"]

spp_summary_birds_mammals[id_no ==103823979,]
dim(spp_summary_birds_mammals)
length(unique(spp_summary_birds_mammals$id_no))


ele = fread("../aoh_out/output_R/validation/summary_elevation_range_maps.csv")
gbif = fread("../aoh_out/output_R/validation/summary_elevation_point_gbif.csv")
combi1 <- merge(spp_summary_birds_mammals, ele, by = "id_no", all = T)
combi2 <- unique(merge(combi1, gbif[, c("id_no","min_elevation_GBif","max_elevation_GBif","median_elevation_GBif")], by = "id_no", all.x = T))
dim(combi2)
dim(spp_summary_birds_mammals)
(combi2[duplicated(combi2$id_no), "id_no"])
fwrite(combi2, file = "../aoh_out/output_R/validation/summary_elevations_all.csv")
