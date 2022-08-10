#' read and reformat the point localities data

brd <- fread("../aoh_out/cache_dir/point_localities_data_4_validation/Point_localities_Ebird.csv",
             colClasses = c('character', 'numeric','numeric', 'character')) # load birds points 
mml <- fread("../aoh_out/cache_dir/point_localities_data_4_validation/Point_localities_Gbif.csv",
             colClasses = c('character', 'numeric','numeric', 'character')) # load birds points
setnames(mml, "decimalLongitude", "LONGITUDE")
setnames(mml, "decimalLatitude", "LATITUDE")  
setnames(mml, "gbifID", "id")
points <- rbindlist(list(brd, mml))
rm(brd, mml)

names(points)[names(points) == "Species"] <- "binomial"
points[, n_data_points := .N, by = binomial]
spp_summary_birds_mammals <- read_csv(paste0("../aoh_out/cache_dir/manual_download/spp_summary_birds_mammals.csv"),
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
spp_summary_birds_mammals <- unique(spp_summary_birds_mammals[, c("id_no", "binomial")])
ancillary <- read_csv("../IUCN_data/NOT_to_be_committed/BOTW/attribute_table/ancillary_taxonomy.csv")[, c("SISID", "Scientific_name")]
names(ancillary) <- c("id_no", "binomial")
spp_summary_birds_mammals <- unique(rbind(spp_summary_birds_mammals, ancillary))
points <- merge(points, spp_summary_birds_mammals, by = "binomial", all = T)
dim(points[is.na(LONGITUDE),])
dim(points[is.na(id_no),])
dim(points)

points <- subset(points, !is.na(LONGITUDE))
points <- subset(points, !is.na(id_no))
points <- points[, c("binomial", "LONGITUDE", "LATITUDE", "id_no")]
rm(spp_summary_birds_mammals)

