#' read and reformat the point localities data

brd <- fread("../aoh_out/cache_dir/point_localities_data_4_validation/Point_localities_Ebird.csv",
            colClasses = c('numeric','character', 'numeric','numeric', 'character')) # load birds points 
brd[, class:= "bird"]
brd <- brd[, -"V1"]
mml <- fread("../aoh_out/cache_dir/point_localities_data_4_validation/Point_localities_Gbif.csv",
             colClasses = c( 'numeric','character', 'numeric','numeric', 'character')) # load birds points
mml <- mml[, -"V1"]
setnames(mml, "decimalLongitude", "LONGITUDE")
setnames(mml, "decimalLatitude", "LATITUDE")  
setnames(mml, "gbifID", "id")
mml[, class := "mammal"]
points <- rbindlist(list(brd, mml))
rm(brd, mml)

names(points)[names(points) == "Species"] <- "binomial"
points[, n_data_points := .N, by = binomial]
# read RL stuff
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
spp_summary_birds_mammals$type = "rl"
ancillary <- read_csv("../IUCN_data/NOT_to_be_committed/BOTW/attribute_table/ancillary_taxonomy.csv")[, c("SISID", "Scientific_name")]
names(ancillary) <- c("id_no", "binomial")



### add synon
syn1 <- read_csv("../aoh_out/cache_dir/manual_download/habitat_iucn/redlist_species_data_birds_passeriformes/synonyms.csv")	
syn2 <- read_csv("../aoh_out/cache_dir/manual_download/habitat_iucn/redlist_species_data_birds_except_passeriformes/synonyms.csv")
syn3 <- read_csv("../aoh_out/cache_dir/manual_download/habitat_iucn/redlist_species_data_mammals_terrestrial/synonyms.csv")
syn <- rbind(syn1, syn2, syn3)
syn$binomial <- str_replace_all(paste0(syn$genusName, " ", syn$speciesName), "  ", " ")
syn$binomial <- str_replace_all(syn$binomial, " NA", "")
syn$id_no <- syn$internalTaxonId
syn <- unique(syn[!is.na(syn$id_no), c("id_no", "binomial")])
syn$type = "syn"
ancillary$type = "anci"

spp_summary_birds_mammals <- unique(rbind(spp_summary_birds_mammals, ancillary, syn))
spp_summary_birds_mammals <- unique(spp_summary_birds_mammals)
spp_summary_birds_mammals <- spp_summary_birds_mammals[!is.na(spp_summary_birds_mammals$binomial),]
spp_summary_birds_mammals$duplicates <- duplicated(spp_summary_birds_mammals$binomial)
spp_summary_birds_mammals$to_delete <- 0
spp_summary_birds_mammals[(spp_summary_birds_mammals$duplicates & spp_summary_birds_mammals$type == "anci"),"to_delete"] <- 1
spp_summary_birds_mammals[(spp_summary_birds_mammals$duplicates & spp_summary_birds_mammals$type == "syn"),"to_delete"] <- 1
spp_summary_birds_mammals <- spp_summary_birds_mammals[spp_summary_birds_mammals$to_delete == 0,]
spp_summary_birds_mammals$duplicates <- duplicated(spp_summary_birds_mammals$binomial)
if(nrow(spp_summary_birds_mammals[spp_summary_birds_mammals$duplicates,])>0) stop("duplicated binomials!")
unique(spp_summary_birds_mammals$type)

ori <- points

spp_summary_birds_mammals <- spp_summary_birds_mammals[!is.na(spp_summary_birds_mammals$binomial), ]
points <- merge(points, spp_summary_birds_mammals[,c("id_no", "binomial")], by = "binomial", all = T)
dim(points[is.na(LONGITUDE),])
dim(points[is.na(id_no),])
dim(points)
dim(ori)
length(unique(points$id_no))
print(length(unique(points[is.na(points$id_no), "binomial"])))
points <- subset(points, !is.na(LONGITUDE))
points <- subset(points, !is.na(id_no))
points <- points[, c("binomial", "LONGITUDE", "LATITUDE", "id_no", "class")]
rm(spp_summary_birds_mammals, ancillary, syn, syn1, syn2, syn3)

