rm(list=ls())
source("rscripts/list_libraries.R")
cache_dir <- "../aoh_out/cache_dir/"

#### collate information about number of pixels suitable and not ##########
lf_b <- list.files("../aoh_out/output_R/birds/lumbierres_LR/area/", pattern = ".csv$", full.names = T)
lf_m <- list.files("../aoh_out/output_R/mammals/lumbierres_LR/area/", pattern = ".csv$", full.names = T)
lf <- c(lf_b, lf_m)
out <- rbindlist(lapply(lf, fread), fill = TRUE)
# Seasonality should be Resident, Breeding and non-breeding
# so subset and delete the Breeding_and_Non-breeding ones!
unique(out$type)
#out <- subset(out, type != "Breeding_and_Nonbreeding")
out$seasonality <- out$type
names(out)
out <- unique(out[, c("id_no", "total_num_pix", "suitable_num_pix", "not_suitable_num_pix", "seasonality")])
###### collate info processed by aoh package summarising the analysed data #####
lf_df_b <- list.files("../aoh_out/output_R/birds/lumbierres_LR/df", pattern = ".csv$", full.names = T)
lf_df_m <- list.files("../aoh_out/output_R/mammals/lumbierres_LR/df", pattern = ".csv$", full.names = T)
lf_df <- c(lf_df_b, lf_df_m)
spp_proc <- rbindlist(lapply(lf_df, fread), fill = TRUE)
spp_proc$elevation_range <- spp_proc$elevation_upper - spp_proc$elevation_lower
spp_proc$mid_point_elevation <- (spp_proc$elevation_range)/2
spp_proc$seasonality_of_hab <- str_sub(spp_proc$path, - 5, -5)
spp_proc[is.na(spp_proc$seasonality_of_hab),]
unique(cbind(spp_proc$seasonal, spp_proc$seasonality_of_hab))
setkey(spp_proc, id_no)
check = unique(spp_proc[,c("id_no", "full_habitat_code")])
dim(check)
length(unique(spp_proc$id_no))
check[duplicated(check$id_no),]
spp_proc[id_no == 103718860,]
103768594
103785851
count_hab <- function(x){
  t2 <- str_c(x, collapse = "|", sep = "|")
  len <- length(as.list(unique(strsplit(t2, split = "\\|")[[1]])))
  # lst <- as.list(unique(strsplit(t2, split = "\\|")[[1]]))
  return(len)
}
spp_proc[,count_hab_full := count_hab(full_habitat_code), by=.(id_no)]
spp_proc[,count_hab_used := count_hab(habitat_code),by=.(id_no)]

sp_not_all_hab_used <- spp_proc[count_hab_full != count_hab_used, ]
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
spp_habitat_birds_mammals <- unique(spp_habitat_birds_mammals[, c("id_no", "migrant_in_iucn")])
sp_not_all_hab_used2 <- merge(sp_not_all_hab_used, spp_habitat_birds_mammals, all.x = T)
dim(sp_not_all_hab_used)
dim(sp_not_all_hab_used2)
sp_not_all_hab_used <- sp_not_all_hab_used2
compare_hab <- function(x, y){
  `%ni%` <- Negate(`%in%`)
  
  x1 <- as.list(unique(strsplit(x, split = "\\|")[[1]]))

  y1 <- as.list(unique(strsplit(y, split = "\\|")[[1]]))
  
  lst <- x1[x1 %ni% y1]
  miss <- paste0(unlist(lst))
  t2 <- miss[1]
  if(length(miss)>1){
  for(i in 2:length(miss)){
    t2 <- paste(t2, miss[i], sep ="|")
  }
  }
  print(class(miss))
  return(t2)
}
st <- list()
for(i in 1:nrow(sp_not_all_hab_used)){
  tmp <- sp_not_all_hab_used[i,]
  tmp$code_missing <- compare_hab(tmp$full_habitat_code, tmp$habitat_code)
  st[[i]] <- tmp[, c("id_no", "full_habitat_code", "habitat_code", "code_missing", "count_hab_full", "count_hab_used" ,
                "seasonal", "migrant_in_iucn")]#, "path")]
}
sp_not_all_hab_used <- rbindlist(st)
sp_not_all_hab_used[ , combo_seas := paste0(sort(seasonal)), by = id_no]
sp_not_all_hab_used[ , all_code_missing := paste0(sort(unique(code_missing)), collapse = "|"), by = id_no]
source("rscripts/aoh_jeff_4wcmc/src_files/calculate_seasonality_combinations.R")
seas
sp_not_all_hab_used[migrant_in_iucn == "not_migrant", seasonality := "Resident"]
sp_not_all_hab_used[migrant_in_iucn == "migrant", seasonality := "Breeding_and_Nonbreeding"]
sp_not_all_hab_used <- unique(sp_not_all_hab_used[, c("id_no", "all_code_missing", "seasonality")])
fwrite(sp_not_all_hab_used, "../aoh_out/output_R/validation/aoh_where_some_habitat_codes_where_not_found.csv")

# here we count the number of habitat in the IUCN per species (all seasons added)
# and laso the number of habitats actually used


spp_proc <- unique(spp_proc[,c("id_no", "full_habitat_code", "elevation_range","mid_point_elevation", 
                               "count_hab_full", "count_hab_used")])
##### merge the output info ########

out2 <- unique(merge(out, spp_proc, all= T, by = "id_no"))
dim(out)
dim(spp_proc)
dim(out2)
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
spp_summary_birds_mammals <- subset(spp_summary_birds_mammals, terrestrial == "true")
spp_summary_birds_mammals <- subset(spp_summary_birds_mammals, freshwater != "true")
spp_summary_birds_mammals <- subset(spp_summary_birds_mammals, marine != "true")
spp_summary_birds_mammals <- subset(spp_summary_birds_mammals, elevation_from_iucn == "yes")
unique(spp_summary_birds_mammals$realm)
names(spp_summary_birds_mammals)
spp_summary_birds_mammals[str_detect(spp_summary_birds_mammals$realm, "\\|"), "realm"] <- "multiple"
spp_summary_birds_mammals <- unique(spp_summary_birds_mammals[, c("id_no", "binomial", "realm",
                                                                  "order_name", "family_name",
                                                                  "category", "bird_or_mammal", "elevation_from_iucn")])
spp_summary_birds_mammals[spp_summary_birds_mammals$id_no ==103823979,]
# merge with outputs
data_summary_aoh <- unique(merge(out2, spp_summary_birds_mammals, by = "id_no", all.x = T))
dim(data_summary_aoh)
dim(out2)



data_summary_aoh <- na.omit(data_summary_aoh, cols = c("suitable_num_pix", "total_num_pix"))
data.table::setkey(data_summary_aoh, id_no)
data_summary_aoh[ , count_id := .N, by = .(id_no)]
data_summary_aoh[, model_prevalence := round(suitable_num_pix/total_num_pix, 3)]
data_summary_aoh <- subset(data_summary_aoh, seasonality == "Breeding_and_Nonbreeding" | seasonality == "Resident")



#### read in results model prevalence AOH_habitat and AOH_elevation ####
source("rscripts/validation/src_files/read_model_prevalence_elevation_habitat_only.R")
names(aoh_ele_hab)
names(data_summary_aoh)
tmp <- merge(data_summary_aoh, aoh_ele_hab, by = c("id_no", "seasonality"), all = T)
dim(aoh_ele_hab)
dim(data_summary_aoh)
dim(tmp)

data_model_prevalence <- tmp
data_model_prevalence[is.na(elevation_from_iucn), elevation_range := NA]
data_model_prevalence[is.na(elevation_from_iucn), mid_point_elevation := NA]
rm(list=setdiff(ls(), "data_model_prevalence"))
