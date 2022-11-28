cache_dir <- "../aoh_out/cache_dir/"


###### collate info processed by aoh package summarising the analysed data #####
lf_df_b <- list.files(paste0("../aoh_out/output_R/birds/lumbierres_LR_", projection, "/df"), pattern = ".csv$", full.names = T)
lf_df_m <- list.files(paste0("../aoh_out/output_R/mammals/lumbierres_LR_", projection, "/df"), pattern = ".csv$", full.names = T)
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
val_dir <- "../aoh_out/output_R/validation/"
if(!dir.exists(val_dir)) dir.create(val_dir)
fwrite(sp_not_all_hab_used, paste0(val_dir, "aoh_where_some_habitat_codes_where_not_found.csv"))

# here we count the number of habitat in the IUCN per species (all seasons added)
# and also the number of habitats actually used

names(spp_proc)
spp_proc <- unique(spp_proc[,c("id_no", "full_habitat_code","habitat_code", 
                               "count_hab_full", "count_hab_used")])



fwrite(spp_proc, file = paste0(tmp_res_val, "/spp_habitat_codings.csv"))
