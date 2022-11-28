###### Read in results of model only elevation and only habitat #####

projection = "esri54017"

#### collate information about number of pixels suitable and not ##########
lf_b <- list.files(paste0("../aoh_out/output_R/birds/lumbierres_LR_", projection), pattern = "area_py", full.names = T)
lf_b = lf_b[1]
lf_m <- list.files(paste0("../aoh_out/output_R/mammals/lumbierres_LR_", projection), pattern = "area_py", full.names = T)
lf <- c(lf_b, lf_m)
print("Reading files..")
print(lf)
out <- rbindlist(lapply(lf, fread), fill = TRUE)
out <- subset(out, name != "name")
splitted <- str_split_fixed(out$name, "_", n = 2)
out$id_no <- as.numeric(splitted[, 1])
out$seasonality <- str_remove_all(splitted[, 2], ".tif")

# Seasonality should be Resident, Breeding and non-breeding
# so subset and delete the Breeding_and_Non-breeding ones!
unique(out$seasonality)

names(out)
setnames(out, "name", "raster_name")
if(!is.numeric(out$suitable)) out[, suitable := as.numeric(suitable),]
if(!is.numeric(out$unsuitable)) out[, unsuitable := as.numeric(unsuitable),]
out[, total_num_pix := (suitable + unsuitable), ]
setnames(out, "suitable", "suitable_num_pix")
setnames(out, "unsuitable", "not_suitable_num_pix")
setnames(out, "total_num_pix", "total_num_pix_seasonal_range_map")
out <- unique(out[, c("id_no", "raster_name", "total_num_pix_seasonal_range_map", "suitable_num_pix", "not_suitable_num_pix", "seasonality")])
out[, obs_mdl_prevalence := round((suitable_num_pix / total_num_pix_seasonal_range_map), 3)]
###########3
path_ele_m <- paste0("../aoh_out/output_R/mammals/lumbierres_LR_validation_elevation_only_", projection, "/")
path_ele_b <- paste0("../aoh_out/output_R/birds/lumbierres_LR_validation_elevation_only_", projection, "/")
fls <- c(list.files(path_ele_b, pattern = "area_py", full.names = T),
         list.files(path_ele_m, pattern = "area_py", full.names = T))
ele <- rbindlist(lapply(fls, fread), fill = T)
ele[, aoh_elevation_obs_mdl_prevalence := round((suitable/(suitable + unsuitable)), 3), ]
setnames(ele, "suitable", "suitable_num_pix_elevation_only")
ele <- unique(ele[, c("name", "aoh_elevation_obs_mdl_prevalence", "suitable_num_pix_elevation_only")])
#data.table::setnames(ele, "type", "aoh_elevation_type")
path_hab_m <- paste0("../aoh_out/output_R/mammals/lumbierres_LR_validation_habitat_only_", projection, "/")
path_hab_b <- paste0("../aoh_out/output_R/birds/lumbierres_LR_validation_habitat_only_", projection, "/")
fls <- c(list.files(path_hab_b, pattern = "area_py", full.names = T),
         list.files(path_hab_m, pattern = "area_py", full.names = T))
hab <- unique(rbindlist(lapply(fls, fread), fill = T))
hab[, aoh_habitat_obs_mdl_prevalence := round((suitable/(suitable + unsuitable)), 3), ]
setnames(hab, "suitable", "suitable_num_pix_habitat_only")
hab <- hab[, c("name", "aoh_habitat_obs_mdl_prevalence", "suitable_num_pix_habitat_only")]
#data.table::setnames(hab, "type", "aoh_habitat_type")

aoh_ele_hab <- merge(ele, hab, by = c("name"), all = T)
data.table::setnames(aoh_ele_hab, "name", "raster_name")
rm(ele, hab, fls, path_ele_b, path_ele_m, path_hab_b, path_hab_m)

aoh_area <- merge(out, aoh_ele_hab, by = "raster_name", all = T)
aoh_area[is.na(id_no), id_no := as.numeric(str_extract_all(raster_name, "\\d+")),]
rm(out, aoh_ele_hab)
fwrite(aoh_area, file = paste0(tmp_res_val, "/observed_mdl_prevalence.csv"))