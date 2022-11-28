###### Read in results of model only elevation and only habitat #####

path_ele_m <- paste0("../aoh_out/output_R/mammals/lumbierres_LR_validation_elevation_only_", projection, "/")
path_ele_b <- paste0("../aoh_out/output_R/birds/lumbierres_LR_validation_elevation_only_", projection, "/")
fls <- c(list.files(path_ele_b, pattern = "area_py", full.names = T),
         list.files(path_ele_m, pattern = "area_py", full.names = T))
ele <- rbindlist(lapply(fls, fread), fill = T)
ele[, aoh_elevation_obs_mdl_prevalence := (suitable/(suitable + unsuitable)),]
setnames(ele, "suitable", "suitable_num_pix_elevation_only")
ele <- unique(ele[, c("name", "aoh_elevation_obs_mdl_prevalence", "suitable_num_pix_elevation_only")])
#data.table::setnames(ele, "type", "aoh_elevation_type")
path_hab_m <- paste0("../aoh_out/output_R/mammals/lumbierres_LR_validation_habitat_only_", projection, "/")
path_hab_b <- paste0("../aoh_out/output_R/birds/lumbierres_LR_validation_habitat_only_", projection, "/")
fls <- c(list.files(path_hab_b, pattern = "area_py", full.names = T),
         list.files(path_hab_m, pattern = "area_py", full.names = T))
hab <- unique(rbindlist(lapply(fls, fread), fill = T))
hab[, aoh_habitat_obs_mdl_prevalence := (suitable/(suitable + unsuitable)),]
setnames(hat, "suitable", "suitable_num_pix_habitat_only")
hab <- hab[, c("name", "aoh_habitat_obs_mdl_prevalence", "suitable_num_pix_habitat_only")]
#data.table::setnames(hab, "type", "aoh_habitat_type")

aoh_ele_hab <- merge(ele, hab, by = c("name"), all = T)
data.table::setnames(aoh_ele_hab, "name", "raster_name")
rm(ele, hab, fls, path_ele_b, path_ele_m, path_hab_b, path_hab_m)
