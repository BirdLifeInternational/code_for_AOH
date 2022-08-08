###### Read in results of model only elevation and only habitat #####

path_ele_m <- "../aoh_out/output_R/mammals/lumbierres_LR_validation_elevation_only/area/"
path_ele_b <- "../aoh_out/output_R/birds/lumbierres_LR_validation_elevation_only/area/"
fls <- c(list.files(path_ele_b, pattern = ".csv$", full.names = T),
         list.files(path_ele_m, pattern = ".csv$", full.names = T))
ele <- rbindlist(lapply(fls, fread), fill = T)
ele[, aoh_elevation_mdl_prevalence := (suitable_num_pix/total_num_pix),]
ele <- ele[, c("id_no", "type", "aoh_elevation_mdl_prevalence")]
#data.table::setnames(ele, "type", "aoh_elevation_type")
path_hab_m <- "../aoh_out/output_R/mammals/lumbierres_LR_validation_habitat_only/area/"
path_hab_b <- "../aoh_out/output_R/birds/lumbierres_LR_validation_habitat_only/area/"
fls <- c(list.files(path_hab_b, pattern = ".csv$", full.names = T),
         list.files(path_hab_m, pattern = ".csv$", full.names = T))
hab <- rbindlist(lapply(fls, fread), fill = T)
hab[, aoh_habitat_mdl_prevalence := (suitable_num_pix/total_num_pix),]
hab <- hab[, c("id_no", "type", "aoh_habitat_mdl_prevalence")]
#data.table::setnames(hab, "type", "aoh_habitat_type")

aoh_ele_hab <- merge(ele, hab, by = c("id_no", "type"), all = T)
data.table::setnames(aoh_ele_hab, "type", "seasonality")
rm(ele, hab, fls, path_ele_b, path_ele_m, path_hab_b, path_hab_m)
