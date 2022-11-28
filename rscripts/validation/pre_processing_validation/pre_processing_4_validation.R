rm(list=ls())
source("rscripts/list_libraries.R")
cache_dir <- "../aoh_out/cache_dir/"
tmp_res_val <- "../aoh_out/output_R/validation/tmp_res_validation"
if(!dir.exists(tmp_res_val)) dir.create(tmp_res_val)
source("rscripts/validation/src_files/read_model_prevalence.R")
source("rscripts/validation/src_files/read_compare_habitat_coding.R")
source("rscripts/validation/src_files/read_elevation_stuff.R")

rm(list = ls())
cache_dir <- "../aoh_out/cache_dir/"
tmp_res_val <- "../aoh_out/output_R/validation/tmp_res_validation"


# read output aoh summaries

obs_mdl_prevalence = fread(paste0(tmp_res_val, "/observed_mdl_prevalence.csv"))
hab_coding <- fread(paste0(tmp_res_val, "/spp_habitat_codings.csv"))
elevations <- fread("../aoh_out/output_R/validation/summary_elevations_all.csv")

any(is.na(obs_mdl_prevalence$id_no))
any(is.na(hab_coding$id_no))
any(is.na(elevations$id_no))
##### merge the output info ########
out <- unique(merge(unique(obs_mdl_prevalence), unique(hab_coding), all= T, by = "id_no"))
data_summary_aoh <- unique(merge(unique(out), unique(elevations), by = "id_no", all = T))
dim(obs_mdl_prevalence)
dim(hab_coding)
dim(elevations)
dim(out)
dim(data_summary_aoh)

data.table::setkey(data_summary_aoh, id_no)
data_summary_aoh[ , count_id := .N, by = .(id_no)]

#data_summary_aoh <- subset(data_summary_aoh, seasonality == "Breeding_and_Nonbreeding" | seasonality == "Resident")
unique(data_summary_aoh[bird_or_mammal == "", "order_name"])



data_summary_aoh[str_detect(raster_name, "Breeding")]
fwrite(data_summary_aoh, file = "../aoh_out/output_R/validation/data_summary_mdl_prevalence.csv")
rm(list=setdiff(ls(), "data_summary_aoh"))
