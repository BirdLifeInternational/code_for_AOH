source("rscripts/validation/src_files/get_paths_pts_prevalence.R")


all_csv <- list.files(paste0("../aoh_out/output_R/", class, "/lumbierres_LR/"), pattern = ".csv$", full.names = TRUE)
all_csv <- str_replace(all_csv[str_detect(all_csv, "area_py.csv")], "\\/\\/", "\\/")
model_prevalence <- data.table(read.csv(all_csv))


model_prevalence[ , total_num_pix := (unsuitable + suitable),]
data.table::setnames(model_prevalence, "suitable", "suitable_num_pix")	
data.table::setnames(model_prevalence, "unsuitable", "unsuitable_num_pix")


all_csv <- list.files(paste0("../aoh_out/output_R/", class, "/lumbierres_LR_validation_elevation_only/"), pattern = ".csv$", full.names = TRUE)
all_csv <- str_replace(all_csv[str_detect(all_csv, "area_py.csv")], "\\/\\/", "\\/")
elevation <- data.table(read.csv(all_csv))
setnames(elevation, "suitable", "suitable_num_pix_elevation_only")
setnames(elevation, "unsuitable", "unsuitable_num_pix_elevation_only")

all_csv <- list.files(paste0("../aoh_out/output_R/", class, "/lumbierres_LR_validation_habitat_only/"), pattern = ".csv$", full.names = TRUE)
all_csv <- str_replace(all_csv[str_detect(all_csv, "area_py.csv")], "\\/\\/", "\\/")
habitat <- data.table(read.csv(all_csv))
setnames(habitat, "suitable", "suitable_num_pix_habitat_only")
setnames(habitat, "unsuitable", "unsuitable_num_pix_habitat_only")
part <- merge(elevation, habitat, all = T)


model_prevalence <- merge(part, model_prevalence, by = "name", all = T)



if(nrow(model_prevalence[str_detect(name, "_1.tif") | str_detect(name, "_2.tif") | str_detect(name, "_3.tif"),])>0){
  stop("Error: some file names contain the _1,_2,or _3 suffix.")
}
model_prevalence$id_no <- as.numeric(stringr::str_extract(model_prevalence$name, "\\d+"))

redl <- as.data.table(read.csv("../aoh_out/cache_dir/list_of_ids_names.csv"))
redl <- unique(redl[, c("id_no", "scientific_name", "order")])
redl[order == "mammal", order := "mammals"]
redl <- subset(redl, order == class)
model_prevalence <- merge(model_prevalence, redl, all = T)
setnames(model_prevalence, "name", "raster_name")
model_prevalence$season <- stringr::str_remove_all(model_prevalence$name, "\\d+")
model_prevalence$season <- stringr::str_remove_all(model_prevalence$season, "_")
model_prevalence$season <- stringr::str_remove_all(model_prevalence$season, ".tif")
model_prevalence[ , observed_md_prevalence := round((suitable_num_pix / total_num_pix), 3), ]
model_prevalence <- model_prevalence[, c("id_no", "scientific_name", "order", "season", 
                                         "raster_name",
                                         "observed_md_prevalence",
                                         "total_num_pix", "suitable_num_pix",
                                         "suitable_num_pix_elevation_only", "suitable_num_pix_habitat_only")]




##### merge with pts prevalence #####

point_prevalence <- fread(paste0(out_dir, class, "_", str_replace(type, "\\/", ""), "_point_prevalence_results.csv"))

names(point_prevalence)
names(model_prevalence)
r1 <- unique(point_prevalence$raster_name)
r2 <- unique(model_prevalence$raster_name)
if(!any(r1 %in% r2)) stop("Error: no matching names between files")
vali <- merge(model_prevalence, point_prevalence, all = T, by = "raster_name")
vali[ , better_than_random_or_pts_prevalence_larger_md_prevalence := +(pts_prevalence > observed_md_prevalence)]


unique(vali$bird_or_mammal)

vali <- vali[, c("raster_name", 
                 "id_no", "scientific_name", "order", "season", 
             
                 "observed_md_prevalence",
                 "total_num_pix", "suitable_num_pix",
                 "suitable_num_pix_elevation_only", "suitable_num_pix_habitat_only",
                 "pts_prevalence", 
                 "number_pts_suitable",
                 "total_number_pts", "number_pts_outside_aoh",
                 "better_than_random_or_pts_prevalence_larger_md_prevalence")]
type <- str_remove_all(type, "\\/")
fwrite(vali, file= paste0(out_dir, class, "_", type, "_point_prevalence_versus_observed_model_prevalence_results.csv"))

unlink(out_tmp_rast, recursive = TRUE)


