# merge pts prevalence results
 FIX and check for typos! Then need to combine with observed model prevalence!
source("rscripts/validation/src_files/get_paths_pts_prevalence.R")
##### merge with pts prevalence #####
lf <- c("../aoh_out/output_R/validation/point_prevalence/mammals_lumbierres_LR_point_prevalence_results.csv",
        "../aoh_out/output_R/validation/point_prevalence/birds_lumbierres_LR_point_prevalence_results.csv")

point_prevalence <- rbindlist(lapply(lf, fread), fill = TRUE)
point_prevalence[, raster_name := str_remove_all(raster_name, "../aoh_out/output_R/birds/lumbierres_LR/")]
point_prevalence[, raster_name := str_remove_all(raster_name, "../aoh_out/output_R/mammals/lumbierres_LR/")]

point_prevalence$raster_name
model_prevalence <- fread("../aoh_out/output_R/validation/data_summary_mdl_prevalence.csv")              
             
names(point_prevalence)
names(model_prevalence)
r1 <- unique(point_prevalence$raster_name)
r2 <- unique(model_prevalence$raster_name)
if(!any(r1 %in% r2)) stop("Error: no matching names between files")
vali <- merge(model_prevalence, point_prevalence, all = T, by = "raster_name")
vali[ , better_than_random_or_pts_prevalence_larger_md_prevalence := +(pts_prevalence > obs_mdl_prevalence)]


unique(vali$bird_or_mammal)

vali <- vali[, c("raster_name", 
                 "id_no", 
                 "pts_prevalence", 
                 "number_pts_suitable",
                 "total_number_pts", "number_pts_outside_aoh",
                 "better_than_random_or_pts_prevalence_larger_md_prevalence")]
type <- str_remove_all(type, "\\/")
fwrite(vali, file= "../aoh_out/output_R/validation/point_prevalence_versus_observed_model_prevalence_results.csv")

unlink(out_tmp_rast, recursive = TRUE)


