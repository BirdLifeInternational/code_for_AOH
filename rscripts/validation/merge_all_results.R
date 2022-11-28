# ultimate merge:
library(data.table)
source("rscripts/list_libraries.R")
pts <- fread("../aoh_out/output_R/validation/point_prevalence_versus_observed_model_prevalence_results.csv")
gen <- fread("../aoh_out/output_R/validation/data_summary_mdl_prevalence.csv")
predm <- fread("../aoh_out/output_R/validation/results_model_prevalence.csv")
predm[, raster_name := paste0(id_no, "_", seasonality, ".tif")]
predm <- predm[, -c("id_no", "seasonality")]
pts <- pts[, -"id_no"]
names(pts)
names(gen)
names(predm)

res1 <- unique(merge(unique(gen), unique(pts), by = "raster_name", all = T))
res2 <- unique(merge(unique(res1), unique(predm), by = "raster_name", all = T))

dim(res1)
dim(res2)
dim(gen)
dim(pts)
dim(predm)

fwrite(res2, "../aoh_out/output_R/validation/complete_summary_validation_aoh.csv")
res <- fread("../aoh_out/output_R/validation/complete_summary_validation_aoh.csv")
res <- res[!is.na(raster_name)]
res<- res[,c("raster_name" ,
             "id_no",
            "binomial","seasonality",                                                 
             "realm",                                                    
             "terrestrial",                                              
             "freshwater",                                               
             "marine",                                                   
              "order_name",                                               
              "family_name",                                              
              "elevation_from_iucn",                                      
             "category",                                                 
              "bird_or_mammal",                                           
             "elevation_lower",                                          
           "elevation_upper",                                          
           "occasional_lower_altitude",                                
             "occasional_upper_altitude",
           "min_elevation_range_map",                                 
           "max_elevation_range_map",                                  
           "min_elevation_GBif",                                       
           "max_elevation_GBif",                                       
           "median_elevation_GBif",
             
             "total_num_pix_seasonal_range_map",
           "obs_mdl_prevalence",
             "suitable_num_pix",                                         
             "not_suitable_num_pix",                                     
                                               
             "aoh_elevation_obs_mdl_prevalence",                         
             "suitable_num_pix_elevation_only",                          
             "aoh_habitat_obs_mdl_prevalence",
           
           "suitable_num_pix_habitat_only",                           
             "full_habitat_code","count_hab_full", "count_hab_used",
           
           "predicted_mdl_prevalence",                                 
           "mild_outliers",                                            
           "extreme_outliers",                                         
           "mdl_intqrt_thresh",                            
             "pts_prevalence",                                           
             "number_pts_suitable","total_number_pts","number_pts_outside_aoh","better_than_random_or_pts_prevalence_larger_md_prevalence"
 )]