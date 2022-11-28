rm(list = ls())
process_birds <- TRUE # set to false to process mammals
type <- "LR" # FRC stands for fractional; LR stands for low-resolution
# If doing model validation set the following parameters to TRUE
model_validation <- FALSE
elevation_only <- FALSE
habitat_only <- FALSE
###### load libraries #####
source("rscripts/list_libraries.R")
library(foreach)

source("rscripts/aoh_jeff_4wcmc/src_files/set_directories.R")
path_lookup <- "../IUCN_data/NOT_to_be_committed/output_gpkg/birds/"



all_gpkg <- list.files(path_lookup, pattern = ".gpkg$", full.names = T)
ordered_gpkg <- file.info(all_gpkg)
ordered_gpkg$file_name <- str_replace(row.names(ordered_gpkg), "\\/\\/", "\\/")
#ordered_gpkg <- ordered_gpkg[order(ordered_gpkg$size), ]
fls <- ordered_gpkg$file_name
res_dir <- "../aoh_out/output_R/birds/elevation_range"
if(!dir.exists(res_dir)) dir.create(res_dir)
i = 1
nthreads = 5
doParallel::registerDoParallel(nthreads)
foreach(i = 1:length(fls), .errorhandling = "pass") %dopar% {

  taxon_id <- str_remove_all(fls[i], path_lookup)
  taxon_id <- str_remove_all(taxon_id, ".gpkg")
  taxon_id <- str_remove_all(taxon_id, "id_no_")
  
  if(file.exists(paste0(res_dir, "/", taxon_id, ".csv")) & file.size(paste0(res_dir, "/", taxon_id, ".csv")) > 0){
    print(paste0(taxon_id, " already processed "))
  } else {
    spp <- fls[i]
    spp_range_data <- read_sf(spp)
    st_geometry(spp_range_data) <- "geometry"
    spp_range_data <- st_transform(spp_range_data, crs(elevation_data))
    spp_ext <- sf::st_bbox(spp_range_data)
    xmin = spp_ext["xmin"]
    ymin = spp_ext["ymin"]
    xmax = spp_ext["xmax"]
    ymax = spp_ext["ymax"]
    # terra::ext order= xmin, xmax, ymin, ymax)
    cropped <- terra::crop(elevation_data, ext(xmin, xmax, ymin, ymax))
    if(hasMinMax(cropped)){
      mm <- minmax(cropped)
      min_ele <- mm["min",]
      max_ele <- mm["max",] 
    } else {
      min_ele <- NA
      max_ele <- NA 
    }
  
    res <- data.table(id_no = taxon_id, min_elevation_range_map = min_ele, max_elevation_range_map = max_ele)
    fwrite(res, file = paste0(res_dir, "/", taxon_id, ".csv"))
    print(paste0("index = ", i, " for ", taxon_id, " done"))
  }
}
doParallel::stopImplicitCluster()
print("loop done")

lf = list.files(res_dir, full.names = TRUE)
out <- rbindlist(lapply(lf, fread), fill = TRUE)
fwrite(out, "../aoh_out/output_R/validation/summary_elevation_range_maps.csv")
print("done")
print("done")
unlink(paste0(normalizePath(tempdir()), "/"), recursive = TRUE)
