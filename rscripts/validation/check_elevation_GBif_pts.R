#1. Loading the point localities: 
rm(list=ls())
library(foreach)
nthreads = 5
source("rscripts/list_libraries.R")
#source("rscripts/validation/pre_processing_validation/write_vect_point_localities.R")
class <- "birds"
type = "lumbierres_LR"
res_loc <- "../aoh_out/output_R/elevation_gbif"
projection <- "esri54017"
if(!dir.exists(res_loc)) dir.create(res_loc)
source("rscripts/validation/src_files/get_paths_pts_prevalence.R")

#### used projections crs ####
wgs_84_crs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
source("rscripts/aoh_jeff_4wcmc/src_files/wb_crs.R")

source("rscripts/validation/src_files/read_ebird_gbif.R")
elevation_data <- terra::rast(paste0("../aoh_out/cache_dir/10-5281_zenodo-5719984/dem-100m-esri54017.tif"))
points <- subset(points, class == "bird")

ids <- unique(points$id_no)
i= 1
res <- foreach(i = 1:length(ids), .errorhandling = "pass") %do% {
	taxon_id <- ids[i]

  spp_points <- subset(points, id_no == taxon_id)
  info <- as.character(unique(spp_points$binomial))
      spp_points <- sf::st_as_sf(spp_points, coords = c("LONGITUDE", "LATITUDE"),
                               crs = wgs_84_crs)
    spp_points <- st_transform(spp_points, crs = wb_crs)
    
    vsp <- as(spp_points, "Spatial")
		spp_points <- vect(vsp)

	res <- terra::extract(elevation_data, spp_points)
	med <- median(res[, "dem-100m-esri54017"], na.rm = TRUE)
	min_max <- range(res[, "dem-100m-esri54017"], na.rm = TRUE)

	summ <- data.table::data.table(binomial = info, id_no = taxon_id, 
		min_elevation_GBif = min(min_max), max_elevation_GBif = max(min_max),
		median_elevation_GBif = med)
	#print(summ)
	print(paste0(res_loc, "/", taxon_id, ".csv"))
	fwrite(summ, paste0(res_loc, "/", taxon_id, ".csv"))
}

lf = list.files(res_loc, full.names = TRUE)
out <- rbindlist(lapply(lf, fread), fill = TRUE)

fwrite(out, "../aoh_out/output_R/validation/summary_elevation_point_gbif.csv")
print("done")

check_string_fun <- function(x) {
  if(any(is.null(x))) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}
to_keep <- lapply(res, check_string_fun)
to_keep <- which(to_keep == TRUE)
res <- res[to_keep]

print(res)

unlink(paste0(normalizePath(tempdir()), "/"), recursive = TRUE)
