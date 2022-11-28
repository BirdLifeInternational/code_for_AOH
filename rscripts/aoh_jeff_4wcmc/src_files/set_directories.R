#' Set directory and load files
if(.Platform$OS.type == "unix") {
  machine <- "linux" # or "windows"
  eng <- "gdal"
} else {
  machine <- "windows"
  eng <- "gdal"
}

#### st_write requires a driver ####
drivers <- st_drivers()
if(drivers[row.names(drivers) == "GPKG", "write"] == TRUE){
  driver <- "GPKG"
} else {
  stop("gpkg driver not available")
}
####### Specify already downloaded rasters of habitat, elevation + crosswalk combination ######
# specify and create cache directory if needed
cache_dir <- "../aoh_out/cache_dir/"
cache_dir <- paste0(R.utils::getAbsolutePath(cache_dir), "/")
# specify and create output directory

if(process_birds){
  baseline <- paste0("../aoh_out/output_R/birds/") 
} else {
  baseline <- paste0("../aoh_out/output_R/mammals/")  
}
baseline <- paste0(R.utils::getAbsolutePath(baseline), "/")
type <- paste0(type, "_", projection)
output_dir <- paste0(baseline, "lumbierres_", type, "/")
output_area <- paste0(baseline, "lumbierres_", type, "/area/")
output_shp <- paste0(baseline, "lumbierres_", type, "/shp/")
output_df <- paste0(baseline, "lumbierres_", type, "/df/")
output_errors <- paste0(baseline, "lumbierres_", type, "/errors/")
output_aoh_tif <- paste0(baseline, "lumbierres_", type, "/aoh_tif/")
combination <- "lumbierres"

if (!file.exists(cache_dir)) {
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
}
if(!file.exists("../aoh_out/output_R")) dir.create("../aoh_out/output_R", showWarnings = FALSE, recursive = TRUE)

if (!file.exists(baseline)) {
  dir.create(baseline, showWarnings = FALSE, recursive = TRUE)
}
if (!file.exists(output_dir)) {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
}
if (!file.exists(output_shp)) {
  dir.create(output_shp, showWarnings = FALSE, recursive = TRUE)
}
if (!file.exists(output_df)) {
  dir.create(output_df, showWarnings = FALSE, recursive = TRUE)
}
if (!file.exists(output_aoh_tif)) {
  dir.create(output_aoh_tif, showWarnings = FALSE, recursive = TRUE)
}

if (!file.exists(output_errors)) {
  dir.create(output_errors, showWarnings = FALSE, recursive = TRUE)
}
if(str_detect(type, "LR")){
  if (!file.exists(output_area)) {
    dir.create(output_area, showWarnings = FALSE, recursive = TRUE)
  }
}
#  Load elevation and habitat rasters
# these can be downloaded with zen4R from 
# https://zenodo.org/record/6622064#.YrrI-NJBxEY
# https://zenodo.org/record/6622149#.YrrI9dJBxEY
if(projection == "esri54017"){ # World_Behrmann
	elevation_data <- terra::rast(paste0(cache_dir, "10-5281_zenodo-5719984/dem-100m-esri54017.tif"))
	path_habitat_tif <- normalizePath(paste0(cache_dir, "lumbierres_v2.0/lumbierres-10-5281_zenodo-5146073-v2.tif"))
	pr_crs <- "ESRI:54017"
} else if(projection == "epsg4326"){ # WGS84
	elevation_data <- terra::rast(paste0(cache_dir, "10-5281_zenodo-5719984/dem-100m-EPSG4326.tif"))
	path_habitat_tif <- normalizePath(paste0(cache_dir, "lumbierres_v2.0/habitat_CGLS.tiff"))

	pr_crs <- "EPSG:4326"
} else if(projection == "esri54009"){ # Glob. Mollweide
	elevation_data <- terra::rast(paste0(cache_dir, "10-5281_zenodo-5719984/dem-100m-esri54009.tif"))
	path_habitat_tif <- normalizePath(paste0(cache_dir, "esacci/esacci-2015-300m-esri54009.tif"))
	pr_crs <- "ESRI:54009"
}
habitat_data <- terra::rast(path_habitat_tif)
# if compareGeom returns TRUE the elevation and habitat data have the same spatial properties
if(!compareGeom(elevation_data, habitat_data)) stop("Error: elevation and habitat raster are not comparable!")
###### Choose crosswalk ######
crosswalk_data <- crosswalk_lumb_cgls_data

if(str_detect(paste0(output_dir), "_FRC")){
  updated_path <- paste0(output_dir, "FRC_")
} else {
  updated_path <- paste0(output_dir)
}
updated_path <- str_replace_all(updated_path, "\\/\\/", "\\/")
