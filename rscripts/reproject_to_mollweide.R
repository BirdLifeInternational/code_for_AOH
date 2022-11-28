#library(gdalUtils)
library(gdalUtilities) 
original_path <- normalizePath("../aoh_out/cache_dir/esacci/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7-wgs84.tif")	
new_path <- paste0(normalizePath("../aoh_out/cache_dir/esacci/"), "/esacci-2015-300m-esri54009.tif")


src_dataset <- file.path(original_path)
out_tif <- file.path(new_path)

# gdalwarp -t_srs '+proj=utm +zone=11 +datum=WGS84' raw_spot.tif utm11.tif
#srcfile, dstfile, ..., s_srs, t_srs
gdalUtilities::gdalwarp(
	srcfile = src_dataset, 
	dstfile = out_tif,
	s_srs = "EPSG:4326",
	t_srs = "ESRI:54009", 
	
	overwrite = TRUE)
 
print(warnings())  

library(terra)
x = rast(out_tif)
print(x)
# rm(list=ls())
# print("terra")
# 
# src_dataset <- file.path(original_path)
# 
# ter <- terra::project(src_dataset, crs = "ESRI:54009", filename = "../aoh_out/cache_dir/10-5281_zenodo-5719984/dem-100m-esri_terra_54009.tif")


print("done")
