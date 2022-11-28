#' Script to process the point validation and save to csv
#' 
#' 
source("rscripts/validation/src_files/get_paths_pts_prevalence.R")

#### used projections crs ####
wgs_84_crs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
source("rscripts/aoh_jeff_4wcmc/src_files/wb_crs.R")
######### check list of rasters ##########
r1 <- list.files(ras_loc, pattern = "Resident.tif$", full.names = T)
r2 <- list.files(ras_loc, pattern = "Breeding_and_Nonbreeding.tif$", full.names = T)
all_ras <- c(r1, r2); 
ras_ids <- str_remove(all_ras, ras_loc)
ras_ids <- str_remove(ras_ids, "_Resident.tif")
ras_ids <- str_remove(ras_ids, "_Breeding_and_Nonbreeding.tif")
ras_ids <- as.numeric(unique(str_remove(ras_ids, "/")))
rm(r1, r2, all_ras)
##### load point localities #####
source("rscripts/validation/src_files/read_ebird_gbif.R")
any(ras_ids %in% points$id_no)
ids_to_process <- unique(ras_ids)
pts_ids <- unique(points$id_no)
#  ids_to_process <- unique(ras_ids[ras_ids %in% points$id_no])
rm(ras_ids)
i = 11
# get grid from land-cover
grid_ras <- raster::raster(template_ras) 
extent_gr <- terra::ext(grid_ras)
res_rg <- terra::res(grid_ras)
rm(grid_ras)
all_x_s <- seq(extent_gr[1], extent_gr[2], by = res_rg[1])
all_y_s <- seq(extent_gr[3], extent_gr[4], by = res_rg[2])
# exclude all files already processed from the loop
'%!in%' <- function(x,y)!('%in%'(x,y))
pat <- "(\\d)+"
lf <- list.files(out_tmp_csv, pattern = "\\.csv$", full.names = TRUE)
lf <- str_replace_all(lf[file.size(lf) > 0], "\\/\\/", "\\/")
lf <- str_remove_all(lf, out_tmp_csv)
lf <- str_remove_all(lf, "\\.csv")
already_processed_ids <- sort(as.numeric(stringr::str_extract(lf, pat)))

if(length(already_processed_ids) > 0){
	still_to_process <- sort(ids_to_process[ids_to_process %!in% already_processed_ids])
} else {
	still_to_process <- sort(ids_to_process)
}
lps <- length(still_to_process)
print(paste0("Files to process : ", lps))
print(lps)
if(lps == 0) (stop("The number of species to process is zero. Did you already submit this scrip earlier? Are the paths correct?"))
# to install gdal
#doParallel::registerDoParallel(nthreads)
# one thread version for(i in 1:length(ids_to_process)){
# on multiple cores
# library(reticulate)
#nam_c <- paste0("r-osgeo")
#conda_create(nam_c, packages = "gdal")
pts_loop <- foreach (i = 1:lps#, #.errorhandling = 'pass',
	) %do% {

  #print(paste("lps = ", lps))
  taxon_id = still_to_process[i]

  res <- paste0(ras_loc, "/", taxon_id, "_Resident.tif")
  both <- paste0(ras_loc, "/", taxon_id, "_Breeding_and_Nonbreeding.tif")
  if(file.exists(res)) { type = "Resident"; path_ras <- res; print(paste0("i = ", i, " file ", path_ras))
  } else if(file.exists(both)){ type = "Breeding_and_Nonbreeding"; path_ras <- both; 
  	print(paste0("i = ", i, " file ", path_ras))
  } else {print("no aoh raster found")}
  output_file_nam <- paste0(out_tmp_csv, "/", taxon_id, ".csv")
  ##### point localities ##########
  # subset data point localities per taxon_id
  # then reformat points by adding a buffer of 300m, and change CRS
  spp_points <- subset(points, id_no == taxon_id)
  spp_points <- spp_points[, c("LONGITUDE", "LATITUDE")]
  spp_points$values <- 1
  ddims <- nrow(spp_points)
  #print(paste(i, ", ", ddims))
  print(paste0("Processing ", taxon_id, "..."))
  
  
  if(ddims < 50){
    # exclude species with less than 50 DATA POINTS!
    tmp <- data.table(id_no = taxon_id, seasonality = type,
                      pts_unsuitable = NA, pts_suitable = NA, 
                      pts_available = "no", raster_name = path_ras, 
                      total_number_pts = ddims,
                      number_pts_outside_aoh = NA)
    fwrite(tmp, file = output_file_nam)
    print("no data points")
  } else if((ddims >= 50 & !file.exists(output_file_nam)) |
            (ddims >= 50 & file.exists(output_file_nam) & (file.size(output_file_nam) == 0))){
    if(file.exists(output_file_nam)) unlink(output_file_nam, recursive = TRUE)
    # Process the species with 50+ data points
    if(file.exists(res)) {ras_missing <- 0; path_aoh = res; aoh_ext <- terra::ext(terra::rast(res, lyrs = 1)); 
    } else if(file.exists(both)){ ras_missing <- 0; path_aoh = both; aoh_ext <- terra::ext(terra::rast(both, lyrs = 1)); 
    } else { ras_missing <- 1; print("no aoh raster found") }
    
    if(ras_missing == 1){
      # tmp <- data.table(id_no = taxon_id, seasonality = NA,
      #                   pts_unsuitable = NA, pts_suitable = NA, 
      #                   pts_available = NA, raster_name = NA,
      #                   total_number_pts = NA,
      #                   number_pts_outside_aoh = NA)
      # fwrite(tmp, file = paste0(out_tmp_csv, taxon_id, ".csv"))
      stop("No raster, skip to next")
    }
    
    aoh_xmin <- as.numeric(aoh_ext[1])
    aoh_xmax <- as.numeric(aoh_ext[2])
    aoh_ymin <- as.numeric(aoh_ext[3])
    aoh_ymax <- as.numeric(aoh_ext[4])
    spp_points <- sf::st_as_sf(spp_points, coords = c("LONGITUDE", "LATITUDE"),
                               crs = wgs_84_crs)
    spp_points <- sf::st_transform(spp_points, crs = wb_crs)
    spp_points <- sf::st_crop(sf::st_geometry(spp_points), 
                                   xmin = aoh_xmin,
                                        xmax = aoh_xmax,
                                        ymin = aoh_ymin,
                                        ymax = aoh_ymax)
    num_pts_outside_extent_aoh = ddims - length(spp_points)
    buffered <- sf::st_buffer(spp_points, 300)

    shp_nam <- paste0(out_tmp_shp, "/", taxon_id, ".shp")
    if(file.exists(shp_nam)) unlink(shp_nam)
    sf::st_write(buffered, dsn = shp_nam, append = FALSE)
    tmp_ras <- paste0(out_tmp_buffered, "/", taxon_id, ".tif")
    info_shp_ras <- paste0(out_tmp_buffered, "/", taxon_id, "_point_cells.tif")

    if(file.exists(tmp_ras)) unlink(tmp_ras)
    gdalUtilities::gdalwarp(ot = "Byte", co = "COMPRESS=LZW",
                            srcfile = path_aoh,
                            dstfile = tmp_ras,
                            cutline = shp_nam, 
                            crop_to_cutline = TRUE,
                            overwrite = TRUE, dryrun = FALSE
                            )
    
    
    # rasterize the polygons of buffered points
    # within the loop
    # box <- sf::st_bbox(buffered)
    # xmin <- box["xmin"]
    # xmax <- box["xmax"]
    # ymin <- box["ymin"]
    # ymax <- box["ymax"]
    # b_xmin <- max(all_x_s[all_x_s < xmin])
    # b_xmax <- min(all_x_s[all_x_s > xmax])
    # b_ymin <- max(all_y_s[all_y_s < ymin])
    # b_ymax <- min(all_y_s[all_y_s > ymax])
    # gdalUtilities::gdal_rasterize(src_datasource = shp_nam,
    #                               dst_filename = info_shp_ras,
    #             te = c(b_xmin, b_ymin, b_xmax, b_ymax),
    #             tr = res_rg, a_nodata = "NA", 
    #             ot = "Int8", burn = 1)
    # pts_tot <- freq(rast(info_shp_ras))
    rm(buffered, spp_points)
    print("start calculating freq")
    res <- frequency_gdalinfo_raster(tmp_ras)
    print("freq calculated")
    # clean-up
    if(file.exists(shp_nam)) unlink(shp_nam, recursive = TRUE)
    #if(file.exists(info_shp_ras)) unlink(info_shp_ras, recursive = TRUE)
    if(file.exists(tmp_ras)) unlink(tmp_ras, recursive = TRUE)
    
    print(paste0(taxon_id, " frequency done"))

    unsu <- as.numeric(res[code_values == 0, "frequency"])
    su <- as.numeric(res[code_values == 1, "frequency"])
    ras_nam <- stringr::str_remove(path_ras, paste0(ras_loc, "/"))
    tmp <- data.table(id_no = taxon_id, seasonality = type,
                      pts_unsuitable = unsu, pts_suitable = su, 
                      pts_available = "yes", raster_name = ras_nam, 
                      total_number_pts = ddims,
                      number_inside_points = (ddims - num_pts_outside_extent_aoh),
                      number_pts_outside_aoh = num_pts_outside_extent_aoh)
    if(file.exists(output_file_nam)) unlink(output_file_nam, recursive = TRUE)
    fwrite(tmp, file = output_file_nam)
    rm(su, unsu, tmp, res, both, aoh_ext)
    
  }
  print(paste0("Processed ", taxon_id))
}
  

#doParallel::stopImplicitCluster()
print(traceback())
print("parallel cluster closed")
save(pts_loop,  file = paste0(out_dir, "/", class, "_", str_replace(type, "\\/", ""), "_pts_loop.RData"))
########## read in results of point and model prevalence ############

all_csv <- list.files(out_tmp_csv, pattern = ".csv$", full.names = TRUE)
point_prevalence <- rbindlist(lapply(all_csv, fread), fill = TRUE)
point_prevalence[, pts_tot := (pts_unsuitable + pts_suitable), ]
point_prevalence[, pts_prevalence := round((pts_suitable/pts_tot), 3), ]
setnames(point_prevalence, "pts_suitable", "number_pts_suitable")
# count number of species processes
table(point_prevalence$pts_available)
point_prevalence <- unique(point_prevalence[, c("raster_name", "pts_prevalence", 
                                                "number_pts_suitable",
                                                "total_number_pts", "number_pts_outside_aoh")])

fwrite(point_prevalence, 
	file = paste0(out_dir, "/", class, "_", projection, "_point_prevalence_results.csv"))





