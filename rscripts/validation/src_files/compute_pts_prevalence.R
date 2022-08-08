#' Script to process the point validation and save to csv
#' 
#' 
#### lookup paths #####
model_prev_loc <- paste0("../aoh_out/output_R/", class, "/lumbierres_LR/area/")
ras_loc <- paste0("../aoh_out/output_R/", class, "/lumbierres_LR/")
### set output directories######
out_dir <- "../aoh_out/output_R/validation/point_prevalence/"
out_class <- paste0(out_dir, class, "/")
out_tmp_rast <- paste0(out_class, "tmp_rasters_points/")
if(!dir.exists("../aoh_out/output_R/validation/")) dir.create("../aoh_out/output_R/validation/")
if(!dir.exists(out_dir)) dir.create(out_dir)
if(!dir.exists(out_class)) dir.create(out_class)
if(!dir.exists(out_tmp_rast)) dir.create(out_tmp_rast)
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
#  ids_to_process <- unique(ras_ids[ras_ids %in% points$id_no])
rm(ras_ids)
i = 1



doParallel::registerDoParallel(nthreads)
# one thread version for(i in 1:length(ids_to_process)){
# on multiple cores
foreach (i = 1:length(ids_to_process), .errorhandling = 'pass') %dopar% {
  taxon_id = ids_to_process[i]
  res <- paste0(ras_loc, taxon_id, "_Resident.tif")
  both <- paste0(ras_loc, taxon_id, "_Breeding_and_Nonbreeding.tif")
  if(file.exists(res)) { type = "Resident"; path_ras <- res
  } else if(file.exists(both)){ type = "Breeding_and_Nonbreeding"; path_ras <- both
  } else {print("no aoh raster found")}
  
  ##### point localities ##########
  # subset data point localities per taxon_id
  # then reformat points by adding a buffer of 300m, and change CRS
  spp_points <- subset(points, id_no == taxon_id)
  ddims <- nrow(spp_points)
  
  if(ddims < 50){
    # exclude species with less than 50 DATA POINTS!
    tmp <- data.table(id_no = taxon_id, seasonality = type,
                      pts_unsuitable = NA, pts_suitable = NA, 
                      pts_available = "no", raster_name = path_ras)
    fwrite(tmp, file = paste0(out_tmp_rast, taxon_id, ".csv"))
  } else {
    # Process the species with 50+ data points
    spp_points <- sf::st_as_sf(spp_points, coords = c("LONGITUDE", "LATITUDE"),
                               crs = wgs_84_crs)
    spp_points <- st_transform(spp_points, crs = wb_crs)
    # buffer
    buffered <- st_buffer(spp_points, 300)
    vsp <- as(buffered, "Spatial")
    buffered <- vect(vsp)
    rm(spp_points, vsp)
    
    if(file.exists(res)) { aoh <- rast(res)
    } else if(file.exists(both)){ aoh <- rast(both)
    } else {
      print("no aoh raster found")
    }
    
    if(crs(aoh, proj = T) != crs(buffered, proj=T)){
      print(paste0("Non corresponding crs!"));
    }
    # remove all buffered points outside the range map 
    cropped <- terra::crop(buffered, aoh)
    # mask the aoh on the points
    masked <- terra::mask(aoh, cropped, touches = TRUE)
    # ext(aoh)
    # ext(spp_points)
    # ext(masked)
    frq <- freq(masked)
    unsu <- subset(frq, value == 0)$count
    su <- subset(frq, value == 1)$count
    tmp <- data.table(id_no = taxon_id, seasonality = type,
                      pts_unsuitable = unsu, pts_suitable = su, 
                      pts_available = "yes", raster_name = path_ras)
    fwrite(tmp, file = paste0(out_tmp_rast, taxon_id, ".csv"))
    rm(cropped, masked, su, unsu, tmp, res, both, aoh)
  }
  print(paste0("Processed ", taxon_id))
}

doParallel::stopImplicitCluster()

########## read in results of point and model prevalence ############

all_csv <- list.files(out_tmp_rast, pattern = ".csv$", full.names = TRUE)
point_prevalence <- rbindlist(lapply(all_csv, fread), fill = TRUE)
point_prevalence[, pts_tot := (pts_unsuitable + pts_suitable), ]
point_prevalence[, pts_prevalence := round((pts_suitable/pts_tot), 3), ]

# count number of species processes
table(point_prevalence$pts_available)
point_prevalence <- unique(point_prevalence[, c("raster_name", "pts_prevalence", "pts_available")])

all_csv <- list.files(model_prev_loc, pattern = ".csv$", full.names = TRUE)
model_prevalence <- rbindlist(lapply(all_csv, fread), fill = TRUE) 
model_prevalence[ , observed_md_prevalence := round((suitable_num_pix / total_num_pix), 3), ]
model_prevalence <- model_prevalence[, c("id_no", "scientific_name","bird_or_mammal","total_num_pix",
                                         "suitable_num_pix","type","raster_name","observed_md_prevalence")]
names(point_prevalence)
names(model_prevalence)
r1 <- unique(point_prevalence$raster_name)
r2 <- unique(model_prevalence$raster_name)
if(!any(r1 %in% r2)) stop("Error: no matching names between files")
vali <- merge(model_prevalence, point_prevalence, all = T, by = "raster_name")
vali[ , better_than_random := +(pts_prevalence > observed_md_prevalence)]
if(class == "birds"){
  vali <- subset(vali, bird_or_mammal == "bird")
} else if(class == "mammals"){
  vali <- subset(vali, bird_or_mammal == "mammal")
} else {
  print("class is neither birds nor mammals")
}

unique(vali$bird_or_mammal)

vali <- vali[, c("raster_name", "id_no", "scientific_name",
                 "bird_or_mammal", "type", "observed_md_prevalence",
                 "pts_prevalence", "better_than_random" )]
fwrite(vali, file= paste0(out_dir, class, "_point_prevalence_results.csv"))




