library(stars)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(stringr)
library(dplyr)
library(viridis)
library(leaflet)
library(terra)
library(janitor)
library(aoh)
library(readr)
library(foreach)
rm(list = ls())
Sys.getenv("DISPLAY")

projection <- "esri54017"
# fileSize function from MODIS package
fileSize <- function(file,units = "GB") {
  
  units <- toupper(units)
  unit <- c(1, 1024, 1048576, 1073741824, 1073741824*1024) 
  names(unit) <- c("B", "KB", "MB", "GB","TB")
  
  if (!units %in% names(unit)) {
    stop('unit must be one of: "B", "KB", "MB", "GB" or "TB"')
  } 
  
  file <- file.info(file)
  file <- file[!file$isdir,"size"]
  
  res <- file/unit[toupper(units)]
  return(res)
}



crs_wkt = "PROJCRS[\"World_Behrmann\",\n    BASEGEOGCRS[\"WGS 84\",\n        DATUM[\"World Geodetic System 1984\",\n            ELLIPSOID[\"WGS 84\",6378137,298.257223563,\n                LENGTHUNIT[\"metre\",1]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"Degree\",0.0174532925199433]]],\n    CONVERSION[\"World_Behrmann\",\n        METHOD[\"Lambert Cylindrical Equal Area\",\n            ID[\"EPSG\",9835]],\n        PARAMETER[\"Latitude of 1st standard parallel\",30,\n            ANGLEUNIT[\"Degree\",0.0174532925199433],\n            ID[\"EPSG\",8823]],\n        PARAMETER[\"Longitude of natural origin\",0,\n            ANGLEUNIT[\"Degree\",0.0174532925199433],\n            ID[\"EPSG\",8802]],\n        PARAMETER[\"False easting\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8806]],\n        PARAMETER[\"False northing\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8807]]],\n    CS[Cartesian,2],\n        AXIS[\"(E)\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1]],\n        AXIS[\"(N)\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1]],\n    USAGE[\n        SCOPE[\"Not known.\"],\n        AREA[\"World.\"],\n        BBOX[-90,-180,90,180]],\n    ID[\"ESRI\",54017]]"
output_plots = paste0("../aoh_out/output_R/plot/")
if(!dir.exists(output_plots)) dir.create(output_plots)
output_tmp = paste0("../aoh_out/output_R/tmp/")
if(!dir.exists(output_tmp)) dir.create(output_tmp)
output_dir <- "../aoh_out/output_R/birds/lumbierres_LR_esri54017/"
############ generic fun #############

all_species <- readxl::read_xlsx("../IUCN_data/habitat_preferences/BL_Habitats_2021.xlsx")  %>% clean_names()
all_species$id_no <- all_species$sis_id 
all_species$binomial <- all_species$scientific_name
all_species <- unique(all_species[, c("id_no", "binomial")])


# continent = c("Africa", "Europe"), 
world <- ne_countries(scale = "medium", returnclass = "sf")

# class(world)
# st_crs(world)

world <- st_transform(world, crs = "ESRI:54017")
myfun <- function(x) round(median(x, na.rm = T))

# how much do we expand the borders?
expand = 0.05
########### load sp ##########

resident_sp <- list.files(normalizePath(paste0("../aoh_out/output_R/birds/lumbierres_LR_", projection)),
                          pattern = "_Resident.tif$")
resident_sp <- unique(stringr::str_replace_all(resident_sp, "_Resident.tif", ""))

foreach::foreach(i = 1:length(resident_sp), .errorhandling = "pass") %do% {


  taxon_id <- resident_sp[i]

  
  plot_nam <- paste0(output_plots, "plot_leaflet_", taxon_id, ".png")
  if(file.exists(plot_nam) & file.size(plot_nam) > 0){
   print(paste0(taxon_id, " already processed"))
  } else {
   
  
    print(paste0("processing ", taxon_id, " with index ", i))
  general_info <- subset(all_species, id_no == taxon_id)
  #print(general_info)
  
  
  ################# 
  
  
  res_tif = normalizePath(paste0(output_dir, taxon_id, "_Resident.tif"))
  file.exists(res_tif)
  out_tif_tmp = paste0(normalizePath(output_tmp), "/", taxon_id, ".tif")
  #sp_rast <- terra::rast(res_tif)
  
  
  fl_size <- ceiling(fileSize(res_tif))
  #print(paste0("file size ", "taxon_id: ", fl_size))
  if( fl_size > 1 & fl_size < 3){                 
  	gdalUtilities::gdalwarp(
		srcfile = res_tif, 
		dstfile = out_tif_tmp,
		s_srs = "ESRI:54017",
		t_srs = "ESRI:54017", 
		tr = c(10000, 10000), 
		r = "mode", 
		dryrun = FALSE, # set to TRUE to see the dgalwarp command line	
		overwrite = TRUE)
   } else if(fl_size >= 3){                 
  	gdalUtilities::gdalwarp(
		srcfile = res_tif, 
		dstfile = out_tif_tmp,
		s_srs = "ESRI:54017",
		t_srs = "ESRI:54017", 
		tr = c(50000, 50000), 
		r = "mode", 
		dryrun = FALSE, # set to TRUE to see the dgalwarp command line	
		overwrite = TRUE)
   } else {
	gdalUtilities::gdalwarp(
		srcfile = res_tif, 
		dstfile = out_tif_tmp,
		s_srs = "ESRI:54017",
		t_srs = "ESRI:54017", 
		tr = c(500, 500), 
		r = "mode", 
		dryrun = FALSE, # set to TRUE to see the dgalwarp command line	
		overwrite = TRUE)
   }
  ############ plot ###########
  
  
  resi <- read_stars(out_tif_tmp)#resident_tiff)
  spb <- st_as_sf(resi, connect8 = T)
  names(spb)[1] <- "resident"
  rm(resi)
  
  ext_spb <- st_bbox(spb)
  ##### copia da aoh: expand borders #####
  
  bb <- as.list(ext_spb)
  bb2 <- bb
  if (expand > 0) {
    xf <- abs(bb[["xmax"]] - bb[["xmin"]]) * expand
    yf <- abs(bb[["ymax"]] - bb[["ymin"]]) * expand
    bb2[["xmin"]] <- bb[["xmin"]] - xf
    bb2[["xmax"]] <- bb[["xmax"]] + xf
    bb2[["ymin"]] <- bb[["ymin"]] - yf
    bb2[["ymax"]] <- bb[["ymax"]] + yf
  }
  new_box <- unlist(bb2)
  
  
  ############ asinara ############
  
  
  species_cropped <- st_crop(world, y = new_box)
  fil.1 = adjustcolor(viridis(2)[1], alpha.f = 1)
  fil.2 = adjustcolor(viridis(2)[2], alpha.f = 1)
  fil.3 = adjustcolor("gray", alpha.f = 0)
  
  
  cols.fill = c( 
    "Range map" = fil.1,
    "Suitable" = fil.2, "Unsuitable" = fil.3
  )
  
  
  
  if(!dir.exists(paste0(output_dir, "plots")))  dir.create(paste0(output_dir, "plots"))
  
  
  sb1 <- spb[spb$resident == 1,]
  sb0 <- spb[spb$resident == 0,]
  range_map <- paste0("../IUCN_data/NOT_to_be_committed/output_gpkg/birds/id_no_", taxon_id, ".gpkg")
  spp_range_data <- read_sf(range_map)
  st_geometry(spp_range_data) <- "geometry"
  
  p <- ggplot() + geom_sf(data = species_cropped) 
  
  
  p = p + 
    geom_sf(data = sb1, color = NA, aes(fill = "Suitable")) + 
    geom_sf(data = sb0, color = NA, aes(fill = "Unsuitable")) +
    geom_sf(data = spp_range_data, size = 1.6, fill = "transparent", color = fil.1) #linetype = "11",
  
  p <- p + ggtitle(paste0("Range and Area Of Habitat maps: ", unique(general_info$binomial))) +
    scale_fill_manual(values = cols.fill,
                      name = "Legend:",
                      labels = c("Range map border", "Suitable habitat", "Unsuitable habitat")) +
    theme(legend.position=c(1.25, 0.4), legend.box = "horizontal",
          legend.margin=margin(t = 0, unit='cm'),
          plot.margin=unit(x=c(0.3,5,0,0),units="cm"),
          plot.background=element_rect(fill="white")) #+  coord_sf()
  
  
  
  png(filename = plot_nam)
  print(p)
  dev.off() 
  graphics.off()
  
  if(file.exists(out_tif_tmp)) unlink(out_tif_tmp)
  }
}

print("done all plots"
