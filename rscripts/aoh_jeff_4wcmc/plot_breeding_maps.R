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

Breeding_sp <- list.files("../aoh_out/output_R/birds/lumbierres_LR_esri54017/", pattern = "_Breeding.tif$")
Breeding_sp <- stringr::str_replace_all(Breeding_sp, "_Breeding.tif", "")
all_fls <- list.files("../aoh_out/output_R/birds/lumbierres_LR_esri54017/", pattern = ".tif$", full.names = T)
foreach::foreach(i = 1:length(Breeding_sp), .errorhandling = "pass") %do% {
  taxon_id <- Breeding_sp[i]

  
  plot_nam <- paste0(output_plots, "plot_leaflet_", taxon_id, ".png")
  if(file.exists(plot_nam) & file.size(plot_nam) > 0){
   print(paste0(taxon_id, " already processed"))
  } else {
  print(paste0("processing ", taxon_id, " with index ", i))
  general_info <- subset(all_species, id_no == taxon_id)
  #print(general_info)
  ################# 
  tmp <- all_fls[str_detect(all_fls, taxon_id)]
  type = stringr::str_remove_all(stringr::str_remove_all(tmp, paste0(output_dir, "/", taxon_id, "_")), ".tif")
  type = str_replace_all(type, "_", " ")
  raster_name = sort(tmp)
  br = raster_name[str_detect(raster_name, "_Breeding.tif")]
  nbr = raster_name[str_detect(raster_name, paste0(taxon_id, "_Nonbreeding.tif"))]
  #bo = raster_name[str_detect(raster_name, "_Breeding_and_Nonbreeding.tif")]
  
  out_br_tmp = paste0(normalizePath(output_tmp), "/", taxon_id, "_Breeding.tif")
  out_nbr_tmp = paste0(normalizePath(output_tmp), "/", taxon_id, "_Nonbreeding.tif")
  
  fl_size <- ceiling(fileSize(br))
  #print(paste0("file size breeding taxon_id: ", fl_size))
  if( fl_size > 1 & fl_size < 3){                 
  	gdalUtilities::gdalwarp(
		srcfile = br, 
		dstfile = out_br_tmp,
		s_srs = "ESRI:54017",
		t_srs = "ESRI:54017", 
		tr = c(10000, 10000), 
		r = "mode", 
		dryrun = FALSE, # set to TRUE to see the dgalwarp command line	
		overwrite = TRUE)
   } else if(fl_size >= 3){                 
  	gdalUtilities::gdalwarp(
		srcfile = br, 
		dstfile = out_br_tmp,
		s_srs = "ESRI:54017",
		t_srs = "ESRI:54017", 
		tr = c(50000, 50000), 
		r = "mode", 
		dryrun = FALSE, # set to TRUE to see the dgalwarp command line	
		overwrite = TRUE)
   } else {
	gdalUtilities::gdalwarp(
		srcfile = br, 
		dstfile = out_br_tmp,
		s_srs = "ESRI:54017",
		t_srs = "ESRI:54017", 
		tr = c(500, 500), 
		r = "mode", 
		dryrun = FALSE, # set to TRUE to see the dgalwarp command line	
		overwrite = TRUE)
   }

  br <- read_stars(out_br_tmp)#Breeding_tiff)
  spbr <- st_as_sf(br, connect8 = T)
  names(spbr)[1] <- "Breeding"
  rm(br)
  
  
  
    fl_size <- ceiling(fileSize(nbr))
  #print(paste0("file size ", "taxon_id: ", fl_size))
  if( fl_size > 1 & fl_size < 3){                 
  	gdalUtilities::gdalwarp(
		srcfile = nbr, 
		dstfile = out_nbr_tmp,
		s_srs = "ESRI:54017",
		t_srs = "ESRI:54017", 
		tr = c(10000, 10000), 
		r = "mode", 
		dryrun = FALSE, # set to TRUE to see the dgalwarp command line	
		overwrite = TRUE)
   } else if(fl_size >= 3){                 
  	gdalUtilities::gdalwarp(
		srcfile = nbr, 
		dstfile = out_nbr_tmp,
		s_srs = "ESRI:54017",
		t_srs = "ESRI:54017", 
		tr = c(50000, 50000), 
		r = "mode", 
		dryrun = FALSE, # set to TRUE to see the dgalwarp command line	
		overwrite = TRUE)
   } else {
	gdalUtilities::gdalwarp(
		srcfile = nbr, 
		dstfile = out_nbr_tmp,
		s_srs = "ESRI:54017",
		t_srs = "ESRI:54017", 
		tr = c(500, 500), 
		r = "mode", 
		dryrun = FALSE, # set to TRUE to see the dgalwarp command line	
		overwrite = TRUE)
   }

  nbr <- read_stars(out_nbr_tmp)#Breeding_tiff)
  spnbr <- st_as_sf(nbr, connect8 = T)
  names(spnbr)[1] <- "Nonbreeding"
  ############ plot ###########
  
  

  
  ext_spbr <- st_bbox(spbr)
  ##### copia da aoh: expand borders #####
  
  bbr <- as.list(ext_spbr)
  bbr2 <- bbr
  if (expand > 0) {
    xf <- abs(bbr[["xmax"]] - bbr[["xmin"]]) * expand
    yf <- abs(bbr[["ymax"]] - bbr[["ymin"]]) * expand
    bbr2[["xmin"]] <- bbr[["xmin"]] - xf
    bbr2[["xmax"]] <- bbr[["xmax"]] + xf
    bbr2[["ymin"]] <- bbr[["ymin"]] - yf
    bbr2[["ymax"]] <- bbr[["ymax"]] + yf
  }
  new_box_br <- unlist(bbr2)
  
  
    ext_spnbr <- st_bbox(spnbr)
  ##### copia da aoh: expand borders #####
  
  bnbr <- as.list(ext_spnbr)
  bnbr2 <- bnbr
  if (expand > 0) {
    xf <- abs(bnbr[["xmax"]] - bnbr[["xmin"]]) * expand
    yf <- abs(bnbr[["ymax"]] - bnbr[["ymin"]]) * expand
    bnbr2[["xmin"]] <- bnbr[["xmin"]] - xf
    bnbr2[["xmax"]] <- bnbr[["xmax"]] + xf
    bnbr2[["ymin"]] <- bnbr[["ymin"]] - yf
    bnbr2[["ymax"]] <- bnbr[["ymax"]] + yf
  }
  new_box_nbr <- unlist(bnbr2)
  
  
  ############ asinara ############
  
  
  species_cropped_br <- st_crop(world, y = new_box_br)
  species_cropped_nbr <- st_crop(world, y = new_box_nbr)
  fil.1 = adjustcolor(viridis(2)[1], alpha.f = 1)
  fil.2 = adjustcolor(viridis(2)[2], alpha.f = 1)
  fil.3 = adjustcolor("gray", alpha.f = 0)
  
  
  cols.fill = c( 
    "Range map" = fil.1,
    "Suitable" = fil.2, "Unsuitable" = fil.3
  )
  
  
  
  if(!dir.exists(paste0(output_dir, "plots")))  dir.create(paste0(output_dir, "plots"))
  
  
  sbr1 <- spbr[spbr$Breeding == 1,]
  sbr0 <- spbr[spbr$Breeding == 0,]
    snbr1 <- spnbr[spnbr$Nonbreeding == 1,]
  snbr0 <- spnbr[spnbr$Nonbreeding == 0,]
  range_map <- paste0("../IUCN_data/NOT_to_be_committed/output_gpkg/birds/id_no_", taxon_id, ".gpkg")
  spp_range_data <- read_sf(range_map)
  st_geometry(spp_range_data) <- "geometry"
  
  spp_range_data <- subset(spp_range_data, presence == 1 | presence == 2)
  spp_range_data <- subset(spp_range_data, origin == 1 | origin == 2 | origin == 6)


   bred <- subset(spp_range_data, seasonal == 1 | seasonal == 2 | seasonal == 5)
    bred$seasonal <- 2
    nonbred <- subset(spp_range_data, seasonal == 1 | seasonal == 3 | seasonal == 5)
    nonbred$seasonal <- 3
    bred <- bred %>% 
      group_by(seasonal) %>%
      summarise(geometry = sf::st_union(geometry), 
                sci_name = unique(spp_range_data$sci_name),
                id_no = unique(spp_range_data$id_no),
                seasonal = 2,
                presence = 1, origin = 1, 
                terrestrial = "true", marine = "false", freshwater = "false"
      ) %>% ungroup()
    
    nonbred <- nonbred %>% 
      group_by(seasonal) %>%
      summarise(geometry = sf::st_union(geometry), 
                sci_name = unique(spp_range_data$sci_name),
                id_no = unique(spp_range_data$id_no),
                seasonal = 3,
                presence = 1, origin = 1, 
                terrestrial = "true", marine = "false", freshwater = "false") %>%  ungroup()
    

    
  p <- ggplot() + geom_sf(data = species_cropped_br) 
  
  
  p = p + 
    geom_sf(data = sbr1, color = NA, aes(fill = "Suitable")) + 
    geom_sf(data = sbr0, color = NA, aes(fill = "Unsuitable")) +
    geom_sf(data = bred, size = 1.2, fill = "transparent", color = fil.1) #linetype = "11",
  
  p <- p + ggtitle(paste0("Breeding range + AOH: ", unique(general_info$binomial))) +
    scale_fill_manual(values = cols.fill,
                      name = "Legend:",
                      labels = c("Range map border", "Suitable habitat", "Unsuitable habitat")) +
        theme(legend.position="none") # suppress the legend
    #theme(legend.position=c(1.25, 0.8), legend.box = "horizontal",
    #      legend.margin=margin(t = 0, unit='cm'),
    #      plot.margin=unit(x=c(0.3,5,0,0),units="cm"),
    #      plot.background=element_rect(fill="white")) #+  coord_sf()
  
  
  
    pn <- ggplot() + geom_sf(data = species_cropped_nbr) 
  
  
  pn = pn + 
    geom_sf(data = snbr1, color = NA, aes(fill = "Suitable")) + 
    geom_sf(data = snbr0, color = NA, aes(fill = "Unsuitable")) +
    geom_sf(data = nonbred, size = 1.2, fill = "transparent", color = fil.1) #linetype = "11",
  
  pn <- pn + ggtitle(paste0("Nonbreeding range + AOH: ", unique(general_info$binomial))) +
    scale_fill_manual(values = cols.fill,
                      name = "Legend:",
                      labels = c("Range map border", "Suitable habitat", "Unsuitable habitat")) +
    theme(legend.position="bottom", legend.box = "horizontal",
          legend.margin=margin(t = 0, unit='cm'),
          plot.margin=unit(x=c(0.3,5,0,0),units="cm"),
          plot.background=element_rect(fill="white")) #+  coord_sf()
          
  
  png(filename = plot_nam)
  print(gridExtra::grid.arrange(p, pn, nrow=2))
  dev.off() 
  graphics.off()
  
  if(file.exists(out_br_tmp)) unlink(out_br_tmp)
  if(file.exists(out_nbr_tmp)) unlink(out_nbr_tmp)
   
  }
}

print("plots done")
