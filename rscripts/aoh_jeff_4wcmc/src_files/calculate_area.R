# calculate frequencies of values within a raster
library(gdalUtilities)
library(stringr)
#file_nam = "/home/michela/Data/git/Birdlife/aoh_out/output_R/birds/lumbierres_LR/aoh_tif/22714939_3.tif"
frequency_gdalinfo_raster <- function(file_nam){
  file_nam <- normalizePath(file_nam)
  gdalLog <- capture.output(gdalUtilities::gdalinfo(file_nam, hist = TRUE, dryrun=FALSE))
  area_calc <- gdalLog[which(stringr::str_detect(gdalLog, "buckets from")) + 1]
  area_calc <- strsplit(area_calc, "\\s+")[[1]]
  if(area_calc[1] == "") area_calc <- area_calc[2:length(area_calc)]
  if(any(area_calc == "")) print("some empty values here")
  
  area_calc <- as.numeric(area_calc)
  vals <- 0:(length(area_calc)-1)
  summa <- data.table::data.table(code_values = vals, frequency = area_calc)
  unlink(paste0(file_nam, ".aux.xml"), recursive = TRUE)
  return(summa)
}