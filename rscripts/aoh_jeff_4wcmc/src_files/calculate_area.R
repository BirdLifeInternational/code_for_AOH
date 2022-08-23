# calculate area of raster by simply counting how many cells are suitable or not

calculate_area <- function(type, wd_path, updated_path, output_area, taxon_id, text_file, spp_summary_data, check_combo){
  #### add also to breeding and non-breeding#####
  if(str_detect(type, "LR")){
    target_file <- paste0(updated_path, taxon_id, "_", text_file, ".tif")
    if(file.exists(target_file)){
      x <- terra::rast(target_file)
      freqs <- terra::freq(x)
      count0 <- subset(freqs, value == 0)$count
      count1 <- subset(freqs, value == 1)$count
      
      area <- data.table(id_no = taxon_id, scientific_name = unique(spp_summary_data$binomial),
                         bird_or_mammal = unique(spp_summary_data$bird_or_mammal),
                         total_num_pix = (count0 + count1), 
                         suitable_num_pix = count1,
                         not_suitable_num_pix = count0,
                         type = text_file,
                         seasonality_combination = check_combo,
                         raster_name = paste(target_file))
      fwrite(area, file = paste0(output_area, "suitable_pixel_calculation_", taxon_id, "_", text_file, ".csv"),
             append = FALSE)
    }
  }
  return(0)
}

