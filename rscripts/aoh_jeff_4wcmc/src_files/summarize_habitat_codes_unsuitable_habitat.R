# summarize the land cover types that were clipped out by habitat

summarize_habitat_code_unsuitable_pixels <- function(updated_path, 
         taxon_id, 
         crosswalk = aoh::crosswalk_lumb_cgls_data, 
         path_habitat_rast = normalizePath(path_habitat_tif),
         output_df = normalizePath(output_df),
         model_validation = FALSE,
         habitat_only = FALSE,
         file_nam = file_nam,
         text_file = text_file # seasonality combination
         ){


    tmp_out <- paste0(normalizePath(updated_path), "/tmp_rast")
    if(!dir.exists(tmp_out)) dir.create(tmp_out)
  
  
    # lf <- list.files(updated_path, pattern = ".tif$", full.names = TRUE)
    # lf <- normalizePath(lf)
    # if(any(str_detect(lf, "aoh_tif"))) lf <- lf[-which(str_detect(lf, "aoh_tif"))]
    # if(any(str_detect(lf, "_1.tif$")))   lf <- lf[-which(str_detect(lf, "_1.tif$"))]
    # if(any(str_detect(lf, "_2.tif$")))  lf <- lf[-which(str_detect(lf, "_2.tif$"))]
    # if(any(str_detect(lf, "_3.tif$")))  lf <- lf[-which(str_detect(lf, "_3.tif$"))]
    
    tar <- normalizePath(file_nam)
    # crop the habitat data to the extent of the raster 
    gdalLog <- capture.output(gdalUtilities::gdalinfo(tar, hist = TRUE, dryrun=FALSE))
    up_left <- str_remove(gdalLog[which(stringr::str_detect(gdalLog, "Upper Left"))], "Upper Left")
    up_left <- strsplit(strsplit(strsplit(up_left, "\\)")[[1]][1], "\\(")[[1]][2], "\\,")
    ymax <- round(as.numeric(up_left[[1]][2]))
    xmin <- round(as.numeric(up_left[[1]][1]))
    low_left  <- gdalLog[which(stringr::str_detect(gdalLog, "Lower Left"))]
    low_left <- strsplit(strsplit(strsplit(low_left, "\\)")[[1]][1], "\\(")[[1]][2], "\\,")
    ymin <- round(as.numeric(low_left[[1]][2]))
    up_right <- gdalLog[which(stringr::str_detect(gdalLog, "Upper Right"))]
    up_right <- strsplit(strsplit(strsplit(up_right, "\\)")[[1]][1], "\\(")[[1]][2], "\\,")
    xmax <- round(as.numeric(up_right[[1]][1]))
  
    # crop the land cover on the extent of the aoh raster
    cropped_file_path <- paste0(tmp_out, "/", taxon_id, "_cropped_habitat.tif")
    if(file.exists(cropped_file_path)) unlink(cropped_file_path)
    gdalUtilities::gdalwarp(srcfile = path_habitat_rast, 
                            dstfile = cropped_file_path,
                            # set the extent as -te <xmin ymin xmax ymax>
                            te = c(xmin, ymin, xmax, ymax),
                            overwrite = TRUE)
  # cropped_elevation_path <- paste0(tmp_out, "/", taxon_id, "_cropped_elevation.tif")
  # gdalUtilities::gdalwarp(srcfile = path_elevation, 
  #                         dstfile = cropped_elevation_path,
  #                         # set the extent as -te <xmin ymin xmax ymax>
  #                         te = c(xmin, ymin, xmax, ymax),
  #                         overwrite = TRUE)
    outfile = paste0(tmp_out, "/", taxon_id, text_file, "_checking_codes_unsuitable.tif")
    if(file.exists(outfile)) unlink(outfile)
    system_call_gdal <- paste0("gdal_calc.py -A ", tar, " -B ", 
           cropped_file_path, " --outfile=", outfile, " --calc='B + A'") 
    system(system_call_gdal)
  #--NoDataValue=1
    res <- frequency_gdalinfo_raster(outfile)
    res <- subset(res, frequency > 0)
    res <- subset(res, code_values%%10 == 0)
  
    combi <- merge(res, crosswalk, by.x = "code_values", by.y = "value", all.x = TRUE)
    miss <- unique(combi[is.na(code), code_values])
    combi[is.na(code), code := paste0("land_cover_", paste0(code_values, collapse = "_"), "_not_available_in_the_crosswalk")]
    rl_not_suitable <- paste0(sort(unique(combi$code)), collapse = "|")
    land_cover_not_suitable <- paste0(sort(unique(combi$code_values)), collapse = "|")
    dt <- data.table(raster_name = paste0(taxon_id, ".tif"), rl_habitat_code_unsuitable_pixels_from_aoh_habitat = rl_not_suitable,
                     land_cover_code_unsuitable_pixels_from_aoh_habitat = land_cover_not_suitable)
    
    dt_file <- paste0(normalizePath(output_df), "/id_no_", taxon_id, "_", text_file, "_habitat_codes_unsuitable_pixels.csv")
    fwrite(dt, file = dt_file)
    file_to_delete <- paste0(tar, ".aux.xml")
    if(file.exists(file_to_delete)) unlink(file_to_delete)
    if(file.exists(cropped_file_path)) unlink(cropped_file_path)
    if(file.exists(outfile)) unlink(outfile)

  return(0)
  
}
