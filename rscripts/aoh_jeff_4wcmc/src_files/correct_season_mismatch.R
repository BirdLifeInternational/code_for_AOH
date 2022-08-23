#### correct season mismatched
#### any habitat code suitable for breeding or non-breeding should also 
#' be considered suitable for a range map coded with seasonality 1 or 5
# Possible values are"Seasonal Occurrence Unknown" "Resident"                   
#  "Passage"                     "Breeding Season"            
#  "Non-Breeding Season"  
#  Similarly, if a species is migratory and some habitat are suitable for resident,
# also consider 

# tmp <- spp_range_data[, "seasonal", drop= F]
# tmp <- sf::st_drop_geometry(tmp)
# tmp <- merge(tmp, hb_iucn_code, by.x = "seasonal", by.y = "code", all.x = T)
correct_season_mismatch <- function(spp_habitat_data){
  if(any(spp_habitat_data$season == "Breeding Season")){
    tmp <- tmp2 <- tmp3 <- subset(spp_habitat_data, season == "Breeding Season")
    tmp$season <- "Resident"
    tmp2$season <- "Seasonal Occurrence Unknown"
    spp_habitat_data <- rbind(spp_habitat_data, tmp, tmp2)
  } 
  
  if(any(spp_habitat_data$season == "Non-Breeding Season")){
    tmp <- tmp2 <- tmp3 <- subset(spp_habitat_data, season == "Non-Breeding Season")
    tmp$season <- "Resident"
    tmp2$season <- "Seasonal Occurrence Unknown"
    spp_habitat_data <- rbind(spp_habitat_data, tmp, tmp2)
  } 
  
  if(any(spp_habitat_data$season == "Seasonal Occurrence Unknown")){
    tmp <- tmp2 <- tmp3 <- subset(spp_habitat_data, season == "Seasonal Occurrence Unknown")
    tmp$season <- "Resident"
    tmp2$season <- "Breeding Season"
    tmp3$season <- "Non-Breeding Season"
    spp_habitat_data <- rbind(spp_habitat_data, tmp, tmp2, tmp3)
  } 
  
  if(any(spp_habitat_data$season == "Resident")){
    tmp <- tmp2 <- tmp3 <- subset(spp_habitat_data, season == "Resident")
    tmp$season <- "Breeding Season"
    tmp2$season <- "Seasonal Occurrence Unknown"
    tmp3$season <- "Non-Breeding Season"
    spp_habitat_data <- rbind(spp_habitat_data, tmp, tmp2, tmp3)
  } 
  
  spp_habitat_data <- unique(spp_habitat_data)
  return(spp_habitat_data)
}