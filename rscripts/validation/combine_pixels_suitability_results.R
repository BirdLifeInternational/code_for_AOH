library(data.table)
library(stringr)
library(rlist)


# quck check that results are identical between python and R
# py_birds_lr <- as.data.table(read.csv("../aoh_out/output_R/birds/lumbierres_LR/birds_lr_area_py.csv"))
# lf <- list.files("../aoh_out/output_R/birds/lumbierres_LR/area/", pattern = "csv$", full.names =  T)
# r_birds_lr <- rbindlist(lapply(lf, fread), fill = TRUE)
# r_birds_lr$name <- paste0(r_birds_lr$id_no, "_", r_birds_lr$type, ".tif")
# r_birds_lr <- r_birds_lr[!str_detect(r_birds_lr$raster_name, "missing"),]
# r_birds_lr <- unique(r_birds_lr[, c("id_no","scientific_name","bird_or_mammal",
#                                     "total_num_pix","suitable_num_pix",
#                                     "not_suitable_num_pix","type","name" )])
# names(py_birds_lr)
# names(r_birds_lr)
# 
# m1 <- merge(py_birds_lr, r_birds_lr, all = T, by= "name")
# m1[unsuitable != not_suitable_num_pix,]
# m1[suitable != suitable_num_pix,]


rm(list = ls())
py_birds_lr <- unique(as.data.table(read.csv("../aoh_out/output_R/birds/lumbierres_LR_esri54017/birds_lr_area_py.csv")))
splitted <- str_split_fixed(py_birds_lr$name, "_", n = 2)
py_birds_lr$id_no <- splitted[,1]
py_birds_lr$season <- str_remove_all(splitted[,2], ".tif")
py_birds_lr[ , tot := unsuitable + suitable]
py_birds_lr[, type := "LR"]



py_birds_ele <- as.data.table(read.csv("../aoh_out/output_R/birds/lumbierres_LR_validation_elevation_only_esri54017/birds_lr_area_py.csv"))
setnames(py_birds_ele, "suitable", "suitable_elevation_only")
py_birds_ele <- unique(py_birds_ele[, c("name", "suitable_elevation_only")])

py_birds_hab <- as.data.table(read.csv("../aoh_out/output_R/birds/lumbierres_LR_validation_habitat_only_esri54017/birds_lr_area_py.csv"))
setnames(py_birds_hab, "suitable", "suitable_habitat_only")
py_birds_hab <- unique(py_birds_hab[, c("name", "suitable_habitat_only")])


vali <- unique(merge(py_birds_ele, py_birds_hab, by = "name"))

pix_birds <- unique(merge(py_birds_lr, vali, by = "name"))
dim(pix_birds)

dim(unique(pix_birds[, c("name")]))

fwrite(pix_birds, "../aoh_out/output_R/birds/area_pixel_summary.csv")


rm(list = ls())
py_mammals_lr <- unique(as.data.table(read.csv("../aoh_out/output_R/mammals/lumbierres_LR_esri54017/mammals_lr_area_py.csv")))
splitted <- str_split_fixed(py_mammals_lr$name, "_", n = 2)
py_mammals_lr$id_no <- splitted[,1]
py_mammals_lr$season <- str_remove_all(splitted[,2], ".tif")
py_mammals_lr[ , tot := unsuitable + suitable]
py_mammals_lr[, type := "LR"]



py_mammals_ele <- as.data.table(
  read.csv("../aoh_out/output_R/mammals/lumbierres_LR_validation_elevation_only_esri54017/mammals_ele_area_py.csv"))
setnames(py_mammals_ele, "suitable", "suitable_elevation_only")
py_mammals_ele <- unique(py_mammals_ele[, c("name", "suitable_elevation_only")])

py_mammals_hab <- as.data.table(read.csv("../aoh_out/output_R/mammals/lumbierres_LR_validation_habitat_only_esri54017/mammals_hab_area_py.csv"))
setnames(py_mammals_hab, "suitable", "suitable_habitat_only")
py_mammals_hab <- unique(py_mammals_hab[, c("name", "suitable_habitat_only")])


vali <- unique(merge(py_mammals_ele, py_mammals_hab, by = "name"))

pix_mammals <- unique(merge(py_mammals_lr, vali, by = "name"))
dim(pix_mammals)

dim(unique(pix_mammals[, c("name")]))
fwrite(pix_mammals, "../aoh_out/output_R/mammals/area_pixel_summary.csv")


print("done")
unlink(paste0(normalizePath(tempdir()), "/"), recursive = TRUE)
