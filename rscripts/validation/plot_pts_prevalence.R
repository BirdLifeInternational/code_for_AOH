library(data.table)
pntp <- fread("../aoh_out/output_R/validation/tmp_birds_pointprevalence.csv")
mdl <- fread("../aoh_out/output_R/validation/data_summary_mdl_prevalence.csv")
dt <- merge(mdl, pntp, by = "raster_name", all = T)
dt <- dt[!is.na(raster_name) & !is.na(pts_prevalence),]
pt <- dt[, .(raster_name, obs_mdl_prevalence, pts_prevalence)]
library(plotrix)
library(viridis)
library(scales)
plotrix::sizeplot(y = round(pt$obs_mdl_prevalence,2), x = round(pt$pts_prevalence,2),
                  pch = 19,
                  xlab = "Model prevalence", ylab = "Point prevalence",
                  xlim = c(0,1.01), ylim = c(0, 1.01),
                  col = alpha("gray", 0.5), 
                  main = "Point prevalence vs. model prevalence for 2400 birds")

lin <- seq(0, 1, by =0.1)
lines(x=lin, y = lin, 
      col = "black",
      lwd = 2)
