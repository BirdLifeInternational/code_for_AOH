# model prevalence with random effect of choice

dat <- subset(dat, seasonality == "Resident" | seasonality == "Breeding_and_Nonbreeding")
dat <- subset(dat, bird_or_mammal == "mammal")
summary(dat)
dat$total_num_pix = round(dat$total_num_pix_seasonal_range_map / 1000)
dat$ss = round(dat$suitable_num_pix/1000)
dat <- dat[total_num_pix>0,]
library(lme4)
model <- glm(cbind(ss, total_num_pix) ~
          scale(count_hab_full)+scale(elevation_range) + 
          scale(mid_point_elevation)+
            realm+
          seasonality,
        #weights = total_num_pix,
        data = dat,
        family = binomial, na.action = na.exclude)


model <- glmmTMB::glmmTMB(obs_mdl_prevalence ~
                            scale(count_hab_full)+scale(elevation_range) + 
                            scale(mid_point_elevation)+realm+
                            seasonality + (1 | family_name),
                          weights = total_num_pix,
                          data = dat,
                          family = binomial(link = "logit"), na.action = na.exclude)

vars <- insight::get_variance(model)
r2_marginal <- vars$var.fixed / (vars$var.fixed + vars$var.random + vars$var.residual) 
r2_conditional <- (vars$var.fixed + vars$var.random) / (vars$var.fixed  + vars$var.random + vars$var.residual) # 0.4127328
#R2


print(paste0("summary model for ", class, " with ", random_variable, "as random effect."))
print(summary(model))
print(paste0("The variance explained by the fixed effects is ", round(r2_marginal, 3)))
print(paste0("The variance explained by the model is ", round(r2_conditional, 3)))


if(!dir.exists("../aoh_out/output_R/validation/")) dir.create("../aoh_out/output_R/validation/")
if(!dir.exists("../aoh_out/output_R/validation/model_prevalence/")) dir.create("../aoh_out/output_R/validation/model_prevalence/")
output_dir_mp <- "../aoh_out/output_R/validation/model_prevalence/"
oo_dir <-  paste0(output_dir_mp, class, "/")
if(!file.exists(oo_dir)) dir.create(oo_dir)
save(model, file = paste0(oo_dir, "model_glmmTMB_", random_variable, ".RData"))




prd_family_RE <- predict(model, type = "response")
prd_family_RE <- as.data.frame(prd_family_RE)
with_predictions <- as.data.table(cbind(dat, round(prd_family_RE, 3)))

png(paste0(oo_dir, "plot_obs_pred_model_prevalence_bird.png"))
plot(x = with_predictions$model_prevalence, y = with_predictions$prd_family_RE,
     xlab = "Observed model prevalence",
     ylab = "Predicted model prevalence", 
     main = paste0("Model prevalence with ", random_variable, " as random effect"))
dev.off()

# Outliers detection:: 

with_predictions[, prd_family_RE := round(prd_family_RE, 3), ]
with_predictions[, obs_mdl_prevalence := round(obs_mdl_prevalence, 3),]
with_predictions[ , difference := obs_mdl_prevalence-prd_family_RE,]
lowerq = quantile(with_predictions$difference,na.rm = T)[2]
upperq = quantile(with_predictions$difference,na.rm = T)[4]
iqr = upperq - lowerq
# multiply (interquartile range × 1.5) as explained in the paper
mild.threshold.upper = round((iqr * 1.5) + upperq, 3) 
mild.threshold.lower = round(lowerq - (iqr * 1.5), 3)  
extreme.threshold.upper = round((iqr * 3) + upperq, 3)
extreme.threshold.lower = round(lowerq - (iqr * 3), 3) 
thresholds <- paste0(extreme.threshold.lower, "|", mild.threshold.lower, 
                     "|", mild.threshold.upper, "|", extreme.threshold.upper)
# Mild upper outliers 
with_predictions[, mdl_intqrt_thresh := thresholds, ]
with_predictions[, mild_upper_outliers := (difference > mild.threshold.upper),]
with_predictions[, mild_lower_outliers := (difference < mild.threshold.lower), ]
with_predictions[, mild_outliers := ((difference > mild.threshold.upper) | (difference < mild.threshold.lower)), ]

with_predictions[, extreme_outliers := ((difference > extreme.threshold.upper) | (difference < extreme.threshold.lower)), ]
setnames(with_predictions, "prd_family_RE", "predicted_mdl_prevalence")

names(with_predictions)

with_predictions <- with_predictions[, c("id_no", "seasonality",  
                                         "predicted_mdl_prevalence", 
                                         "mild_outliers", "extreme_outliers", "mdl_intqrt_thresh")]


class= "bird"
class = "mammal"
fwrite(with_predictions, paste0("../aoh_out/output_R/validation/results_model_prevalence_", class, ".csv"))
bi = fread(paste0("../aoh_out/output_R/validation/results_model_prevalence_bird.csv"))
mam = fread(paste0("../aoh_out/output_R/validation/results_model_prevalence_mammal.csv"))
pred <- rbind(bi, mam)
fwrite(pred, "../aoh_out/output_R/validation/results_model_prevalence.csv")
