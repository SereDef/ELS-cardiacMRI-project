invisible(lapply(c('mice','dplyr','CMAverse'), require, character.only = TRUE));

datapath <- '../Data'

sample <- readRDS(file.path(datapath, 'impset_sample.rds'))

cardio_outcomes <- c('LV_mass', 'LV_end_diastolic_volume', 'RV_end_diastolic_volume',
                     'LV_ejection_fraction','RV_ejection_fraction')

dset <- complete(sample, 1)


test_reg <- function(e, o, covs = '+ sex + age_mri_10y + ethnicity_cat', data = dset, 
                    non_linear=TRUE) {
  
  fit <- lm(as.formula(paste(o,'~', e, covs)), data=data)
  
  plot(fit)
}

plot(model)