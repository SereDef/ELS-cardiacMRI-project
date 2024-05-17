# MAIN ANALYSIS ================================================================

invisible(lapply(c('mice'), require, character.only = TRUE));

datapath <- '../Data'

sample <- readRDS(file.path(datapath, 'impset_sample.rds'))

cardio_outcomes <- c('LV_mass', 'LV_end_diastolic_volume', 'RV_end_diastolic_volume',
                     'LV_ejection_fraction','RV_ejection_fraction')

fitit <- function(e, o, covs = '+ sex + age_mri_10y + ethnicity_cat', data = sample) {
  fit <- with(data, lm(as.formula(paste(o,'~', e, covs))))
  p <- mice::pool(fit)
  mod <- summary(p, 'all', conf.int = 0.95) # extract relevant information
  names(mod)[which(names(mod) %in% c('2.5 %','97.5 %'))] <- c('lci','uci')
  mod$rsq <- c(pool.r.squared(fit)[1], rep(NA, nrow(mod)-1)) # add a column for R2
  mod$rsq_adj <- c(pool.r.squared(fit, adjusted = T)[1], rep(NA, nrow(mod)-1)) # adjusted R2
  # Round everything
  mod[,-1] <-round(mod[,-1], 3)
  # add model name as first column
  mod_name <- paste(e,'-',o)
  mod <- cbind(data.frame("model" = rep(mod_name, nrow(mod))), mod)
  # And one space in between models
  mod[nrow(mod)+1,] <- NA

  print(mod)
  return(mod)
}

mods <- list()

for (out in c(paste0(cardio_outcomes,'_bsaz'), paste0(cardio_outcomes,'_10y'))) { 
  pre <- fitit('prenatal_stress_z', out) 
  pos <- fitit('postnatal_stress_z', out)
  add <- fitit('prenatal_stress_z + postnatal_stress_z', out)
  int <- fitit('prenatal_stress_z * postnatal_stress_z', out)

  mods[[paste0(out,'_M0')]] <- rbind(pre, pos, add, int)
}

for (out in c(paste0(cardio_outcomes,'_bsaz'), paste0(cardio_outcomes,'_10y'))) { 
  m1_adj <- '+ sex + age_mri_10y + ethnicity_cat + mom_BMI_pre_pregnancy + mom_smoking'
  pre <- fitit('prenatal_stress_z', out, covs=m1_adj) 
  pos <- fitit('postnatal_stress_z', out, covs=m1_adj)
  add <- fitit('prenatal_stress_z + postnatal_stress_z', out, covs=m1_adj)
  int <- fitit('prenatal_stress_z * postnatal_stress_z', out, covs=m1_adj)
  
  mods[[paste0(out,'_M1')]] <- rbind(pre, pos, add, int)
}


openxlsx::write.xlsx(mods, file = file.path(datapath, paste0('../results.xlsx')),
                     overwrite = T)
# write.csv(mods, file.path(datapath,'result.csv'))
