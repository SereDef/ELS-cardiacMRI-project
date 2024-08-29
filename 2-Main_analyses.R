# MAIN ANALYSIS ================================================================

invisible(lapply(c('mice','dplyr','CMAverse'), require, character.only = TRUE));

datapath <- '../Data'

sample <- readRDS(file.path(datapath, 'impset_sample.rds'))

cardio_outcomes <- c('LV_mass', 'LV_end_diastolic_volume', 'RV_end_diastolic_volume',
                     'LV_ejection_fraction','RV_ejection_fraction')

fit_reg <- function(e, o, covs = '+ sex + age_mri_10y + ethnicity_cat', data = sample, 
                    non_linear=TRUE) {
  
  fit <- with(data, lm(as.formula(paste(o,'~', e, covs))))
  
  lin_test <- ''
  if (non_linear) {
    fit.nln <-  with(data, lm(as.formula(paste0(o,' ~ poly(', e,',2)', covs))))
    test <- D1(fit.nln, fit)
    if (test$result[1,4]< 0.05) {
      message('\n', e, ' is non-linearly related to ', o)
      #fit <- fit.nln
      lin_test <- 'non-linear'
    }
  }
  p <- mice::pool(fit)
  mod <- summary(p, 'all', conf.int = 0.95) # extract relevant information
  names(mod)[which(names(mod) %in% c('2.5 %','97.5 %'))] <- c('lci','uci')
  mod$rsq <- pool.r.squared(fit)[1] # add a column for R2
  mod$rsq_adj <- pool.r.squared(fit, adjusted = T)[1] # adjusted R2
  # Round everything
  mod[,-1] <-round(mod[,-1], 3)
  # mod$rsq[2] <- lin_test
  # add model name as first column
  mod_name <- paste(e,'-',o)
  mod <- cbind(data.frame("model" = rep(mod_name, nrow(mod))), mod)
  # And one space in between models
  mod[nrow(mod)+1,] <- NA

  #print(mod)
  return(mod)
}

fit_med <- function(o, data = sample) {
  
  datlist <- miceadds::mids2datlist(data)
    
  cma <- function(dset) {
    mod <- CMAverse::cmest(data = dset, 
                  exposure = "prenatal_stress_z", mediator = c("postnatal_stress_z"),
                  outcome = o,  
                  yreg = "linear", # outcome regression model (linear or multimod)
                  EMint = FALSE, # exposure-mediator interaction in yreg (i.e. prenatal*postnatal). 
                  model = "gformula", 
                  # exposure-outcome confounder(s), exposure-mediator confounder(s) and mediator-outcome confounder(s) not affected by the exposure
                  basec = c("sex", "age_mri_10y", "ethnicity_cat", "mom_BMI_pre_pregnancy"), 
                  # mediator-outcome confounder(s) affected by the exposure following temporal order
                  postc = c("mom_smoking"), # regression model for each variable in postc (gformula): 
                  postcreg = list(glm(mom_smoking ~ prenatal_stress_z + ethnicity_cat, family = binomial(), data = dset)), 
                  mreg = list("linear"), # regression model for each mediator (i.e. postnatal ~ prenatal + covariates). 
                  astar = 0, # control value for the exposure (i.e. prenatal stress = mean)
                  a = 1, # the active value for the exposure (i.e. prenatal stress = + 1sd)
                  mval = list(0), # control value for mediator (i.e. postnatal stress = mean).
                  estimation = "imputation", # method for estimating causal effects. paramfunc is alternative. 
                  inference = "bootstrap", # method for estimating standard errors of causal effects. delta is alternative. 
                  nboot = 500)
    return(summary(mod)$summarydf)
  }
  
  res <- lapply(datlist, cma)
  
  sum <- Reduce("+",res) / length(res)
  
  return(sum)
    
} 


mods <- list()
meds <- list()

pren_domains <- c('pre_life_events','pre_contextual_risk','pre_parental_risk','pre_interpersonal_risk')
post_domains <- c('post_life_events','post_contextual_risk','post_parental_risk','post_interpersonal_risk','post_direct_victimization')

dom_formula <- function(dom_list) paste0(paste0(dom_list, '_z'), collapse=' + ')

for (out in c(paste0(cardio_outcomes,'_bsaz'), paste0(cardio_outcomes,'_10y'))) {
    
    # BASE MODEL -----------------------------------------------------------
    pre <- fit_reg('prenatal_stress_z', out) 
    pos <- fit_reg('postnatal_stress_z', out)
    add <- fit_reg('prenatal_stress_z + postnatal_stress_z', out, 
                   non_linear = FALSE)
    # int <- fit_reg('prenatal_stress_z * postnatal_stress_z', out)
    pre_dom <- fit_reg(dom_formula(pren_domains), out, non_linear = FALSE)
    pos_dom <- fit_reg(dom_formula(post_domains), out, non_linear = FALSE)
    
    mods[[paste0(out,'_M0')]] <- rbind(pre, pos, add, pre_dom, pos_dom) 
    
    # CAUSAL MEDIATION ANALYSIS --------------------------------------------
    if (pre[2, 'p.value'] < 0.05 | pos[2, 'p.value'] < 0.05) {
      meds[[out]] = fit_med(out)
    }
    
    # CONFOUNDER MODEL -----------------------------------------------------
    m1_adj <- '+ sex + age_mri_10y + ethnicity_cat + mom_BMI_pre_pregnancy + mom_smoking'
    pre <- fit_reg('prenatal_stress_z', out, covs=m1_adj) 
    pos <- fit_reg('postnatal_stress_z', out, covs=m1_adj)
    add <- fit_reg('prenatal_stress_z + postnatal_stress_z', out, covs=m1_adj, 
                   non_linear = FALSE)
    pre_dom <- fit_reg(dom_formula(pren_domains), out, covs=m1_adj, non_linear = FALSE)
    pos_dom <- fit_reg(dom_formula(post_domains), out, covs=m1_adj, non_linear = FALSE)
   
    mods[[paste0(out,'_M1')]] <- rbind(pre, pos, add, pre_dom, pos_dom) 
}

openxlsx::write.xlsx(mods, file = file.path(datapath, paste0('../Results/results.xlsx')),
                     overwrite = T)

openxlsx::write.xlsx(meds, file = file.path(datapath, paste0('../Results/results_cma.xlsx')),
                     overwrite = T)

# Add sex stratified models ====================================================

sex_strata <- list()

covs_no_sex <-  '+ age_mri_10y + ethnicity_cat + mom_BMI_pre_pregnancy + mom_smoking'

for (strata in c('boy','girl')) {
  
  subd <- mice::filter(sample, sex == strata)
  
  for (out in paste0(cardio_outcomes,'_bsaz')) { 
    # BASE MODEL -----------------------------------------------------------
    pre <- fit_reg('prenatal_stress_z', out, covs = covs_no_sex, data = subd) 
    pos <- fit_reg('postnatal_stress_z', out, covs = covs_no_sex, data = subd)
    add <- fit_reg('prenatal_stress_z + postnatal_stress_z', out, 
                   covs = covs_no_sex, data = subd, non_linear = FALSE)
  
    sex_strata[[paste0(out,'_',substr(strata,1,1))]] <- rbind(pre, pos, add) 
  }
}
  
openxlsx::write.xlsx(sex_strata, 
                     file = file.path(datapath, paste0('../Results/results_sex_stratified.xlsx')),
                     overwrite = T)
# ==============================================================================
# FDR correction
# ==============================================================================

res_path <- file.path("..", "Results","results.xlsx")

library(readxl)
library(purrr)
results_raw <- map(set_names(excel_sheets(res_path)), read_excel, path = res_path)

adjust_pvalues <- function(models, terms=c("prenatal_stress_z", "postnatal_stress_z"), 
                           method="fdr", results=results_raw){
  
  for (t in terms){
    # cat(t,'\n')
    ps <- list()
    for (m in models) {
      restab <- data.frame(results[[m]])
      ps[[paste(m,t)]] <- restab[grepl(t, restab$term), "p.value"]
      
    }
    out <- data.frame(unlist(ps))
    out[,'new_ps'] <- p.adjust(unlist(ps), method=method)
    out[,'sign'] <- ifelse(out[,'new_ps'] < 0.05, "*", ifelse(out[,'new_ps'] < 0.06, "-",""))
    
    # print(out)
    
    # Write this back to results
    for (r in row.names(out)) {
      which_model = stringr::str_split_i(r, " ", 1)
      
      which_terms = stringr::str_split_i(r, " ", 2)
      which_term = substr(which_terms, 1, nchar(which_terms)-1)
      
      submodel_names = c(paste(which_term, "-"), "prenatal_stress_z + postnatal_stress_z -" )
      which_submodel = submodel_names[as.integer(substr(which_terms, nchar(which_terms), nchar(which_terms)))]

      if (!"fdr_p" %in% names(results[[which_model]])) {
        results[[which_model]]$fdr_p <- NA
      }
      
      # print(results[[which_model]]$fdr_p[(results[[which_model]]$term == which_term) & 
      #                                (grepl(which_submodel, results[[which_model]]$model)) ]) 
      # print(out[r, 'new_ps'])
      results[[which_model]]$fdr_p[(results[[which_model]]$term == which_term) & 
                                   (startsWith(results[[which_model]]$model, which_submodel))] <- out[r, 'new_ps']

    }
    
  }
  return(results)
}

# adjust_pvalues(names(results)[grep("bsaz", names(results))])

a1 = adjust_pvalues(names(results_raw)[grep("volume_bsaz", names(results_raw))])

a2 = adjust_pvalues(names(results_raw)[grep("mass_bsaz", names(results_raw))],
                    results = a1)
a3 = adjust_pvalues(names(results_raw)[grep("fraction_bsaz", names(results_raw))], 
                    results = a2)

openxlsx::write.xlsx(a3,
                     file = '../Results/results_fdr.xlsx',
                     overwrite = T)
