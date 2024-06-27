# Required packages
invisible(lapply(c('foreign','dplyr','mice'), # 'gamlss','rms' later cause MASS messes with select
                 require, character.only = TRUE));

datapath <- '../Data'
# 
# readsav <- function(file, summary=FALSE) {
#   d <- foreign::read.spss(file.path(datapath, file), use.value.labels = TRUE, to.data.frame = TRUE)
#   # labels <- attr(d, 'variable.labels')
#   # make sure missing are read in correctly
#   for (col in d) { if (max(as.numeric(col), na.rm=TRUE) == 999) { d[col == 999,] <- NA } }
#   if (summary) { print(summary(d)) }
#   return(d)
# }
# # LOAD DATA ====================================================================
# 
# hrt <- readsav('CHILDMRICARDIO_standardizedbsa_12102018.sav') %>%
#   select(IDC,   'age_mri' = 'agechildMRI9',
#              'weight_mri' = 'weightMRI9',
#                 'BSA_mri' = 'BSAchild9', # 1286 missing
#           # _without outliers 12-10-2018 (all below)
#          'heart_rate_mri' = 'HeartRate.MRI9', # 1289 missing
# 'LV_end_diastolic_volume' = 'LVEDVolume.MRI9', # 1288 missing # + bsa zscore
#  'LV_end_systolic_volume' = 'LVESVolume.MRI9', # 1288 missing
#        'LV_stroke_volume' = 'LVStrokeVolume.MRI9', # 1286 missing
#    'LV_ejection_fraction' = 'LVEjectionfraction.MRI9', # 1287 missing # + bsa zscore
#                 'LV_mass' = 'Mass.MRI9', # 1288 missing # + bsa zscore # LV???
#          'cardiac_output' = 'CO.MRI9', # 1288 missing
# 'RV_end_diastolic_volume' = 'RVEDvolume.MRI9', # 1288 missing # + bsa zscore
#  'RV_end_systolic_volume' = 'RVESvolume.MRI9', # 1288 missing
#        'RV_stroke_volume' = 'RVStrokevolume.MRI9', # 1288 missing
#    'RV_ejection_fraction' = 'RVEjectionfraction.MRI9' # 1288 missing # + bsa zscore
#   # Add median age to variable names
#   ) %>% rename_at(vars(-IDC), function(x) paste0(x, '_10y'))
# 
# # ------------------------------------------------------------------------------
# 
# # Other cardiac MRI dataset (?)
# # hrt2 <- readsav('CHILDMRICARDIO9_08122016.sav') %>%
#    # Heart rate intern & PIA (with outliers?)
#    # LV end-diastolic volume: clean (without outliers), PIA, intern
#    # LV end-systolic volume: clean (without outliers), PIA, intern
#    # LV stroke volume: clean (without outliers), PIA, intern
#    # LV ejection fraction: clean (without outliers), PIA, intern
#    # RV end-diastolic volume: clean (without outliers), PIA, intern
#    # RV end-systolic volume: clean (without outliers), PIA, intern
#    # RV stroke volume: clean (without outliers), PIA, intern
#    # RV ejection fraction: clean (without outliers), PIA, intern
#    # LV mass: clean (without outliers), PIA, intern
#    # Cardiac output: clean (without outliers), PIA, intern
#    # Aorta: ann, sin, mid_asc, mid_desc, dis_desc
# 
# # 'CHILDMRICARDIO9inclZ_14122016.sav' ?
# 
# # Heart ultrasound data
# # hrt_us <- readsav('CHILDHEART9_16032016.sav')
# 
# # ------------------------------------------------------------------------------
# # General data
# gen <- readsav('CHILD-ALLGENERALDATA_15012024.sav') %>%
#   # Only select children alive at birth
#   filter(OUTCOMECHILD == 'live birth') %>%
#   # Combine repeated measures
#   mutate(mom_education = case_when(!is.na(EDUCM5) ~ EDUCM5, # GR1075 Education mother based on 5/6 year questionnaire
#                                    !is.na(EDUCM3) ~ EDUCM3, # GR1065 Education mother based on 36 months questionnaire
#                                    TRUE ~ EDUCM), # Education level mother: highest education finished (pregnancy?)
#          dad_education = case_when(!is.na(EDUCP5) ~ EDUCP5, # GR1075 Education partner based on 5/6 year questionnaire
#                                    !is.na(EDUCP3) ~ EDUCP3, # GR1066 Education partner based on 36 months questionnaire
#                                    TRUE ~ EDUCP), # education level partner (pregnancy?)
#          marital_status = factor(case_when(!is.na(MAR_DICH5) ~ MAR_DICH5,
#                                     MARDICH == 'married/living together' ~ 'married/registered partnership/living together',
#                                     MARDICH == 'no partner' ~ 'no partner (no partner at all/partner with whom i do not live)',
#                                     TRUE ~ NA)),
#          household_income = factor(case_when(!is.na(INCOME5) ~ INCOME5,
#                                       INCOME %in% c('less than 450','450-600 euro','600-700 euro','700-800 euro') ~ 'Less than € 800',
#                                       INCOME %in% c('800-900 euro','900-1200 euro') ~ '€ 800-1200',
#                                       INCOME %in% c('1200-1400 euro','1400-1600 euro') ~ '€ 1200-1600',
#                                       INCOME %in% c('1600-1800 euro','1800-2000 euro') ~ '€ 1600-2000',
#                                       INCOME == '2000-2200 euro' ~ '€ 2000-2400',
#                                       INCOME == '> 2200 euro' ~ '€ 2800-3200'),
#                                    levels=levels(INCOME5) # maintain the original order
#                                    )) %>%
#   # Select and rename variables
#   select(IDC, IDM, MOTHER, # for sibling detection
#          mom_education, dad_education, marital_status, household_income,
#                   'sex' = 'GENDER', # Gender live birth corrected 10-12-2014
#             'ethnicity' = 'ETHNINFv3', # CBS ethnicity child update after focus@5 - Surinamese subdivided
#               'mom_age' = 'AGE_M_v2', # Age mother at intake corrected 13-4-2016
# 'mom_BMI_pre_pregnancy' = 'BMI_0', # bmi before pregnancy
#                'parity' = 'PARITY',
#                  'twin' = 'TWIN',
# 'birth_gestational_age' = 'GESTBIR',
#          'birth_weight' = 'WEIGHT')
# # ------------------------------------------------------------------------------
# # Prenatal maternal health and lifestyle
# 
# maternal_smoking <- readsav('MATERNALSMOKING_22112016.sav') %>%
#   # Select and rename variables
#   select('IDM' = 'idm', 'mom_smoking' = 'SMOKE_ALL')
# 
# pregnancy_compli <- readsav('MATERNALCOMPLICATIONS_22112016.sav') %>%
#   # Select and rename variables
#   select(IDM, 'mom_preclampsia' = 'PE', # PE - definition Sarah Timmermans
#        'mom_diabetes_pregnancy' = 'DIAB_GRA', # Diabetes Gravidarum - unvalidated
#    'mom_hyperten_pre_pregnancy' = 'HYPERTENSIE_QUEST', # preexisting hypertension, based on questionnaire GR1001 and Medical records
#        'mom_hyperten_pregnancy' = 'PIH_v1') # Pregnancy Induced Hypertension
# 
# gen <- Reduce(function(x,y) merge(x = x, y = y, by = 'IDM', all.x=T),
#                   list(gen, maternal_smoking, pregnancy_compli))
# 
# # ------------------------------------------------------------------------------
# # Blood pressure data
# bp6 <- readsav('CHILDBLOODPRESSURE5_10022012.sav') %>%
#   select(IDC, 'age_bp' = 'ageChildYF5',
#                  'SBP' = 'MeanSBP_ex1_5child', # Mean Systole  excl first measurement
#                  'DBP' = 'MeanDBP_ex1_5child'  # Mean Diastole excl first measurement
#          # ALSO: pulse and MAP
#          # Add median age to variable names
#   ) %>% rename_at(vars(-IDC), function(x) paste0(x, '_6y'))
# 
# bp10 <- readsav('CHILDBLOODPRESSURE9_21042016.sav') %>%
#   select(IDC, 'age_bp' = 'agechild9_visit1',
#                  'SBP' = 'MEANSBP_ex1_child9', # Mean Systole  excl first measurement
#                  'DBP' = 'MEANDBP_ex1_child9'  # Mean Diastole excl first measurement
#          # ALSO: pulse and MAP
#          # Add median age to variable names
#   ) %>% rename_at(vars(-IDC), function(x) paste0(x, '_10y'))
# # ------------------------------------------------------------------------------
# 
# # BMI data
# bmi6 <- readsav('CHILDGROWTH5_10122014.sav') %>%
#   select(IDC, 'age_bmi' = 'agey5child',
#                   'BMI' = 'BMI5child', # F@5 GROWTH - child: BMI
#          # ALSO: The Netherlands 1997, BMI for age corrected 10-12-2014
#          # Height (cm and m), weight (kg), head circumference (cm) + sds for age
#          # Add median age to variable names
#   ) %>% rename_at(vars(-IDC), function(x) paste0(x, '_6y'))
# 
# bmi10 <- readsav('CHILDGROWTH9_06072021.sav') %>%
#   select(IDC, 'age_bmi' = 'agechild9',
#                   'BMI' = 'bmichild9', # F9 GROWTH - child: bmi
#                'height' = 'heightchild9', # F9 GROWTH - child: height (cm)
#          # ALSO: Netherlands, the 1997 (Netherlands, the) / Fredriks
#          # Weight (kg) + sds for age
#          # Add median age to variable names
#   ) %>% rename_at(vars(-IDC), function(x) paste0(x, '_10y'))
# 
# bmi13 <- readsav('CHILDGROWTH13_10122020.sav') %>%
#   select(IDC, 'age_bmi' = 'AGECHILD13',
#                   'BMI' = 'bmichild13', # BMI (kg/m2) child F@13
#          # ALSO: Netherlands, the 1997 (Netherlands, the) / Fredriks
#          #       Netherlands, the 2010 (Netherlands, the) / Talma
#          # Height, weight + sds for age
#          # Add median age to variable names
#   ) %>% rename_at(vars(-IDC), function(x) paste0(x, '_13y'))
# 
# #pren <- readRDS(file.path(datapath, 'prenatal_stress.rds'))
# #post <- readRDS(file.path(datapath, 'postnatal_stress.rds'))
# 
# files <- list(gen, bp6, bmi6, bmi10, bp10, bmi13, hrt)
# 
# dataset <- Reduce(function(x,y) merge(x = x, y = y, by = 'IDC', all.x=T),
#                   files)
# rm(files)
# 
# saveRDS(dataset, file.path(datapath, paste0('dataset_og_',as.character(Sys.Date()),'.rds')))

# ==============================================================================
# Inspect correlations 

# dc <- dataset %>% select(where(is.numeric))
# dd <- dataset[, !names(dataset)%in%names(dc)]
# 
# dc <- dc %>% select(!c(IDC, MOTHER))
# m = cor(dc, use='pairwise.complete.obs', method='spearman')
# 
# m[abs(m) < 0.2] <- 0 # threshold
# dim(m)
# tm <- m[rowSums(abs(m), na.rm = TRUE) > 1, colSums(abs(m), na.rm = TRUE) > 1]
# dim(tm)
# 
# sink('qpred.txt')
# lapply(row.names(tm), function(x) { 
#   cat(x,'\n')
#   print(round(sort(m[x, which(m[x,] > 0)], decreasing = TRUE), 2))
#   cat('\n\n') } )
# sink()

# write.csv(tm, 'corrmat.csv', row.names = T)

# IMPUTATION ===================================================================

dsets <- list.files(path = datapath, pattern = 'dataset_og_', full.names = TRUE)
dataset <- readRDS(dsets[length(dsets)])

# Random forest imputation 
start.time <- Sys.time()
imp_rf <- mice(dataset, method = 'rf', m = 20, maxit = 40, ntree = 10) 
end.time <- Sys.time()

time.taken <- round(end.time - start.time,2)
time.taken

imp_rf$loggedEvents

saveRDS(imp_rf, file.path(datapath, paste0('impset_og_',as.character(Sys.Date()),'.rds')))

pdf('QC-imputation_cardiacMRI.pdf')
for (v in names(dataset)) { if (nrow(imp_rf$imp[[v]]) > 1) {
  message(v)
  
  nmiss <- sum(is.na(imp_rf$data[v]))
  nmiss <- paste0('\n n missing = ', nmiss, ' (',round(nmiss/nrow(imp_rf$data)*100,1),'%)')
  
  try(print(mice::densityplot(imp_rf, as.formula(paste('~',v)), main=paste(v, nmiss))))
  }
}
dev.off()

# ------------------------------------------------------------------------------
# imp_rf <- readRDS(list.files(path = datapath, pattern = 'impset_og_', full.names = TRUE))

# BSA-ADJUSTED Z-SCORES ========================================================

invisible(lapply(c('gamlss','rms'), require, character.only = TRUE));

# Based on Z-scoresonbsaPIANEW script, define:
# 1. degrees of freedom for smoothing 
# 2. sigma formula
# 3. family used to define the distribution and link functions
vars <- list('LV_mass' = list(3, ~1, gamlss.dist::BCCG()),
             'LV_end_diastolic_volume' = list(3, ~BSA_mri_10y, gamlss.dist::BCT()),
             'RV_end_diastolic_volume' = list(3, ~1, gamlss.dist::BCT()),
             'LV_ejection_fraction' = list(3, ~1, gamlss.dist::BCPE()),
             'RV_ejection_fraction' = list(0, ~1, gamlss.dist::BCPE()) )

bsa_zscore <- function(dset) {
  
  # Summaries of variables for effect and plotting ranges, values to adjust to...
  suppressWarnings(dd <- rms::datadist(dset))
  options(datadist = 'dd') # store info with model fit
  
  dset[, paste0(names(vars),'_bsaz')] <- NA # Empty placer columns
  
  for (v in names(vars)) {
    # Sample 
    dsub <- dset[, paste0(c(v,'BSA_mri'), '_10y')]
    dsub <- na.omit(dsub)
    
    # Parameters
    df <- vars[[v]][[1]]
    sf <- as.formula(vars[[v]][[2]])
    fam <- vars[[v]][[3]]
    
    # Model
    f <- as.formula(paste0(v, '_10y ~ gamlss::cs(BSA_mri_10y, ',df,')'))
    m <- gamlss::gamlss(f, sigma.formula=sf, data=dsub, family=fam)
    
    # Residuals 
    zx <- residuals(m, what = c('z-scores'), type = c('simple'), terms = NULL)
    
    if (nrow(dsub)==nrow(dset)) { dset[, paste0(v,'_bsaz')] <- zx } else { 
      # imp = 0 (original dataset)
      dset[(!is.na(dset[paste0(v,'_10y')])) & (!is.na(dset['BSA_mri_10y'])), 
           paste0(v,'_bsaz')] <- zx
    }
      
    message(v)
    print(summary(zx))
    
  }
  return(dset)
}

imp_bsaz <- complete(imp_rf, action = 'long', include = TRUE) %>%
  group_by(.imp) %>%
  group_modify(~ bsa_zscore(.))

imp_bsaz <- as.mids(imp_bsaz)

saveRDS(imp_bsaz, file.path(datapath, paste0('impset_bsaz_',as.character(Sys.Date()),'.rds')))

# ==============================================================================
