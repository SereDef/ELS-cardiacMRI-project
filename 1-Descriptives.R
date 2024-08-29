# DESCRIPTIVES =================================================================

invisible(lapply(c('dplyr','mice','miceadds'), require, character.only = TRUE));

datapath <- '../Data'

# Read in imputed datasets -----------------------------------------------------
# Pick last (most recent) file
impset <- readRDS(tail(list.files(path = datapath, pattern = 'impset_bsaz_', 
                             full.names = TRUE),1))

elsset <- readRDS(tail(list.files(path = datapath, pattern = 'ELS_impset_', 
                             full.names = TRUE),1))

pren_domains <- c('pre_life_events','pre_contextual_risk','pre_parental_risk','pre_interpersonal_risk')
post_domains <- c('post_life_events','post_contextual_risk','post_parental_risk','post_interpersonal_risk','post_direct_victimization')

# Merge the datasets -----------------------------------------------------------
ld_outc <- complete(impset, action = 'long', include = TRUE) 
ld_expo <- complete(elsset, action = 'long', include = TRUE) 

if (identical(ld_outc$IDC, ld_outc$IDC)) { # Check same order
  ld_all <- cbind(ld_outc, ld_expo[,c(
    'pre_percent_missing', 'post_percent_missing',
    'prenatal_stress', 'postnatal_stress', pren_domains, post_domains)])
}

# Filter sample ----------------------------------------------------------------

verbose_filter <- function(dataframe, ...) {
  df <- dataframe
  vars = as.list(substitute(list(...)))[-1L]
  
  m = 21 # for stacked (multiple imputation) datasets (else m=1)
  
  for(arg in vars) {
    dataframe <- df
    dataframe_new <- dataframe %>% filter(!!arg)
    rows_filtered <- (nrow(df) - nrow(dataframe_new))/m
    new_df_size <- nrow(dataframe_new)/m
    cat(sprintf('Filtered out %s rows using: %s. New data size = %s\n', rows_filtered, deparse(arg), new_df_size))
    df = dataframe_new
  }
  return(dataframe_new)
}

select_sibling <- function(dt, column_selection = c(), random = FALSE, seed = 31081996, 
                           mother_id = 'MOTHER', child_id = 'IDC') {
  # if no selection is specified, missingness in the entire dataframe is used
  if (length(column_selection) > 0) { dt <- dt[, c(child_id, mother_id, column_selection)] } 
  # First randomly shuffle the dataset 
  set.seed(seed)
  dt <- dt[sample(nrow(dt)),]
  # Get rid of empty NA values for mother
  dt <- dt[!is.na(dt[,mother_id]),]
  # Determine a list of children that have a sibling in the set
  if (random) { 
    sibling_ids <- dt[duplicated(dt[, mother_id]), child_id] # i.e.  which mother IDs recur more than once
  } else {
    dt$missing <- rowSums(is.na(dt)) # compute how many missing values in the columns of interest 
    dt_ord <- dt[order(dt$missing, decreasing=FALSE),] # order based on number of missing (less missing first)
    sibling_ids <- dt_ord[duplicated(dt_ord[, mother_id]), child_id] # selection the child with more missings
  }
  message(length(sibling_ids), ' siblings identified.')
  return(sibling_ids)
}

sibs_to_exclude <- ld_all %>% filter(.imp == 0) %>%  # original dataset 
  select_sibling()

# 
sample <- ld_all %>%
  # Refactor ethnicity 
  mutate(
    ethnicity_cat = relevel(factor(case_when(
      ethnicity %in% c('Dutch','European') ~ 'European',
      ethnicity == 'Turkish' ~ 'Turkish',
      ethnicity == 'Moroccan' ~ 'Moroccan',
      ethnicity == 'Dutch Antilles' ~ 'Dutch Antillean',
      ethnicity == 'Surinamese-Hindustani' ~ 'Surinamese-Hindustani',
      ethnicity == 'Surinamese-Creole' ~ 'Surinamese-Creole',
      ethnicity == 'Cape Verdian' ~ 'Cape Verdian',
      ethnicity %in% c('Asian, non western',
                       'African',
                       'American, non western',
                       'Surinamese-Unspecified',
                       'American,western',
                       'Indonesian',
                       'Oceanie',
                       'Asian, western') ~ 'Other',
      TRUE ~ NA
      )), ref='European'),
    
    ethnicity_cat_better = relevel(factor(case_when(
      ethnicity == 'Dutch' ~ 'Dutch',
      ethnicity == 'European' ~ '(other) European',
      ethnicity %in% c('Cape Verdian',
                        'Moroccan',
                        'Turkish',
                        'African') ~ 'Africa and Middle East',
       ethnicity %in% c('Dutch Antilles',
                        'Surinamese',
                        'American, non western') ~ 'Latin America',
       ethnicity %in% c('Indonesian',
                        'Asian, western',
                        'Asian, non western') ~ 'South/Central/East Asia',
       ethnicity %in% c('American,western',
                        'Oceanie') ~ 'North America or Oceania',
       TRUE ~ NA
      )), ref='Dutch')) %>%
  
  # Remove more than 50% missing, twins and 1 of each sibling couple
  verbose_filter(pre_percent_missing < 50, 
                 post_percent_missing < 50, 
                 twin == 'No',
                 !IDC %in% sibs_to_exclude)  %>% 
  # Convert back to mids
  mice::as.mids()

# Compute sample ELS z-scores --------------------------------------------------
els_scores <- c("prenatal_stress", "postnatal_stress", pren_domains, post_domains)
sample <- datlist2mids( scale_datlist( mids2datlist(sample), orig_var = els_scores, 
                                       trafo_var = paste0(els_scores, "_z")))

# ==============================================================================
ld <- complete(sample, action = 'long', include = FALSE) 

# DESCIPTIVES ==================================================================
  
cont_summ <- function(cont_vars) {
  summ <- ld %>% 
    select(.imp, all_of(cont_vars)) %>%
    group_by(.imp) %>%
    summarise_all(list('med'=median, 'min'=min, 'max'=max)) %>%
    summarise_at(vars(-.imp), mean) %>% 
    round(., 2) %>% 
    matrix(nrow=length(cont_vars), ncol=3, dimnames = list(cont_vars, c('med','min','max'))) %>% 
    as.data.frame() %>% transmute(levels = cont_vars, summary = paste0(med, ' (', min, ', ', max, ')'))
}

cate_summ <- function(cate_var) {
  summ <- ld %>% 
    select(.imp, !!as.name(cate_var)) %>%
    group_by(.imp) %>%
    count(!!as.name(cate_var)) %>% 
    group_by(!!as.name(cate_var)) %>% 
    summarise_at(vars(-.imp), mean) %>% 
    transmute(levels = paste(cate_var, !!as.name(cate_var)), 
              summary = paste0(n, ' (', round((n / (nrow(ld)/20))*100, 1), '%)'))
}

cardio_outcomes <- c('LV_mass', 'LV_end_diastolic_volume', 'RV_end_diastolic_volume',
                     'LV_ejection_fraction','RV_ejection_fraction')

summ1 <- cont_summ(c(paste0(cardio_outcomes,'_bsaz'), 
                     paste0(c(cardio_outcomes,'age_mri','weight_mri','BSA_mri',
                              'heart_rate_mri'),'_10y'), 'mom_BMI_pre_pregnancy',
                     els_scores, paste0(els_scores, '_z')))

other <- c('sex','ethnicity_cat','ethnicity','mom_smoking','mom_education','dad_education','household_income')

summ2 <- do.call(rbind, lapply(other,cate_summ))

total_summ <- rbind(summ1,summ2)

write.csv(total_summ, '../Results/descriptives.csv')

# Save sample ------------------------------------------------------------------
saveRDS(sample, file.path(datapath, 'impset_sample.rds'))

# For plotting -----------------------------
datalist <- split(ld, as.factor(ld[,'.imp']))
dir.create('../Data/df_by_imp')
for (m in names(datalist)) { write.csv(datalist[[m]], paste0('../Data/df_by_imp/Data_imp',m,'.csv'))}

write.csv(complete(sample, action=0), paste0('../Data/df_by_imp/Data_imp0.csv'))


