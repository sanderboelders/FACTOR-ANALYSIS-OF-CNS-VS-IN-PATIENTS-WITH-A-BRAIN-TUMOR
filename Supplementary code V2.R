# ============= Number of factors ============= #

library('psych')
library('nFactors')
library(EFA.dimensions)
library("REdaS")
library("xlsx")
library("mice")
library(EFAtools)

# Load test data
lgg = read.xlsx('PATH_TO_EXCEL.xlsx', 1)
hgg = read.xlsx('PATH_TO_EXCEL.xlsx', 1)
men = read.xlsx('PATH_TO_EXCEL.xlsx', 1)
norm = read.xlsx("PATH_TO_EXCEL.xlsx", 1)

# Set datatypes and column names
cns_vs_varnames = c('verm_initial_correct_hits', 'verm_initial_correct_passes', 'verm_delayed_correct_hits', 'verm_delayed_correct_passes', 'vism_initial_correct_hits', 'vism_initial_correct_passes', 'vism_delayed_correct_hits', 'vism_delayed_correct_passes', 'sdc_correct', 'sdc_errors', 'stroop_simple_reaction_time', 'stroop_stroop_correct', 'stroop_stroop_reaction_time_correct', 'stroop_stroop_commission_errors', 'stroop_complex_correct', 'stroop_complex_reaction_time_correct', 'stroop_complex_commission_errors', 'sat_average_correct_rt', 'cpt_omission_errors', 'cpt_commission_errors', 'cpt_choice_reaction_time_correct', 'sat_correct.AND.sat_errors', 'ftt_right_taps_average', 'ftt_left_taps_average')

for (colname in cns_vs_varnames) {
  lgg[, colname] <- as.numeric(as.character(lgg[, colname]))
  hgg[, colname] <- as.numeric(as.character(hgg[, colname]))
  men[, colname] <- as.numeric(as.character(men[, colname]))
  norm[, colname] <- as.numeric(as.character(norm[, colname])) }

lgg = lgg[cns_vs_varnames]
lgg['from'] = 'lgg'
hgg = hgg[cns_vs_varnames]
hgg['from'] = 'hgg'
men = men[cns_vs_varnames]
men['from'] = 'men'
norm = norm[cns_vs_varnames]
norm['from'] = 'norm'

data = do.call("rbind", list(lgg, hgg, men, norm))
aggregate(data, by = list(data$from), FUN = mean)

names(data)[names(data) == "sat_correct.AND.sat_errors"] <- "sat_correct_AND_sat_errors"
names(data)[names(data) == "ftt_right_taps_average.AND.ftt_left_taps_average"] <- "ftt_right_taps_average_AND_ftt_left_taps_average"
data_hgg = subset(data, from == 'hgg')
data_lgg = subset(data, from == 'lgg')
data_norm = subset(data, from == 'norm')
data_men = subset(data, from == 'men')

cns_vs_varnames_forordered = c('verm_initial_correct_hits', 'verm_initial_correct_passes', 'verm_delayed_correct_hits', 'verm_delayed_correct_passes', 'vism_initial_correct_hits', 'vism_initial_correct_passes', 'vism_delayed_correct_hits', 'vism_delayed_correct_passes', 'sdc_correct', 'sdc_errors', 'stroop_simple_reaction_time', 'stroop_stroop_correct', 'stroop_stroop_reaction_time_correct', 'stroop_stroop_commission_errors', 'stroop_complex_correct', 'stroop_complex_reaction_time_correct', 'stroop_complex_commission_errors', 'sat_average_correct_rt', 'cpt_omission_errors', 'cpt_commission_errors', 'cpt_choice_reaction_time_correct', 'sat_correct_AND_sat_errors', 'ftt_right_taps_average', 'ftt_left_taps_average')

dataset <- apply(data[, cns_vs_varnames_forordered], 2,            # Specify own function within apply
                    function(x) as.numeric(as.character(x)))
data_hgg <- apply(data_hgg[, cns_vs_varnames_forordered], 2,            # Specify own function within apply
                    function(x) as.numeric(as.character(x)))
data_lgg <- apply(data_lgg[, cns_vs_varnames_forordered], 2,            # Specify own function within apply
                    function(x) as.numeric(as.character(x)))
data_norm <- apply(data_norm[, cns_vs_varnames_forordered], 2,            # Specify own function within apply
                    function(x) as.numeric(as.character(x)))
data_men <- apply(data_men[, cns_vs_varnames_forordered], 2,            # Specify own function within apply
                    function(x) as.numeric(as.character(x)))

# Test for sphericity
bart_spher(dataset, use = "pairwise.complete.obs")
bart_spher(data_hgg, use = "pairwise.complete.obs")
bart_spher(data_lgg, use = "pairwise.complete.obs")
bart_spher(data_norm, use = "pairwise.complete.obs")
bart_spher(data_men, use = "pairwise.complete.obs")

# KMO score. Does not accept missings or multiple mputation so impute once
dataset_c <- complete(mice(dataset,m=),1)
data_hgg_c <- complete(mice(data_hgg,m=1),1)
data_lgg_c <- complete(mice(data_lgg,m=1),1)
data_norm_c <- complete(mice(data_norm,m=1),1)
data_men_c <- complete(mice(data_men,m=1),1)

KMO(dataset_c)
KMO(data_hgg_c)
KMO(data_lgg_c)
KMO(data_norm_c)
KMO(data_men_c)

N_FACTORS(dataset_c, method = "ML")
N_FACTORS(data_hgg_c, method = "ML")
N_FACTORS(data_lgg_c, method = "ML")
N_FACTORS(data_norm_c, method = "ML")
N_FACTORS(data_men_c, method = "ML")



# ============= Exploratory factor analysis ============= #

library('psych')
library('GPArotation')
library("xlsx")
library(dplyr)
library(mifa)

lgg = read.xlsx('PATH_TO_EXCEL..xlsx', 1)
hgg = read.xlsx('PATH_TO_EXCEL..xlsx', 1)
men = read.xlsx('PATH_TO_EXCEL..xlsx', 1)
norm = read.xlsx("PATH_TO_EXCEL..xlsx", 1)

cns_vs_varnames = c('verm_initial_correct_hits', 'verm_initial_correct_passes', 'verm_delayed_correct_hits', 'verm_delayed_correct_passes', 'vism_initial_correct_hits', 'vism_initial_correct_passes', 'vism_delayed_correct_hits', 'vism_delayed_correct_passes', 'sdc_correct', 'sdc_errors', 'stroop_simple_reaction_time', 'stroop_complex_correct', 'stroop_complex_reaction_time_correct', 'stroop_complex_commission_errors',  'stroop_stroop_correct', 'stroop_stroop_reaction_time_correct', 'stroop_stroop_commission_errors', 'cpt_omission_errors', 'cpt_commission_errors', 'cpt_choice_reaction_time_correct','sat_average_correct_rt', 'sat_correct.AND.sat_errors', 'ftt_right_taps_average', 'ftt_left_taps_average')

lgg = lgg[cns_vs_varnames]
hgg = hgg[cns_vs_varnames]
men = men[cns_vs_varnames]
norm = norm[cns_vs_varnames]
combined = do.call("rbind", list(lgg, hgg[sample(1:nrow(hgg), 99), ], men[sample(1:nrow(men), 99), ]))

names(lgg)[names(lgg) == "sat_correct.AND.sat_errors"] <- "sat_correct_AND_sat_errors"
names(lgg)[names(lgg) == "ftt_right_taps_average.AND.ftt_left_taps_average"] <- "ftt_right_taps_average_AND_ftt_left_taps_average"
names(hgg)[names(hgg) == "sat_correct.AND.sat_errors"] <- "sat_correct_AND_sat_errors"
names(hgg)[names(hgg) == "ftt_right_taps_average.AND.ftt_left_taps_average"] <- "ftt_right_taps_average_AND_ftt_left_taps_average"
names(men)[names(men) == "sat_correct.AND.sat_errors"] <- "sat_correct_AND_sat_errors"
names(men)[names(men) == "ftt_right_taps_average.AND.ftt_left_taps_average"] <- "ftt_right_taps_average_AND_ftt_left_taps_average"
names(norm)[names(norm) == "sat_correct.AND.sat_errors"] <- "sat_correct_AND_sat_errors"
names(norm)[names(norm) == "ftt_right_taps_average.AND.ftt_left_taps_average"] <- "ftt_right_taps_average_AND_ftt_left_taps_average"
names(combined)[names(combined) == "sat_correct.AND.sat_errors"] <- "sat_correct_AND_sat_errors"
names(combined)[names(combined) == "ftt_right_taps_average.AND.ftt_left_taps_average"] <- "ftt_right_taps_average_AND_ftt_left_taps_average"

dataset = hgg

n_factors = 5
dataset <- apply(dataset, 2,            # Specify own function within apply
                 function(x) as.numeric(as.character(x)))


mi_comb <- mifa(
  data      = as.data.frame(data_comb),
  cov_vars  = c('verm_initial_correct_hits', 'verm_initial_correct_passes', 'verm_delayed_correct_hits', 'verm_delayed_correct_passes', 'vism_initial_correct_hits', 'vism_initial_correct_passes', 'vism_delayed_correct_hits', 'vism_delayed_correct_passes', 'sdc_correct', 'sdc_errors', 'stroop_simple_reaction_time', 'stroop_complex_correct', 'stroop_complex_reaction_time_correct', 'stroop_complex_commission_errors',  'stroop_stroop_correct', 'stroop_stroop_reaction_time_correct', 'stroop_stroop_commission_errors', 'cpt_omission_errors', 'cpt_commission_errors', 'cpt_choice_reaction_time_correct','sat_average_correct_rt', 'sat_correct_AND_sat_errors', 'ftt_right_taps_average', 'ftt_left_taps_average'),
  # cov_vars  = c('verm_initial_correct_hits', 'verm_initial_correct_passes', 'verm_delayed_correct_hits', 'verm_delayed_correct_passes', 'vism_initial_correct_hits', 'vism_initial_correct_passes', 'vism_delayed_correct_hits', 'vism_delayed_correct_passes', 'sdc_correct', 'sdc_errors', 'stroop_simple_reaction_time', 'stroop_complex_reaction_time_correct', 'stroop_complex_commission_errors',  'stroop_stroop_correct', 'stroop_stroop_reaction_time_correct', 'stroop_stroop_commission_errors', 'cpt_choice_reaction_time_correct','sat_average_correct_rt', 'sat_correct_AND_sat_errors', 'ftt_right_taps_average', 'ftt_left_taps_average'),
  n_pc      = 2:8,
  ci        = "fieller",
  print     = FALSE
)
mi_lgg <- mifa(
  data      = as.data.frame(lgg),
  cov_vars  = c('verm_initial_correct_hits', 'verm_initial_correct_passes', 'verm_delayed_correct_hits', 'verm_delayed_correct_passes', 'vism_initial_correct_hits', 'vism_initial_correct_passes', 'vism_delayed_correct_hits', 'vism_delayed_correct_passes', 'sdc_correct', 'sdc_errors', 'stroop_simple_reaction_time', 'stroop_complex_correct', 'stroop_complex_reaction_time_correct', 'stroop_complex_commission_errors',  'stroop_stroop_correct', 'stroop_stroop_reaction_time_correct', 'stroop_stroop_commission_errors', 'cpt_omission_errors', 'cpt_commission_errors', 'cpt_choice_reaction_time_correct','sat_average_correct_rt', 'sat_correct_AND_sat_errors', 'ftt_right_taps_average', 'ftt_left_taps_average'),
  # cov_vars  = c('verm_initial_correct_hits', 'verm_initial_correct_passes', 'verm_delayed_correct_hits', 'verm_delayed_correct_passes', 'vism_initial_correct_hits', 'vism_initial_correct_passes', 'vism_delayed_correct_hits', 'vism_delayed_correct_passes', 'sdc_correct', 'sdc_errors', 'stroop_simple_reaction_time', 'stroop_complex_reaction_time_correct', 'stroop_complex_commission_errors',  'stroop_stroop_correct', 'stroop_stroop_reaction_time_correct', 'stroop_stroop_commission_errors', 'cpt_choice_reaction_time_correct','sat_average_correct_rt', 'sat_correct_AND_sat_errors', 'ftt_right_taps_average', 'ftt_left_taps_average'),
  n_pc      = 2:8,
  ci        = "fieller",
  print     = FALSE
)
mi_hgg <- mifa(
  data      = as.data.frame(hgg),
  cov_vars  = c('verm_initial_correct_hits', 'verm_initial_correct_passes', 'verm_delayed_correct_hits', 'verm_delayed_correct_passes', 'vism_initial_correct_hits', 'vism_initial_correct_passes', 'vism_delayed_correct_hits', 'vism_delayed_correct_passes', 'sdc_correct', 'sdc_errors', 'stroop_simple_reaction_time', 'stroop_complex_correct', 'stroop_complex_reaction_time_correct', 'stroop_complex_commission_errors',  'stroop_stroop_correct', 'stroop_stroop_reaction_time_correct', 'stroop_stroop_commission_errors', 'cpt_omission_errors', 'cpt_commission_errors', 'cpt_choice_reaction_time_correct','sat_average_correct_rt', 'sat_correct_AND_sat_errors', 'ftt_right_taps_average', 'ftt_left_taps_average'),
  # cov_vars  = c('verm_initial_correct_hits', 'verm_initial_correct_passes', 'verm_delayed_correct_hits', 'verm_delayed_correct_passes', 'vism_initial_correct_hits', 'vism_initial_correct_passes', 'vism_delayed_correct_hits', 'vism_delayed_correct_passes', 'sdc_correct', 'sdc_errors', 'stroop_simple_reaction_time', 'stroop_complex_reaction_time_correct', 'stroop_complex_commission_errors',  'stroop_stroop_correct', 'stroop_stroop_reaction_time_correct', 'stroop_stroop_commission_errors', 'cpt_choice_reaction_time_correct','sat_average_correct_rt', 'sat_correct_AND_sat_errors', 'ftt_right_taps_average', 'ftt_left_taps_average'),
  n_pc      = 2:8,
  ci        = "fieller",
  print     = FALSE
)
mi_men <- mifa(
  data      = as.data.frame(men),
  cov_vars  = c('verm_initial_correct_hits', 'verm_initial_correct_passes', 'verm_delayed_correct_hits', 'verm_delayed_correct_passes', 'vism_initial_correct_hits', 'vism_initial_correct_passes', 'vism_delayed_correct_hits', 'vism_delayed_correct_passes', 'sdc_correct', 'sdc_errors', 'stroop_simple_reaction_time', 'stroop_complex_correct', 'stroop_complex_reaction_time_correct', 'stroop_complex_commission_errors',  'stroop_stroop_correct', 'stroop_stroop_reaction_time_correct', 'stroop_stroop_commission_errors', 'cpt_omission_errors', 'cpt_commission_errors', 'cpt_choice_reaction_time_correct','sat_average_correct_rt', 'sat_correct_AND_sat_errors', 'ftt_right_taps_average', 'ftt_left_taps_average'),
  # cov_vars  = c('verm_initial_correct_hits', 'verm_initial_correct_passes', 'verm_delayed_correct_hits', 'verm_delayed_correct_passes', 'vism_initial_correct_hits', 'vism_initial_correct_passes', 'vism_delayed_correct_hits', 'vism_delayed_correct_passes', 'sdc_correct', 'sdc_errors', 'stroop_simple_reaction_time', 'stroop_complex_reaction_time_correct', 'stroop_complex_commission_errors',  'stroop_stroop_correct', 'stroop_stroop_reaction_time_correct', 'stroop_stroop_commission_errors', 'cpt_choice_reaction_time_correct','sat_average_correct_rt', 'sat_correct_AND_sat_errors', 'ftt_right_taps_average', 'ftt_left_taps_average'),
  n_pc      = 2:8,
  ci        = "fieller",
  print     = FALSE
)
mi_norm <- mifa(
  data      = as.data.frame(norm),
  cov_vars  = c('verm_initial_correct_hits', 'verm_initial_correct_passes', 'verm_delayed_correct_hits', 'verm_delayed_correct_passes', 'vism_initial_correct_hits', 'vism_initial_correct_passes', 'vism_delayed_correct_hits', 'vism_delayed_correct_passes', 'sdc_correct', 'sdc_errors', 'stroop_simple_reaction_time', 'stroop_complex_correct', 'stroop_complex_reaction_time_correct', 'stroop_complex_commission_errors',  'stroop_stroop_correct', 'stroop_stroop_reaction_time_correct', 'stroop_stroop_commission_errors', 'cpt_omission_errors', 'cpt_commission_errors', 'cpt_choice_reaction_time_correct','sat_average_correct_rt', 'sat_correct_AND_sat_errors', 'ftt_right_taps_average', 'ftt_left_taps_average'),
  # cov_vars  = c('verm_initial_correct_hits', 'verm_initial_correct_passes', 'verm_delayed_correct_hits', 'verm_delayed_correct_passes', 'vism_initial_correct_hits', 'vism_initial_correct_passes', 'vism_delayed_correct_hits', 'vism_delayed_correct_passes', 'sdc_correct', 'sdc_errors', 'stroop_simple_reaction_time', 'stroop_complex_reaction_time_correct', 'stroop_complex_commission_errors',  'stroop_stroop_correct', 'stroop_stroop_reaction_time_correct', 'stroop_stroop_commission_errors', 'cpt_choice_reaction_time_correct','sat_average_correct_rt', 'sat_correct_AND_sat_errors', 'ftt_right_taps_average', 'ftt_left_taps_average'),
  n_pc      = 2:8,
  ci        = "fieller",
  print     = FALSE
)


vars_for_cfa = c('verm_initial_correct_hits', 'verm_initial_correct_passes', 'verm_delayed_correct_hits', 'verm_delayed_correct_passes', 'vism_initial_correct_hits', 'vism_initial_correct_passes', 'vism_delayed_correct_hits', 'vism_delayed_correct_passes', 'sdc_correct', 'sdc_errors', 'stroop_simple_reaction_time', 'stroop_complex_reaction_time_correct', 'stroop_complex_commission_errors',  'stroop_stroop_correct', 'stroop_stroop_reaction_time_correct', 'stroop_stroop_commission_errors', 'cpt_choice_reaction_time_correct','sat_average_correct_rt', 'sat_correct_AND_sat_errors', 'ftt_right_taps_average', 'ftt_left_taps_average')

print('-----')
for (n_factors in c(2,3,4,5,6)) {
  print('lgg')
  print(n_factors)
  efa = fa(mi_lgg$cov_combined, n.obs = nrow(bfi), nfactors = n_factors, n.iter = 1, rotate = "oblimin", scores = "regression",
           residuals = FALSE, SMC = TRUE, covar = FALSE, missing = FALSE,
           min.err = 0.001, max.iter = 50, symmetric = TRUE, warnings = TRUE, fm = "minres",
           alpha = .1, p = .05, oblique.scores = TRUE, np.obs = NULL, use = "pairwise", cor = "cor",
           correct = .5, weight = NULL)
  cfa = sem.mi(convert_efa_to_cfa(efa, treshold=0.3), data = lgg[vars_for_cfa], miPackage = 'mice', estimator = 'MLM', m = 10)
  print(fitMeasures(cfa)[c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.robust","rmsea.robust","srmr_bentler")])
}

print('-----')
for (n_factors in c(2, 3,4,5,6)) {
  print('hgg')
  print(n_factors)
  efa = fa(mi_hgg$cov_combined, n.obs = nrow(bfi), nfactors = n_factors, n.iter = 1, rotate = "oblimin", scores = "regression",
           residuals = FALSE, SMC = TRUE, covar = FALSE, missing = FALSE,
           min.err = 0.001, max.iter = 50, symmetric = TRUE, warnings = TRUE, fm = "minres",
           alpha = .1, p = .05, oblique.scores = TRUE, np.obs = NULL, use = "pairwise", cor = "cor",
           correct = .5, weight = NULL)
  cfa = sem.mi(convert_efa_to_cfa(efa, treshold=0.3), data = hgg[vars_for_cfa], miPackage = 'mice', estimator = 'MLM', m = 10)
  print(fitMeasures(cfa)[c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.robust","rmsea.robust","srmr_bentler")])
}

print('-----')
for (n_factors in c(2, 3,4,5,6)) {
  print('men')
  print(n_factors)
  efa = fa(mi_men$cov_combined, n.obs = nrow(bfi), nfactors = n_factors, n.iter = 1, rotate = "oblimin", scores = "regression",
           residuals = FALSE, SMC = TRUE, covar = FALSE, missing = FALSE,
           min.err = 0.001, max.iter = 50, symmetric = TRUE, warnings = TRUE, fm = "minres",
           alpha = .1, p = .05, oblique.scores = TRUE, np.obs = NULL, use = "pairwise", cor = "cor",
           correct = .5, weight = NULL)
  cfa = sem.mi(convert_efa_to_cfa(efa, treshold=0.3), data = men[vars_for_cfa], miPackage = 'mice', estimator = 'MLM', m = 10)
  print(fitMeasures(cfa)[c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.robust","rmsea.robust","srmr_bentler")])
}

print('-----')
for (n_factors in c(2, 3,4,5,6)) {
  print('norm')
  print(n_factors)
  efa = fa(mi_norm$cov_combined, n.obs = nrow(bfi), nfactors = n_factors, n.iter = 1, rotate = "oblimin", scores = "regression",
           residuals = FALSE, SMC = TRUE, covar = FALSE, missing = FALSE,
           min.err = 0.001, max.iter = 50, symmetric = TRUE, warnings = TRUE, fm = "minres",
           alpha = .1, p = .05, oblique.scores = TRUE, np.obs = NULL, use = "pairwise", cor = "cor",
           correct = .5, weight = NULL)
  cfa = sem.mi(convert_efa_to_cfa(efa, treshold=0.3), data = norm[vars_for_cfa], miPackage = 'mice', estimator = 'MLM', m = 10)
  print(fitMeasures(cfa)[c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.robust","rmsea.robust","srmr_bentler")])
}


# ============= Confirmatory factor analysis ============= #
library("lavaan")
library('psych')
library("writexl")
library('Hotelling')
library("xlsx")
library(mifa)
library(semTools)
library(sirt)
library('semPlot')

lgg = read.xlsx('PATH_TO_EXCEL..xlsx', 1)
hgg = read.xlsx('PATH_TO_EXCEL..xlsx', 1)
men = read.xlsx('PATH_TO_EXCEL..xlsx', 1)
norm = read.xlsx("PATH_TO_EXCEL..xlsx", 1)


cns_vs_varnames = c('verm_initial_correct_hits', 'verm_initial_correct_passes', 'verm_delayed_correct_hits', 'verm_delayed_correct_passes', 'vism_initial_correct_hits', 'vism_initial_correct_passes', 'vism_delayed_correct_hits', 'vism_delayed_correct_passes', 'sdc_correct', 'sdc_errors', 'sat_correct', 'sat_errors', 'stroop_simple_reaction_time', 'stroop_stroop_correct', 'stroop_stroop_reaction_time_correct', 'stroop_stroop_commission_errors', 'stroop_complex_correct', 'stroop_complex_reaction_time_correct', 'stroop_complex_commission_errors', 'sat_average_correct_rt', 'cpt_omission_errors', 'cpt_commission_errors', 'cpt_choice_reaction_time_correct', 'sat_correct.AND.sat_errors', 'ftt_right_taps_average', 'ftt_left_taps_average')

for (colname in cns_vs_varnames) {
  lgg[, colname] <- as.numeric(as.character(lgg[, colname]))
  hgg[, colname] <- as.numeric(as.character(hgg[, colname]))
  men[, colname] <- as.numeric(as.character(men[, colname]))
  norm[, colname] <- as.numeric(as.character(norm[, colname])) }

lgg = lgg[cns_vs_varnames]
lgg['from'] = 'lgg'
hgg = hgg[cns_vs_varnames]
hgg['from'] = 'hgg'
men = men[cns_vs_varnames]
men['from'] = 'men'
norm = norm[cns_vs_varnames]
norm['from'] = 'norm'

data = do.call("rbind", list(lgg, hgg, men, norm))
aggregate(data, by = list(data$from), FUN = mean)

names(data)[names(data) == "sat_correct.AND.sat_errors"] <- "sat_correct_AND_sat_errors"
names(data)[names(data) == "ftt_right_taps_average.AND.ftt_left_taps_average"] <- "ftt_right_taps_average_AND_ftt_left_taps_average"
data_patients = subset(data, from != 'norm')
data_hgg = subset(data, from == 'hgg')
data_lgg = subset(data, from == 'lgg')
data_norm = subset(data, from == 'norm')
data_men = subset(data, from == 'men')
data_comb = do.call("rbind", list(data_lgg, data_hgg[sample(1:nrow(data_hgg), 99), ], data_men[sample(1:nrow(data_men), 99), ]))

cns_vs_varnames_forordered = c('verm_initial_correct_hits', 'verm_initial_correct_passes', 'verm_delayed_correct_hits', 'verm_delayed_correct_passes', 'vism_initial_correct_hits', 'vism_initial_correct_passes', 'vism_delayed_correct_hits', 'vism_delayed_correct_passes', 'sdc_correct', 'sdc_errors', 'stroop_simple_reaction_time', 'stroop_stroop_correct', 'stroop_stroop_reaction_time_correct', 'stroop_stroop_commission_errors', 'stroop_complex_correct', 'stroop_complex_reaction_time_correct', 'stroop_complex_commission_errors', 'sat_average_correct_rt', 'cpt_omission_errors', 'cpt_commission_errors', 'cpt_choice_reaction_time_correct', 'sat_correct_AND_sat_errors', 'ftt_right_taps_average', 'ftt_left_taps_average')


# Models resulting from the combined sample
six = '
  MR1 =~ sdc_correct + stroop_simple_reaction_time + stroop_complex_correct + stroop_complex_reaction_time_correct + stroop_stroop_correct + stroop_stroop_reaction_time_correct + cpt_omission_errors + cpt_choice_reaction_time_correct + sat_average_correct_rt + sat_correct_AND_sat_errors
  MR6 =~ verm_initial_correct_passes + verm_delayed_correct_passes
  MR3 =~ ftt_right_taps_average + ftt_left_taps_average
  MR2 =~ vism_initial_correct_hits + vism_initial_correct_passes + vism_delayed_correct_hits + vism_delayed_correct_passes
  MR4 =~ sdc_errors + stroop_complex_commission_errors + stroop_stroop_commission_errors + cpt_commission_errors
  MR5 =~ verm_initial_correct_hits + verm_delayed_correct_hits
'

five =
  'MR1 =~ sdc_correct + stroop_simple_reaction_time + stroop_complex_reaction_time_correct + stroop_stroop_correct + stroop_stroop_reaction_time_correct + cpt_omission_errors + cpt_choice_reaction_time_correct + sat_average_correct_rt + sat_correct_AND_sat_errors
  MR2 =~ verm_initial_correct_passes + verm_delayed_correct_passes + vism_initial_correct_passes + vism_delayed_correct_passes
  MR4 =~ verm_initial_correct_hits + verm_delayed_correct_hits + vism_initial_correct_hits + vism_delayed_correct_hits
  MR3 =~ sdc_errors + stroop_complex_correct + stroop_complex_commission_errors + stroop_stroop_commission_errors + cpt_commission_errors
  MR5 =~ ftt_right_taps_average + ftt_left_taps_average'

four = 'MR1 =~ sdc_correct + stroop_simple_reaction_time + stroop_complex_reaction_time_correct + stroop_stroop_correct + stroop_stroop_reaction_time_correct + cpt_omission_errors + cpt_choice_reaction_time_correct + sat_average_correct_rt + sat_correct_AND_sat_errors + ftt_right_taps_average + ftt_left_taps_average
  MR2 =~ verm_initial_correct_passes + verm_delayed_correct_passes + vism_initial_correct_passes + vism_delayed_correct_passes
  MR4 =~ verm_initial_correct_hits + verm_delayed_correct_hits + vism_initial_correct_hits + vism_delayed_correct_hits
  MR3 =~ sdc_errors + stroop_complex_correct + stroop_complex_commission_errors + stroop_stroop_commission_errors + cpt_commission_errors'

three = 'MR1 =~ sdc_correct + stroop_simple_reaction_time + stroop_complex_reaction_time_correct + stroop_stroop_correct + stroop_stroop_reaction_time_correct + cpt_omission_errors + cpt_choice_reaction_time_correct + sat_average_correct_rt + sat_correct_AND_sat_errors + ftt_right_taps_average + ftt_left_taps_average
  MR2 =~ verm_initial_correct_hits + verm_initial_correct_passes + verm_delayed_correct_hits + verm_delayed_correct_passes + vism_initial_correct_hits + vism_initial_correct_passes + vism_delayed_correct_hits + vism_delayed_correct_passes
  MR3 =~ sdc_errors + stroop_complex_correct + stroop_complex_commission_errors + stroop_stroop_commission_errors + cpt_commission_errors
'

two = '
  MR1 =~ sdc_correct + sdc_errors + stroop_simple_reaction_time + stroop_complex_correct + stroop_complex_reaction_time_correct + stroop_complex_commission_errors + stroop_stroop_correct + stroop_stroop_reaction_time_correct + stroop_stroop_commission_errors + cpt_omission_errors + cpt_commission_errors + cpt_choice_reaction_time_correct + sat_average_correct_rt + sat_correct_AND_sat_errors + ftt_right_taps_average + ftt_left_taps_average
  MR2 =~ verm_initial_correct_hits + verm_initial_correct_passes + verm_delayed_correct_hits + verm_delayed_correct_passes + vism_initial_correct_hits + vism_initial_correct_passes + vism_delayed_correct_hits + vism_delayed_correct_passes
'

one = '
MR1 =~ verm_initial_correct_hits + verm_initial_correct_passes + verm_delayed_correct_hits + verm_delayed_correct_passes + vism_initial_correct_hits + vism_initial_correct_passes + vism_delayed_correct_hits + vism_delayed_correct_passes + sdc_correct + sdc_errors + stroop_simple_reaction_time + stroop_complex_correct + stroop_complex_reaction_time_correct + stroop_complex_commission_errors + stroop_stroop_correct + stroop_stroop_reaction_time_correct + stroop_stroop_commission_errors + cpt_omission_errors + cpt_commission_errors + cpt_choice_reaction_time_correct + sat_average_correct_rt + sat_correct_AND_sat_errors + ftt_right_taps_average + ftt_left_taps_average
'


use_model = five


hgg_cfa = sem.mi(use_model, data = data_hgg, miPackage = 'mice', estimator = 'MLM', m = 10)
lgg_cfa = sem.mi(use_model, data = data_lgg, miPackage = 'mice', estimator = 'MLM', m = 10)
men_cfa = sem.mi(use_model, data = data_men, miPackage = 'mice', estimator = 'MLM', m = 10)
norm_cfa = sem.mi(use_model, data = data_norm, miPackage = 'mice', estimator = 'MLM', m = 10)
comb_balanced_cfa = sem.mi(use_model, data = data_comb, miPackage = 'mice', estimator = 'MLM', m = 10)
summary(lgg_cfa, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
summary(hgg_cfa, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
summary(men_cfa, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
summary(norm_cfa, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
summary(comb_balanced_cfa, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE)
fitMeasures(men_cfa)[c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.robust","rmsea.robust","srmr_bentler")]
fitMeasures(lgg_cfa)[c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.robust","rmsea.robust","srmr_bentler")]
fitMeasures(hgg_cfa)[c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.robust","rmsea.robust","srmr_bentler")]
fitMeasures(norm_cfa)[c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.robust","rmsea.robust","srmr_bentler")]
fitMeasures(comb_balanced_cfa)[c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.robust","rmsea.robust","srmr_bentler")]


syntax.config <- measEq.syntax(configural.model = use_model, data = data_patients,
                               parameterization = "theta",
                               ID.fac = "std.lv",
                               group = "from")
syntax.weak <- measEq.syntax(configural.model = use_model, data = data_patients,
                               parameterization = "theta",
                               ID.fac = "std.lv",
                               group = "from",
                               group.equal = c("loadings"))
syntax.strong <- measEq.syntax(configural.model = use_model, data = data_patients,
                               parameterization = "theta",
                               ID.fac = "std.lv",
                               group = "from",
                               group.equal = c("loadings", "intercepts"))
syntax.strict <- measEq.syntax(configural.model = use_model, data = data_patients,
                               parameterization = "theta",
                               ID.fac = "std.lv",
                               group = "from",
                               group.equal = c("loadings", "intercepts", "residuals"))
eqvars <- measEq.syntax(configural.model = use_model, data = data_patients,
                               parameterization = "theta",
                               ID.fac = "std.lv",
                               group = "from",
                               group.equal = c("loadings", "intercepts", "residuals", 'lv.variances'))
eqmeans <- measEq.syntax(configural.model = use_model, data = data_patients,
                               parameterization = "theta",
                               ID.fac = "std.lv",
                               group = "from",
                               group.equal = c("loadings", "intercepts", "residuals", 'lv.variances', 'means'))
eqmeans2 <- measEq.syntax(configural.model = use_model, data = data_patients,
                               parameterization = "theta",
                               ID.fac = "std.lv",
                               group = "from",
                               group.equal = c("loadings", "intercepts", "residuals", 'means'))
mod.config <- as.character(syntax.config)
mod.weak <- as.character(syntax.weak)
mod.strong <- as.character(syntax.strong)
mod.strict <- as.character(syntax.strict)
mod.eqvars <- as.character(eqvars)
mod.eqmeans <- as.character(eqmeans)
mod.eqmeans2 <- as.character(eqmeans2)
res_config = sem.mi(mod.config, data = data_patients, miPackage = 'mice', estimator = 'MLM', m = 10, group = 'from', std.lv = TRUE)

# ============= Congruence coefficients ============= #

library("xlsx")
library('psych')

# Load efa results
efa_men = read.xlsx("PATH_TO_EXCEL.xlsx", 1)
efa_lgg = read.xlsx("PATH_TO_EXCEL.xlsx", 1)
efa_hgg = read.xlsx("PATH_TO_EXCEL.xlsx", 1)
efa_hc = read.xlsx("PATH_TO_EXCEL.xlsx", 1)

efa_men = efa_men[1:24,2:6]
efa_lgg = efa_lgg[1:24,2:6]
efa_hgg = efa_hgg[1:24,2:6]
efa_hc = efa_hc[1:24,2:6]

# Calculate Tuckers coefficients
hgg_lgg = factor.congruence(efa_hgg, y=efa_lgg,digits=2,use=NULL,structure=FALSE)
lgg_men = factor.congruence(efa_lgg, y=efa_men,digits=2,use=NULL,structure=FALSE)
hgg_men = factor.congruence(efa_hgg, y=efa_men,digits=2,use=NULL,structure=FALSE)

hc_hgg = factor.congruence(efa_hc, y=efa_hgg,digits=2,use=NULL,structure=FALSE)
hg_lgg = factor.congruence(efa_hc, y=efa_lgg,digits=2,use=NULL,structure=FALSE)
hg_men = factor.congruence(efa_hc, y=efa_men,digits=2,use=NULL,structure=FALSE)

