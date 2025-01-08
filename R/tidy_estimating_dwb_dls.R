library(tidyverse)
library(openxlsx)
library(magrittr)
library(fixest)

source("R/import_data.R")

wb <- createWorkbook()

expand_grid(
  c("UK_"),
  c("DHE_MCS", "DHE_PCS", "DLS"),
  c("1", "2_Males", "2_Females")
) |> 
  pmap(paste0) |> 
  reduce(c) |> 
  walk(~addWorksheet(.x, wb = wb))

# Do GHQ here too!
#' Lag Age and Agesq
#' Remove lagged PCS

dhe_mcs_model <- data_with_dummies_lags %$%
  lm(
    Dhe_mcs ~ 0 + Constant + D_Econ_benefits + D_Home_owner + Dcpst_Single +
      Dcpst_PreviouslyPartnered + Dnc_L1 + UKC + UKD + UKE + UKF +
      UKG + UKH + UKJ + UKK + UKL + UKM + UKN + Ydses_c5_Q2_L1 +
      Ydses_c5_Q3_L1 + Ydses_c5_Q4_L1 + Ydses_c5_Q5_L1 + Dlltsd_L1 +
      Dhe_mcs_L1 + Dhe_pcs_L1 + 
      Dgn + Dag_L1 + Dag_sq_L1 + Deh_c3_Medium +
      Deh_c3_Low + Year_transformed,
    weights = weight
  )

dhe_mcs_model_coef <- dhe_mcs_model$coefficients
dhe_mcs_model_vcov <- sandwich::vcovCL(dhe_mcs_model, cluster = ~pidp)
dhe_mcs_model_cols <-  cbind(dhe_mcs_model_coef, dhe_mcs_model_vcov)
dhe_mcs_model_cols2 <- as.data.frame(dhe_mcs_model_cols)
dhe_mcs_model_cols3 <- rownames_to_column(dhe_mcs_model_cols2, "REGRESSOR") |>
  rename('COEFFICIENT' = 'dhe_mcs_model_coef')

writeData(wb, "UK_DHE_MCS1", dhe_mcs_model_cols3)

## Ouctome: SF12 mental (sf12mcs_dv), as linear regression
#' PCS age stays unlagged

dhe_pcs_model <- data_with_dummies_lags %$%
  lm(
    Dhe_pcs ~ 0 + Constant + D_Econ_benefits + D_Home_owner + Dcpst_Single +
      Dcpst_PreviouslyPartnered + Dnc_L1 + UKC + UKD + UKE + UKF +
      UKG + UKH + UKJ + UKK + UKL + UKM + UKN + Ydses_c5_Q2_L1 +
      Ydses_c5_Q3_L1 + Ydses_c5_Q4_L1 + Ydses_c5_Q5_L1 + Dlltsd_L1 +
      Dhe_mcs_L1 + Dhe_pcs_L1 +
      Dgn + Dag + Dag_sq + Deh_c3_Medium +
      Deh_c3_Low + Year_transformed,
    weights = weight
  )

dhe_pcs_model_coef <- dhe_pcs_model$coefficients
dhe_pcs_model_vcov <- sandwich::vcovCL(dhe_pcs_model, cluster = ~pidp)
dhe_pcs_model_cols <-  cbind(dhe_pcs_model_coef, dhe_pcs_model_vcov)
dhe_pcs_model_cols2 <- as.data.frame(dhe_pcs_model_cols)
dhe_pcs_model_cols3 <- rownames_to_column(dhe_pcs_model_cols2, "REGRESSOR") |>
  rename('COEFFICIENT' = 'dhe_pcs_model_coef')

writeData(wb, "UK_DHE_PCS1", dhe_pcs_model_cols3)

#' Lag Age and Agesq

dls_model <- data_with_dummies_lags %$%
  lm(
    Dls ~ 0 + Constant + D_Econ_benefits + D_Home_owner + Dcpst_Single +
      Dcpst_PreviouslyPartnered + Dnc_L1 + UKC + UKD + UKE + UKF +
      UKG + UKH + UKJ + UKK + UKL + UKM + UKN + Ydses_c5_Q2_L1 +
      Ydses_c5_Q3_L1 + Ydses_c5_Q4_L1 + Ydses_c5_Q5_L1 + Dlltsd_L1 +
      Dls_L1 + Dhe_pcs_L1 +
      Dgn + Dag_L1 + Dag_sq_L1 + Deh_c3_Medium +
      Deh_c3_Low + Year_transformed,
    weights = weight
  )

dls_model_coef <- dls_model$coefficients
dls_model_vcov <- sandwich::vcovCL(dls_model, cluster = ~pidp)
dls_model_cols <-  cbind(dls_model_coef, dls_model_vcov)
dls_model_cols2 <- as.data.frame(dls_model_cols)
dls_model_cols3 <- rownames_to_column(dls_model_cols2, "REGRESSOR") |>
  rename('COEFFICIENT' = 'dls_model_coef')

writeData(wb, "UK_DLS1", dls_model_cols3)



# Fixest of other bits ----------------------------------------------------

cols_to_test <- c(
  "EmployedToUnemployed",
  "UnemployedToEmployed",
  "PersistentUnemployed",
  "NonPovertyToPoverty",
  "PovertyToNonPoverty",
  "PersistentPoverty",
  "RealIncomeChange",
  "RealIncomeDecrease_D"
)

mcs_formulae <- cols_to_test |>
  map(\(outcome) Formula::as.Formula(
    glue::glue(
      "Dhe_mcs ~ 0 + {outcome} | {other_cols} + Constant + D_Econ_benefits + D_Home_owner + Dcpst_Single +
    Dcpst_PreviouslyPartnered + Dnc_L1 + UKC + UKD + UKE + UKF +
    UKG + UKH + UKJ + UKK + UKL + UKM + UKN + Ydses_c5_Q2_L1 +
    Ydses_c5_Q3_L1 + Ydses_c5_Q4_L1 + Ydses_c5_Q5_L1 + Dlltsd_L1 +
    Dhe_mcs_L1 + Dhe_pcs_L1 + Dag + Dag_sq + Deh_c3_Medium +
    Deh_c3_Low + Year_transformed",
    other_cols = paste(cols_to_test[cols_to_test != outcome], collapse = " + ")
    )
  ))

pcs_formulae <- cols_to_test |>
  map(\(outcome) Formula::as.Formula(
    glue::glue(
      "Dhe_pcs ~ 0 + {outcome} | {other_cols} + Constant + D_Econ_benefits + D_Home_owner + Dcpst_Single +
    Dcpst_PreviouslyPartnered + Dnc_L1 + UKC + UKD + UKE + UKF +
    UKG + UKH + UKJ + UKK + UKL + UKM + UKN + Ydses_c5_Q2_L1 +
    Ydses_c5_Q3_L1 + Ydses_c5_Q4_L1 + Ydses_c5_Q5_L1 + Dlltsd_L1 +
    Dhe_mcs_L1 + Dhe_pcs_L1 + Dag + Dag_sq + Deh_c3_Medium +
    Deh_c3_Low + Year_transformed",
    other_cols = paste(cols_to_test[cols_to_test != outcome], collapse = " + ")
    )
  ))

dls_formulae <- cols_to_test |>
  map(\(outcome) Formula::as.Formula(
    glue::glue(
      "Dls ~ 0 + {outcome} | {other_cols} + Constant + D_Econ_benefits + D_Home_owner + Dcpst_Single +
    Dcpst_PreviouslyPartnered + Dnc_L1 + UKC + UKD + UKE + UKF +
    UKG + UKH + UKJ + UKK + UKL + UKM + UKN + Ydses_c5_Q2_L1 +
    Ydses_c5_Q3_L1 + Ydses_c5_Q4_L1 + Ydses_c5_Q5_L1 + Dlltsd_L1 +
    Dls_L1 + Dhe_mcs_L1 + Dhe_pcs_L1 + Dag + Dag_sq + Deh_c3_Medium +
    Deh_c3_Low + Year_transformed",
    other_cols = paste(cols_to_test[cols_to_test != outcome], collapse = " + ")
    )
  ))

# mod1 <- feols(
#   Dhe_mcs ~ 0 + EmployedToUnemployed | Constant + D_Econ_benefits + D_Home_owner + Dcpst_Single +
#     Dcpst_PreviouslyPartnered + Dnc_L1 + UKC + UKD + UKE + UKF +
#     UKG + UKH + UKJ + UKK + UKL + UKM + UKN + Ydses_c5_Q2_L1 +
#     Ydses_c5_Q3_L1 + Ydses_c5_Q4_L1 + Ydses_c5_Q5_L1 + Dlltsd_L1 +
#     Dhe_mcs_L1 + Dhe_pcs_L1 + Dgn + Dag + Dag_sq + Deh_c3_Medium +
#     Deh_c3_Low + Year_transformed,
#   data = data_with_dummies_lags,
#   weights = ~ weight
# )

library(furrr)
plan(multisession, workers = 32)
# 
# mcs_mods_males <- future_map(.options = furrr_options(seed = TRUE), mcs_formulae, \(form) {
#   feols(form, data = data_with_dummies_lags, 
#         weights = ~weight,
#         cluster = ~pidp, se = "cluster", subset = data_with_dummies_lags$Dgn == 0)
# })
# 
# mcs_mods_females <- future_map(.options = furrr_options(seed = TRUE), mcs_formulae, \(form) {
#   feols(form, data = data_with_dummies_lags, 
#         weights = ~weight,
#         cluster = ~pidp, se = "cluster", subset = data_with_dummies_lags$Dgn == 1)
# })
# 
# pcs_mods_males <- future_map(.options = furrr_options(seed = TRUE), pcs_formulae, \(form) {
#   feols(form, data = data_with_dummies_lags, 
#         weights = ~weight,
#         cluster = ~pidp, se = "cluster", subset = data_with_dummies_lags$Dgn == 0)
# })
# 
# pcs_mods_females <- future_map(.options = furrr_options(seed = TRUE), pcs_formulae, \(form) {
#   feols(form, data = data_with_dummies_lags, 
#         weights = ~weight,
#         cluster = ~pidp, se = "cluster", subset = data_with_dummies_lags$Dgn == 1)
# })
# 
# dls_mods_males <- future_map(.options = furrr_options(seed = TRUE), dls_formulae, \(form) {
#   feols(form, data = data_with_dummies_lags, 
#         weights = ~weight,
#         cluster = ~pidp, se = "cluster", subset = data_with_dummies_lags$Dgn == 0)
# })
# 
# dls_mods_females <- future_map(.options = furrr_options(seed = TRUE), dls_formulae, \(form) {
#   feols(form, data = data_with_dummies_lags, 
#         weights = ~weight,
#         cluster = ~pidp, se = "cluster", subset = data_with_dummies_lags$Dgn == 1)
# })

mcs_mega_mod_males <- 
  feols(
    Dhe_mcs ~ 0 + EmployedToUnemployed +
    UnemployedToEmployed +
    PersistentUnemployed +
    NonPovertyToPoverty +
    PovertyToNonPoverty +
    PersistentPoverty +
    RealIncomeChange +
    RealIncomeDecrease_D + D_Econ_benefits + D_Home_owner + Dcpst_Single +
      Dcpst_PreviouslyPartnered + Dnc_L1 + UKC + UKD + UKE + UKF +
      UKG + UKH + UKJ + UKK + UKL + UKM + UKN + Ydses_c5_Q2_L1 +
      Ydses_c5_Q3_L1 + Ydses_c5_Q4_L1 + Ydses_c5_Q5_L1 + Dlltsd_L1 +
      Dhe_mcs_L1 + Dhe_pcs_L1 + Dag + Dag_sq + Deh_c3_Medium +
      Deh_c3_Low + Year_transformed,
    fixef = "pidp",
    data = data_with_dummies_lags,
    weights = ~weight, 
    cluster = ~pidp, se = "cluster", subset = data_with_dummies_lags$Dgn == 0
  )

mcs_mega_mod_females <- 
  feols(
    Dhe_mcs ~ 0 + EmployedToUnemployed +
    UnemployedToEmployed +
    PersistentUnemployed +
    NonPovertyToPoverty +
    PovertyToNonPoverty +
    PersistentPoverty +
    RealIncomeChange +
    RealIncomeDecrease_D | Constant + D_Econ_benefits + D_Home_owner + Dcpst_Single +
      Dcpst_PreviouslyPartnered + Dnc_L1 + UKC + UKD + UKE + UKF +
      UKG + UKH + UKJ + UKK + UKL + UKM + UKN + Ydses_c5_Q2_L1 +
      Ydses_c5_Q3_L1 + Ydses_c5_Q4_L1 + Ydses_c5_Q5_L1 + Dlltsd_L1 +
      Dhe_mcs_L1 + Dhe_pcs_L1 + Dag + Dag_sq + Deh_c3_Medium +
      Deh_c3_Low + Year_transformed,
    data = data_with_dummies_lags,
    weights = ~weight,
    cluster = ~pidp, se = "cluster", subset = data_with_dummies_lags$Dgn == 1
  )

pcs_mega_mod_males <- 
  feols(
    Dhe_pcs ~ 0 + EmployedToUnemployed +
    UnemployedToEmployed +
    PersistentUnemployed +
    NonPovertyToPoverty +
    PovertyToNonPoverty +
    PersistentPoverty +
    RealIncomeChange +
    RealIncomeDecrease_D + D_Econ_benefits + D_Home_owner + Dcpst_Single +
      Dcpst_PreviouslyPartnered + Dnc_L1 + UKC + UKD + UKE + UKF +
      UKG + UKH + UKJ + UKK + UKL + UKM + UKN + Ydses_c5_Q2_L1 +
      Ydses_c5_Q3_L1 + Ydses_c5_Q4_L1 + Ydses_c5_Q5_L1 + Dlltsd_L1 +
      Dhe_mcs_L1 + Dhe_pcs_L1 + Dag + Dag_sq + Deh_c3_Medium +
      Deh_c3_Low + Year_transformed,
    fixef = "pidp",
    data = data_with_dummies_lags,
    weights = ~weight, 
    cluster = ~pidp, se = "cluster", subset = data_with_dummies_lags$Dgn == 0
  )

pcs_mega_mod_females <- 
  feols(
    Dhe_pcs ~ 0 + EmployedToUnemployed +
    UnemployedToEmployed +
    PersistentUnemployed +
    NonPovertyToPoverty +
    PovertyToNonPoverty +
    PersistentPoverty +
    RealIncomeChange +
    RealIncomeDecrease_D | Constant + D_Econ_benefits + D_Home_owner + Dcpst_Single +
      Dcpst_PreviouslyPartnered + Dnc_L1 + UKC + UKD + UKE + UKF +
      UKG + UKH + UKJ + UKK + UKL + UKM + UKN + Ydses_c5_Q2_L1 +
      Ydses_c5_Q3_L1 + Ydses_c5_Q4_L1 + Ydses_c5_Q5_L1 + Dlltsd_L1 +
      Dhe_mcs_L1 + Dhe_pcs_L1 + Dag + Dag_sq + Deh_c3_Medium +
      Deh_c3_Low + Year_transformed,
    data = data_with_dummies_lags,
    weights = ~weight,
    cluster = ~pidp, se = "cluster", subset = data_with_dummies_lags$Dgn == 1
  )

dls_mega_mod_males <- 
  feols(
    Dls ~ 0 + EmployedToUnemployed +
    UnemployedToEmployed +
    PersistentUnemployed +
    NonPovertyToPoverty +
    PovertyToNonPoverty +
    PersistentPoverty +
    RealIncomeChange +
    RealIncomeDecrease_D | Constant + D_Econ_benefits + D_Home_owner + Dcpst_Single +
      Dcpst_PreviouslyPartnered + Dnc_L1 + UKC + UKD + UKE + UKF +
      UKG + UKH + UKJ + UKK + UKL + UKM + UKN + Ydses_c5_Q2_L1 +
      Ydses_c5_Q3_L1 + Ydses_c5_Q4_L1 + Ydses_c5_Q5_L1 + Dlltsd_L1 +
      Dls_L1 + Dhe_mcs_L1 + Dhe_pcs_L1 + Dag + Dag_sq + Deh_c3_Medium +
      Deh_c3_Low + Year_transformed,
    data = data_with_dummies_lags,
    weights = ~weight, 
    cluster = ~pidp, se = "cluster", subset = data_with_dummies_lags$Dgn == 0
  )

dls_mega_mod_females <- 
  feols(
    Dls ~ 0 + EmployedToUnemployed +
    UnemployedToEmployed +
    PersistentUnemployed +
    NonPovertyToPoverty +
    PovertyToNonPoverty +
    PersistentPoverty +
    RealIncomeChange +
    RealIncomeDecrease_D | Constant + D_Econ_benefits + D_Home_owner + Dcpst_Single +
      Dcpst_PreviouslyPartnered + Dnc_L1 + UKC + UKD + UKE + UKF +
      UKG + UKH + UKJ + UKK + UKL + UKM + UKN + Ydses_c5_Q2_L1 +
      Ydses_c5_Q3_L1 + Ydses_c5_Q4_L1 + Ydses_c5_Q5_L1 + Dlltsd_L1 +
      Dls_L1 + Dhe_mcs_L1 + Dhe_pcs_L1 + Dag + Dag_sq + Deh_c3_Medium +
      Deh_c3_Low + Year_transformed,
    data = data_with_dummies_lags,
    weights = ~weight,
    cluster = ~pidp, se = "cluster", subset = data_with_dummies_lags$Dgn == 1
  )


# combine and write -------------------------------------------------------

var_matrix <- function(model_in) {
  var_covar <- vcov(model_in)
  var_mat <- var_covar
  var_mat[1:nrow(var_mat), 1:ncol(var_mat)] <- 0
  
  diag(var_mat) <- diag(var_covar)
  
  var_mat
}

mcs_mega_mod_males |> 
  broom::tidy() |> 
  transmute(
    REGRESSOR = term,
    COEFFICIENT = estimate
  ) |> 
  bind_cols(
    var_matrix(mcs_mega_mod_males)
  ) |> 
  writeData(wb, "UK_DHE_MCS2_Males", x = _)

mcs_mega_mod_females |> 
  broom::tidy() |> 
  transmute(
    REGRESSOR = term,
    COEFFICIENT = estimate
  ) |> 
  bind_cols(
    var_matrix(mcs_mega_mod_females)
  ) |> 
  writeData(wb, "UK_DHE_MCS2_Females", x = _)

pcs_mega_mod_males |> 
  broom::tidy() |> 
  transmute(
    REGRESSOR = term,
    COEFFICIENT = estimate
  ) |> 
  bind_cols(
    var_matrix(pcs_mega_mod_males)
  ) |> 
  writeData(wb, "UK_DHE_PCS2_Males", x = _)

pcs_mega_mod_females |> 
  broom::tidy() |> 
  transmute(
    REGRESSOR = term,
    COEFFICIENT = estimate
  ) |> 
  bind_cols(
    var_matrix(pcs_mega_mod_females)
  ) |> 
  writeData(wb, "UK_DHE_PCS2_Females", x = _)

dls_mega_mod_males |> 
  broom::tidy() |> 
  transmute(
    REGRESSOR = term,
    COEFFICIENT = estimate
  ) |> 
  bind_cols(
    var_matrix(dls_mega_mod_males)
  ) |> 
  writeData(wb, "UK_DLS2_Males", x = _)

dls_mega_mod_females |> 
  broom::tidy() |> 
  transmute(
    REGRESSOR = term,
    COEFFICIENT = estimate
  ) |> 
  bind_cols(
    var_matrix(dls_mega_mod_females)
  ) |> 
  writeData(wb, "UK_DLS2_Females", x = _)

# mcs_mods_males |> 
#   map(\(model) {
#     
#     term <- names(model$coefficients)
#     
#     model |> 
#       broom::tidy() |> 
#       transmute(
#         REGRESSOR = term,
#         COEFFICIENT = estimate,
#         "{term}" := std.error^2
#       )
#   }) |> 
#   reduce(bind_rows) |> 
#   map_dfr(replace_na, 0) |> 
#   writeData(wb, "UK_DHE_MCS2_Males", x = _)
# 
# mcs_mods_females |> 
#   map(\(model) {
#     
#     term <- names(model$coefficients)
#     
#     model |> 
#       broom::tidy() |> 
#       transmute(
#         REGRESSOR = term,
#         COEFFICIENT = estimate,
#         "{term}" := std.error^2
#       )
#   }) |> 
#   reduce(bind_rows) |> 
#   map_dfr(replace_na, 0) |> 
#   writeData(wb, "UK_DHE_MCS2_Females", x = _)
# 
# pcs_mods_males |> 
#   map(\(model) {
#     
#     term <- names(model$coefficients)
#     
#     model |> 
#       broom::tidy() |> 
#       transmute(
#         REGRESSOR = term,
#         COEFFICIENT = estimate,
#         "{term}" := std.error^2
#       )
#   }) |> 
#   reduce(bind_rows) |> 
#   map_dfr(replace_na, 0) |> 
#   writeData(wb, "UK_DHE_PCS2_Males", x = _)
# 
# pcs_mods_females |> 
#   map(\(model) {
#     
#     term <- names(model$coefficients)
#     
#     model |> 
#       broom::tidy() |> 
#       transmute(
#         REGRESSOR = term,
#         COEFFICIENT = estimate,
#         "{term}" := std.error^2
#       )
#   }) |> 
#   reduce(bind_rows) |> 
#   map_dfr(replace_na, 0) |> 
#   writeData(wb, "UK_DHE_PCS2_Females", x = _)
# 
# dls_mods_males |> 
#   map(\(model) {
#     
#     term <- names(model$coefficients)
#     
#     model |> 
#       broom::tidy() |> 
#       transmute(
#         REGRESSOR = term,
#         COEFFICIENT = estimate,
#         "{term}" := std.error^2
#       )
#   }) |> 
#   reduce(bind_rows) |> 
#   map_dfr(replace_na, 0) |> 
#   writeData(wb, "UK_DLS2_Males", x = _)
# 
# dls_mods_females |> 
#   map(\(model) {
#     
#     term <- names(model$coefficients)
#     
#     model |> 
#       broom::tidy() |> 
#       transmute(
#         REGRESSOR = term,
#         COEFFICIENT = estimate,
#         "{term}" := std.error^2
#       )
#   }) |> 
#   reduce(bind_rows) |> 
#   map_dfr(replace_na, 0) |> 
#   writeData(wb, "UK_DLS2_Females", x = _)

dir.create("outfiles")

saveWorkbook(wb, "outfiles/reg_wellbeing.xlsx", overwrite = TRUE)
