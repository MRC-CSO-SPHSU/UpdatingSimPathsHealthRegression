library(tidyverse)
library(openxlsx)
library(magrittr)
library(fixest)

source("R/import_data.R")

wb_health_wb <- createWorkbook()
wb_mental_health <- createWorkbook()

expand_grid(
  c("UK_"),
  c("DHE_MCS", "DHE_PCS", "DLS"),
  c("1", "2_Males", "2_Females")
) |> 
  pmap(paste0) |> 
  reduce(c) |> 
  walk(~addWorksheet(.x, wb = wb_health_wb))

expand_grid(
  c("UK_"),
  c("HM"),
  c("1_L", "2_Males_L", "2_Females_L")
) |> 
  pmap(paste0) |> 
  reduce(c) |> 
  walk(~addWorksheet(.x, wb = wb_mental_health))

#' Lag Age and Agesq

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

writeData(wb_health_wb, "UK_DHE_MCS1", dhe_mcs_model_cols3)

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

writeData(wb_health_wb, "UK_DHE_PCS1", dhe_pcs_model_cols3)

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

writeData(wb_health_wb, "UK_DLS1", dls_model_cols3)

dhm_model <- data_with_dummies_lags %$%
  lm(
    Dhm ~ 0 + Constant + D_Econ_benefits + D_Home_owner + Dcpst_Single +
      Dcpst_PreviouslyPartnered + Dnc_L1 + UKC + UKD + UKE + UKF +
      UKG + UKH + UKJ + UKK + UKL + UKM + UKN + Ydses_c5_Q2_L1 +
      Ydses_c5_Q3_L1 + Ydses_c5_Q4_L1 + Ydses_c5_Q5_L1 + Dlltsd_L1 +
      Dhm_L1 + Dhe_pcs_L1 +
      Dgn + Dag_L1 + Dag_sq_L1 + Deh_c3_Medium +
      Deh_c3_Low + Year_transformed,
    weights = weight
  )

dhm_model_coef <- dhm_model$coefficients
dhm_model_vcov <- sandwich::vcovCL(dhm_model, cluster = ~pidp)
dhm_model_cols <-  cbind(dhm_model_coef, dhm_model_vcov)
dhm_model_cols2 <- as.data.frame(dhm_model_cols)
dhm_model_cols3 <- rownames_to_column(dhm_model_cols2, "REGRESSOR") |>
  rename('COEFFICIENT' = 'dhm_model_coef')

writeData(wb_mental_health, "UK_HM1_L", dhm_model_cols3)



# Fixest of other bits ----------------------------------------------------

mcs_mega_mod_males <- 
  feols(
    Dhe_mcs ~ EmployedToUnemployed +
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
      Dhe_mcs_L1 + Dhe_pcs_L1 + Dag_L1 + Dag_sq_L1 + Deh_c3_Medium +
      Deh_c3_Low + Year_transformed,
    fixef = "pidp",
    data = data_with_dummies_lags,
    weights = ~weight, 
    cluster = ~pidp, se = "cluster", subset = data_with_dummies_lags$Dgn == 0
  )

mcs_mega_mod_females <- 
  feols(
    Dhe_mcs ~ EmployedToUnemployed +
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
      Dhe_mcs_L1 + Dhe_pcs_L1 + Dag_L1 + Dag_sq_L1 + Deh_c3_Medium +
      Deh_c3_Low + Year_transformed,
    fixef = "pidp",
    data = data_with_dummies_lags,
    weights = ~weight, 
    cluster = ~pidp, se = "cluster", subset = data_with_dummies_lags$Dgn == 1
  )

pcs_mega_mod_males <- 
  feols(
    Dhe_pcs ~ EmployedToUnemployed +
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
    Dhe_pcs ~ EmployedToUnemployed +
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
    cluster = ~pidp, se = "cluster", subset = data_with_dummies_lags$Dgn == 1
  )

dls_mega_mod_males <- 
  feols(
    Dls ~ EmployedToUnemployed +
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
      Dhe_mcs_L1 + Dhe_pcs_L1 + Dag_L1 + Dag_sq_L1 + Deh_c3_Medium +
      Deh_c3_Low + Year_transformed,
    fixef = "pidp",
    data = data_with_dummies_lags,
    weights = ~weight, 
    cluster = ~pidp, se = "cluster", subset = data_with_dummies_lags$Dgn == 0
  )

dls_mega_mod_females <- 
  feols(
    Dls ~ EmployedToUnemployed +
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
      Dhe_mcs_L1 + Dhe_pcs_L1 + Dag_L1 + Dag_sq_L1 + Deh_c3_Medium +
      Deh_c3_Low + Year_transformed,
    fixef = "pidp",
    data = data_with_dummies_lags,
    weights = ~weight, 
    cluster = ~pidp, se = "cluster", subset = data_with_dummies_lags$Dgn == 1
  )

dhm_mega_mod_males <- 
  feols(
    Dhm ~ EmployedToUnemployed +
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
      Dhm_L1 + Dhe_pcs_L1 + Dag_L1 + Dag_sq_L1 + Deh_c3_Medium +
      Deh_c3_Low + Year_transformed,
    fixef = "pidp",
    data = data_with_dummies_lags,
    weights = ~weight, 
    cluster = ~pidp, se = "cluster", subset = data_with_dummies_lags$Dgn == 0
  )

dhm_mega_mod_females <- 
  feols(
    Dhm ~ EmployedToUnemployed +
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
      Dhm_L1 + Dhe_pcs_L1 + Dag_L1 + Dag_sq_L1 + Deh_c3_Medium +
      Deh_c3_Low + Year_transformed,
    fixef = "pidp",
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
  
  var_mat[1:8, 1:8]
}

mcs_mega_mod_males |> 
  broom::tidy() |> 
  head(8) |> 
  transmute(
    REGRESSOR = term,
    COEFFICIENT = estimate
  ) |> 
  bind_cols(
    var_matrix(mcs_mega_mod_males)
  ) |> 
  writeData(wb_health_wb, "UK_DHE_MCS2_Males", x = _)

mcs_mega_mod_females |> 
  broom::tidy() |> 
  head(8) |> 
  transmute(
    REGRESSOR = term,
    COEFFICIENT = estimate
  ) |> 
  bind_cols(
    var_matrix(mcs_mega_mod_females)
  ) |> 
  writeData(wb_health_wb, "UK_DHE_MCS2_Females", x = _)

pcs_mega_mod_males |> 
  broom::tidy() |> 
  head(8) |> 
  transmute(
    REGRESSOR = term,
    COEFFICIENT = estimate
  ) |> 
  bind_cols(
    var_matrix(pcs_mega_mod_males)
  ) |> 
  writeData(wb_health_wb, "UK_DHE_PCS2_Males", x = _)

pcs_mega_mod_females |> 
  broom::tidy() |> 
  head(8) |> 
  transmute(
    REGRESSOR = term,
    COEFFICIENT = estimate
  ) |> 
  bind_cols(
    var_matrix(pcs_mega_mod_females)
  ) |> 
  writeData(wb_health_wb, "UK_DHE_PCS2_Females", x = _)

dls_mega_mod_males |> 
  broom::tidy() |> 
  head(8) |> 
  transmute(
    REGRESSOR = term,
    COEFFICIENT = estimate
  ) |> 
  bind_cols(
    var_matrix(dls_mega_mod_males)
  ) |> 
  writeData(wb_health_wb, "UK_DLS2_Males", x = _)

dls_mega_mod_females |> 
  broom::tidy() |> 
  head(8) |> 
  transmute(
    REGRESSOR = term,
    COEFFICIENT = estimate
  ) |> 
  bind_cols(
    var_matrix(dls_mega_mod_females)
  ) |> 
  writeData(wb_health_wb, "UK_DLS2_Females", x = _)

dhm_mega_mod_males |> 
  broom::tidy() |> 
  head(8) |> 
  transmute(
    REGRESSOR = term,
    COEFFICIENT = estimate
  ) |> 
  bind_cols(
    var_matrix(dhm_mega_mod_males)
  ) |> 
  writeData(wb_mental_health, "UK_HM2_Males_L", x = _)

dhm_mega_mod_females |> 
  broom::tidy() |> 
  head(8) |> 
  transmute(
    REGRESSOR = term,
    COEFFICIENT = estimate
  ) |> 
  bind_cols(
    var_matrix(dhm_mega_mod_females)
  ) |> 
  writeData(wb_mental_health, "UK_HM2_Females_L", x = _)

if(!dir.exists("outfiles")) dir.create("outfiles")

saveWorkbook(wb_health_wb, "outfiles/reg_health_wellbeing.xlsx", overwrite = TRUE)
saveWorkbook(wb_mental_health, "outfiles/reg_health_mental.xlsx", overwrite = TRUE)
