library(tidyverse)
library(openxlsx)
library(magrittr)
library(fixest)

source("R/import_data.R")

wb <- createWorkbook()

expand_grid(
  c("UK_"),
  c("DWB_MCS", "DWB_PCS", "DLS"),
  c("1", "2_Males", "2_Females")
) |> 
  pmap(paste0) |> 
  reduce(c) |> 
  walk(~addWorksheet(.x, wb = wb))

dwb_mcs_model <- data_with_dummies_lags %$%
  lm(
    Dwb_mcs ~ 0 + Constant + D_Econ_benefits + D_Home_owner + Dcpst_Single +
      Dcpst_PreviouslyPartnered + Dnc_L1 + Dhe_L1 + UKC + UKD + UKE + UKF +
      UKG + UKH + UKJ + UKK + UKL + UKM + UKN + Ydses_c5_Q2_L1 +
      Ydses_c5_Q3_L1 + Ydses_c5_Q4_L1 + Ydses_c5_Q5_L1 + Dlltsd_L1 +
      Dwb_mcs_L1 + Dwb_pcs_L1 + Dgn + Dag + Dag_sq + Deh_c3_Medium +
      Deh_c3_Low + Year_transformed,
    weights = weight
  )

dwb_mcs_model_coef <- dwb_mcs_model$coefficients
dwb_mcs_model_vcov <- sandwich::vcovCL(dwb_mcs_model, cluster = ~pidp)
dwb_mcs_model_cols <-  cbind(dwb_mcs_model_coef, dwb_mcs_model_vcov)
dwb_mcs_model_cols2 <- as.data.frame(dwb_mcs_model_cols)
dwb_mcs_model_cols3 <- rownames_to_column(dwb_mcs_model_cols2, "REGRESSOR") |>
  rename('COEFFICIENT' = 'dwb_mcs_model_coef')

writeData(wb, "UK_DWB_MCS1", dwb_mcs_model_cols3)

## Ouctome: SF12 mental (sf12mcs_dv), as linear regression

dwb_pcs_model <- data_with_dummies_lags %$%
  lm(
    Dwb_pcs ~ 0 + Constant + D_Econ_benefits + D_Home_owner + Dcpst_Single +
      Dcpst_PreviouslyPartnered + Dnc_L1 + Dhe_L1 + UKC + UKD + UKE + UKF +
      UKG + UKH + UKJ + UKK + UKL + UKM + UKN + Ydses_c5_Q2_L1 +
      Ydses_c5_Q3_L1 + Ydses_c5_Q4_L1 + Ydses_c5_Q5_L1 + Dlltsd_L1 +
      Dwb_mcs_L1 + Dwb_pcs_L1 + Dgn + Dag + Dag_sq + Deh_c3_Medium +
      Deh_c3_Low + Year_transformed,
    weights = weight
  )

dwb_pcs_model_coef <- dwb_pcs_model$coefficients
dwb_pcs_model_vcov <- sandwich::vcovCL(dwb_pcs_model, cluster = ~pidp)
dwb_pcs_model_cols <-  cbind(dwb_pcs_model_coef, dwb_pcs_model_vcov)
dwb_pcs_model_cols2 <- as.data.frame(dwb_pcs_model_cols)
dwb_pcs_model_cols3 <- rownames_to_column(dwb_pcs_model_cols2, "REGRESSOR") |>
  rename('COEFFICIENT' = 'dwb_pcs_model_coef')

writeData(wb, "UK_DWB_PCS1", dwb_pcs_model_cols3)

dls_model <- data_with_dummies_lags %$%
  lm(
    Dls ~ 0 + Constant + D_Econ_benefits + D_Home_owner + Dcpst_Single +
      Dcpst_PreviouslyPartnered + Dnc_L1 + Dhe_L1 + UKC + UKD + UKE + UKF +
      UKG + UKH + UKJ + UKK + UKL + UKM + UKN + Ydses_c5_Q2_L1 +
      Ydses_c5_Q3_L1 + Ydses_c5_Q4_L1 + Ydses_c5_Q5_L1 + Dlltsd_L1 +
      Dls_L1 + Dwb_mcs_L1 + Dwb_pcs_L1 + Dgn + Dag + Dag_sq + Deh_c3_Medium +
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
      "Dwb_mcs ~ 0 + {outcome} | {other_cols} + Constant + D_Econ_benefits + D_Home_owner + Dcpst_Single +
    Dcpst_PreviouslyPartnered + Dnc_L1 + Dhe_L1 + UKC + UKD + UKE + UKF +
    UKG + UKH + UKJ + UKK + UKL + UKM + UKN + Ydses_c5_Q2_L1 +
    Ydses_c5_Q3_L1 + Ydses_c5_Q4_L1 + Ydses_c5_Q5_L1 + Dlltsd_L1 +
    Dwb_mcs_L1 + Dwb_pcs_L1 + Dag + Dag_sq + Deh_c3_Medium +
    Deh_c3_Low + Year_transformed",
    other_cols = paste(cols_to_test[cols_to_test != outcome], collapse = " + ")
    )
  ))

pcs_formulae <- cols_to_test |>
  map(\(outcome) Formula::as.Formula(
    glue::glue(
      "Dwb_pcs ~ 0 + {outcome} | {other_cols} + Constant + D_Econ_benefits + D_Home_owner + Dcpst_Single +
    Dcpst_PreviouslyPartnered + Dnc_L1 + Dhe_L1 + UKC + UKD + UKE + UKF +
    UKG + UKH + UKJ + UKK + UKL + UKM + UKN + Ydses_c5_Q2_L1 +
    Ydses_c5_Q3_L1 + Ydses_c5_Q4_L1 + Ydses_c5_Q5_L1 + Dlltsd_L1 +
    Dwb_mcs_L1 + Dwb_pcs_L1 + Dag + Dag_sq + Deh_c3_Medium +
    Deh_c3_Low + Year_transformed",
    other_cols = paste(cols_to_test[cols_to_test != outcome], collapse = " + ")
    )
  ))

dls_formulae <- cols_to_test |>
  map(\(outcome) Formula::as.Formula(
    glue::glue(
      "Dls ~ 0 + {outcome} | {other_cols} + Constant + D_Econ_benefits + D_Home_owner + Dcpst_Single +
    Dcpst_PreviouslyPartnered + Dnc_L1 + Dhe_L1 + UKC + UKD + UKE + UKF +
    UKG + UKH + UKJ + UKK + UKL + UKM + UKN + Ydses_c5_Q2_L1 +
    Ydses_c5_Q3_L1 + Ydses_c5_Q4_L1 + Ydses_c5_Q5_L1 + Dlltsd_L1 +
    Dls_L1 + Dwb_mcs_L1 + Dwb_pcs_L1 + Dag + Dag_sq + Deh_c3_Medium +
    Deh_c3_Low + Year_transformed",
    other_cols = paste(cols_to_test[cols_to_test != outcome], collapse = " + ")
    )
  ))

# mod1 <- feols(
#   Dwb_mcs ~ 0 + EmployedToUnemployed | Constant + D_Econ_benefits + D_Home_owner + Dcpst_Single +
#     Dcpst_PreviouslyPartnered + Dnc_L1 + Dhe_L1 + UKC + UKD + UKE + UKF +
#     UKG + UKH + UKJ + UKK + UKL + UKM + UKN + Ydses_c5_Q2_L1 +
#     Ydses_c5_Q3_L1 + Ydses_c5_Q4_L1 + Ydses_c5_Q5_L1 + Dlltsd_L1 +
#     Dwb_mcs_L1 + Dwb_pcs_L1 + Dgn + Dag + Dag_sq + Deh_c3_Medium +
#     Deh_c3_Low + Year_transformed,
#   data = data_with_dummies_lags,
#   weights = ~ weight
# )

library(furrr)
plan(multisession, workers = 6)

mcs_mods_males <- future_map(.options = furrr_options(seed = TRUE), mcs_formulae, \(form) {
  feols(form, data = data_with_dummies_lags, weights = ~weight, cluster = ~pidp, se = "cluster", subset = data_with_dummies_lags$Dgn == 0)
})

mcs_mods_females <- future_map(.options = furrr_options(seed = TRUE), mcs_formulae, \(form) {
  feols(form, data = data_with_dummies_lags, weights = ~weight, cluster = ~pidp, se = "cluster", subset = data_with_dummies_lags$Dgn == 1)
})

pcs_mods_males <- future_map(.options = furrr_options(seed = TRUE), pcs_formulae, \(form) {
  feols(form, data = data_with_dummies_lags, weights = ~weight, cluster = ~pidp, se = "cluster", subset = data_with_dummies_lags$Dgn == 0)
})

pcs_mods_females <- future_map(.options = furrr_options(seed = TRUE), pcs_formulae, \(form) {
  feols(form, data = data_with_dummies_lags, weights = ~weight, cluster = ~pidp, se = "cluster", subset = data_with_dummies_lags$Dgn == 1)
})

dls_mods_males <- future_map(.options = furrr_options(seed = TRUE), dls_formulae, \(form) {
  feols(form, data = data_with_dummies_lags, weights = ~weight, cluster = ~pidp, se = "cluster", subset = data_with_dummies_lags$Dgn == 0)
})

dls_mods_females <- future_map(.options = furrr_options(seed = TRUE), dls_formulae, \(form) {
  feols(form, data = data_with_dummies_lags, weights = ~weight, cluster = ~pidp, se = "cluster", subset = data_with_dummies_lags$Dgn == 1)
})

mcs_mega_mod_males <- 
  feols(
    EmployedToUnemployed +
    UnemployedToEmployed +
    PersistentUnemployed +
    NonPovertyToPoverty +
    PovertyToNonPoverty +
    PersistentPoverty +
    RealIncomeChange +
    RealIncomeDecrease_D | Constant + D_Econ_benefits + D_Home_owner + Dcpst_Single +
      Dcpst_PreviouslyPartnered + Dnc_L1 + Dhe_L1 + UKC + UKD + UKE + UKF +
      UKG + UKH + UKJ + UKK + UKL + UKM + UKN + Ydses_c5_Q2_L1 +
      Ydses_c5_Q3_L1 + Ydses_c5_Q4_L1 + Ydses_c5_Q5_L1 + Dlltsd_L1 +
      Dls_L1 + Dwb_mcs_L1 + Dwb_pcs_L1 + Dag + Dag_sq + Deh_c3_Medium +
      Deh_c3_Low + Year_transformed,
    weights = ~weight, cluster = ~pidp, se = "cluster", subset = data_with_dummies_lags$Dgn == 0
  )


# combine and write -------------------------------------------------------

mcs_mods_males |> 
  map(\(model) {
    
    term <- names(model$coefficients)
    
    model |> 
      broom::tidy() |> 
      transmute(
        REGRESSOR = term,
        COEFFICIENT = estimate,
        "{term}" := std.error^2
      )
  }) |> 
  reduce(bind_rows) |> 
  map_dfr(replace_na, 0) |> 
  writeData(wb, "UK_DWB_MCS2_Males", x = _)

mcs_mods_females |> 
  map(\(model) {
    
    term <- names(model$coefficients)
    
    model |> 
      broom::tidy() |> 
      transmute(
        REGRESSOR = term,
        COEFFICIENT = estimate,
        "{term}" := std.error^2
      )
  }) |> 
  reduce(bind_rows) |> 
  map_dfr(replace_na, 0) |> 
  writeData(wb, "UK_DWB_MCS2_Females", x = _)

pcs_mods_males |> 
  map(\(model) {
    
    term <- names(model$coefficients)
    
    model |> 
      broom::tidy() |> 
      transmute(
        REGRESSOR = term,
        COEFFICIENT = estimate,
        "{term}" := std.error^2
      )
  }) |> 
  reduce(bind_rows) |> 
  map_dfr(replace_na, 0) |> 
  writeData(wb, "UK_DWB_PCS2_Males", x = _)

pcs_mods_females |> 
  map(\(model) {
    
    term <- names(model$coefficients)
    
    model |> 
      broom::tidy() |> 
      transmute(
        REGRESSOR = term,
        COEFFICIENT = estimate,
        "{term}" := std.error^2
      )
  }) |> 
  reduce(bind_rows) |> 
  map_dfr(replace_na, 0) |> 
  writeData(wb, "UK_DWB_PCS2_Females", x = _)

dls_mods_males |> 
  map(\(model) {
    
    term <- names(model$coefficients)
    
    model |> 
      broom::tidy() |> 
      transmute(
        REGRESSOR = term,
        COEFFICIENT = estimate,
        "{term}" := std.error^2
      )
  }) |> 
  reduce(bind_rows) |> 
  map_dfr(replace_na, 0) |> 
  writeData(wb, "UK_DLS2_Males", x = _)

dls_mods_females |> 
  map(\(model) {
    
    term <- names(model$coefficients)
    
    model |> 
      broom::tidy() |> 
      transmute(
        REGRESSOR = term,
        COEFFICIENT = estimate,
        "{term}" := std.error^2
      )
  }) |> 
  reduce(bind_rows) |> 
  map_dfr(replace_na, 0) |> 
  writeData(wb, "UK_DLS2_Females", x = _)

saveWorkbook(wb, "outfiles/reg_wellbeing.xlsx", overwrite = TRUE)
