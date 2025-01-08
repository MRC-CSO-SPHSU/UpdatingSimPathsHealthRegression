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

# All baseline
library(Formula)
constant <- "Constant"

baseline <- c(
  "D_Econ_benefits",
  "D_Home_owner",
  "Dcpst_Single",
  "Dcpst_PreviouslyPartnered",
  "Deh_c3_Medium",
  "Deh_c3_Low"
)

sex <- "Dgn"

places <-
  c("UKC",
    "UKD",
    "UKE",
    "UKF",
    "UKG",
    "UKH",
    "UKJ",
    "UKK",
    "UKL",
    "UKM",
    "UKN")

age <- c(
  "Dag",
  "Dag_sq"
)

lags <- c(
  "Dnc",
  "Ydses_c5_Q2",
  "Ydses_c5_Q3",
  "Ydses_c5_Q4",
  "Ydses_c5_Q5",
  "Dlltsd"
)

stage2vars <- c(
"EmployedToUnemployed",
"UnemployedToEmployed",
"PersistentUnemployed",
"NonPovertyToPoverty",
"PovertyToNonPoverty",
"PersistentPoverty",
"RealIncomeChange",
"RealIncomeDecrease_D")

stage2 <- paste(stage2vars, collapse = " + ")

time <- "Year_transformed"

mcs <- "Dhe_mcs"
pcs <- "Dhe_pcs"
dls <- "Dls"
dhm <- "Dhm"

lag_vars <- function(...) {
  paste("l(", c(...), ")", sep = "", collapse = " + ")
}

rename_lags <- function(lag_name) str_replace_all(lag_name, "l\\((\\w*).*\\)", "\\1_L1")

mcs_controls <- lag_vars(age, mcs, pcs)
pcs_controls <- paste(paste(age, collapse = " + "), lag_vars(mcs, pcs), sep = " + ")
dls_controls <- lag_vars(age, dls, pcs)
dhm_controls <- lag_vars(age, dhm, pcs)
  
  
common_vars <- 
  paste(
          paste(baseline, collapse = " + "), 
          paste(places, collapse = " + "), 
          paste(glue::glue("l({lags})"), collapse = " + "),
          time,
          sep = " + "
  )

mcs1_formula <- as.formula(
  paste("Dhe_mcs ~",
        paste(
          0,
          common_vars,
          mcs_controls,
          constant,
          sep = " + "
        ))
) |> 
  Formula()

pcs1_formula <- as.formula(
  paste("Dhe_pcs ~",
        paste(
          0,
          common_vars,
          pcs_controls,
          constant,
          sep = " + "
        ))
) |> 
  Formula()

dls1_formula <- as.formula(
  paste("Dls ~",
        paste(
          0,
          common_vars,
          dls_controls,
          constant,
          sep = " + "
        ))
) |> 
  Formula()

dhm1_formula <- as.formula(
  paste("Dhm ~",
        paste(
          0,
          common_vars,
          dhm_controls,
          constant,
          sep = " + "
        ))
) |> 
  Formula()

stage1_mods <- tibble(
  variable = c("UK_DHE_MCS1", "UK_DHE_PCS1", "UK_DLS1", "UK_HM1_L"),
  formula = list(mcs1_formula, pcs1_formula, dls1_formula, dhm1_formula),
  wb = c(wb_health_wb, wb_health_wb, wb_health_wb, wb_mental_health)
) |>
  mutate(model = map(formula, \(mod_form) {
    feols(
      mod_form,
      data = data_with_dummies_lags,
      weights = ~ weight,
      panel.id = ~ pidp + wave
    )
  }),
  out_table = map(model, \(mod_out) {
    broom::tidy(mod_out) |>
      mutate(REGRESSOR = rename_lags(term),
             COEFFICIENT = estimate,
             .keep = "none") |>
      bind_cols(vcov(mod_out)) |>
      rename_with(rename_lags)
    
  }))

stage1_mods |> 
  group_by(variable) |> 
  group_walk(\(results, sheet_name) {
    
    writeData(results$wb[[1]], sheet_name, results$out_table[[1]])
    
  })

# Stage 2

var_matrix <- function(model_in, keep) {
  var_covar <- vcov(model_in)
  var_mat <- var_covar
  var_mat[1:nrow(var_mat), 1:ncol(var_mat)] <- 0
  
  diag(var_mat) <- diag(var_covar)
  
  var_mat[keep, keep]
}

mcs2_formula <- as.formula(
  paste("Dhe_mcs ~",
        paste(
          stage2,
          common_vars,
          mcs_controls,
          sep = " + "
        ))
) |> 
  Formula()

pcs2_formula <- as.formula(
  paste("Dhe_pcs ~",
        paste(
          stage2,
          common_vars,
          pcs_controls,
          sep = " + "
        ))
) |> 
  Formula()

dls2_formula <- as.formula(
  paste("Dls ~",
        paste(
          stage2,
          common_vars,
          dls_controls,
          sep = " + "
        ))
) |> 
  Formula()

dhm2_formula <- as.formula(
  paste("Dhm ~",
        paste(
          stage2,
          common_vars,
          dhm_controls,
          sep = " + "
        ))
) |> 
  Formula()

first_mods <- tibble(
  variable = c("UK_DHE_MCS2_{sex}", "UK_DHE_PCS2_{sex}", "UK_DLS2_{sex}", "UK_HM2_{sex}_L"),
  formula = list(mcs2_formula, pcs2_formula, dls2_formula, dhm2_formula),
  wb = c(wb_health_wb, wb_health_wb, wb_health_wb, wb_mental_health)
) |> 
  cross_join(tibble(sex = c("Males", "Females"))) |> 
  mutate(variable = glue::glue(variable, sex = sex), 
         sex = as.numeric(sex == "Females"), 
         .by = c(variable, sex)) |> 
  mutate(model = map2(formula, sex, \(mod_form, sex_grp) {
    feols(
      mod_form,
      data = data_with_dummies_lags |> filter(Dgn == sex_grp),
      fixef = "pidp",
      weights = ~ weight,
      panel.id = ~ pidp + wave,
      cluster = ~pidp, se = "cluster"
    )
  }))

stage2_mods <- first_mods |> 
  mutate(
  out_table = map(model, \(mod_out) {
    
    broom::tidy(mod_out) |>
      mutate(REGRESSOR = term,
             COEFFICIENT = estimate,
             .keep = "none") |>
      filter(REGRESSOR %in% stage2vars) |> 
      bind_cols(
        var_matrix(mod_out, stage2vars)
      ) 
  })) 

stage2_mods |> 
  group_by(variable) |> 
  group_walk(\(results, sheet_name) {
    
    writeData(results$wb[[1]], sheet_name, results$out_table[[1]])
    
  })


saveWorkbook(wb_health_wb, "outfiles/reg_health_wellbeing2.xlsx", overwrite = TRUE)
saveWorkbook(wb_mental_health, "outfiles/reg_health_mental2.xlsx", overwrite = TRUE)
