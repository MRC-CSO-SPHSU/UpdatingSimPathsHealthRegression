library(tidyverse)
library(openxlsx)
library(magrittr)
library(fixest)
source("R/import_data.R")

data_in_panel <- panel_xs_weighted

wb_health_wb <- createWorkbook()
wb_mental_health <- createWorkbook()

expand_grid(
  c("UK_"),
  c("DHE_MCS", "DHE_PCS", "DLS"),
  c("1", "2_Males", "2_Females")
) |> 
  pmap(paste0) |> 
  reduce(c) |> 
  walk(\(new_col) {addWorksheet(new_col, wb = wb_health_wb) 
    setColWidths(wb = wb_health_wb, new_col, 1, 30)})

expand_grid(
  c("UK_"),
  c("HM"),
  c("1_L", "2_Males_L", "2_Females_L")
) |> 
  pmap(paste0) |> 
  reduce(c) |> 
  walk(\(new_col) {addWorksheet(new_col, wb = wb_mental_health) 
    setColWidths(wb = wb_mental_health, new_col, 1, 30)})

# local demog 	"i.dgn L.dag L.dagsq i.deh_c3 i.dot"
# local health 	"dhe_pcs"
# local area 		"ib8.drgn"
# local id 		"idperson"
# local time 		"stm"
# local restrictions		"stm!=20 & stm!=21 & dag>=25 & dag<=64 & wave!=12"
# local exposures2			"ib13.exp_emp i.econ_benefits_uc##i.lhw_c5  ib999.lhwsp_c6 i.econ_benefits_nonuc D.log_income i.exp_incchange ib1.exp_poverty  "

#' Ethincity to be added still!
#' Adding in:
#' - UC*work hours interactions (LHW_c5)
#' - non-UC benefits
#' - Partner's work hours

constant <- "Constant"

baseline <- c(
  "Deh_c3_Medium",
  "Deh_c3_Low"
)

sex <- "Dgn"

ethnicity <- c(
  # "EthnicityBlack",
  # "EthnicityAsian",
  # "EthnicityMixed",
  # "EthnicityOther"
  "EthnicityWhite"
)

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
  "D_Econ_benefits",
  "D_Home_owner_L1",
  "Dcpst_Single_L1",
  "Dcpst_PreviouslyPartnered_L1",
  "Dnc_L1",
  "Ydses_c5_Q2_L1",
  "Ydses_c5_Q3_L1",
  "Ydses_c5_Q4_L1",
  "Ydses_c5_Q5_L1"
  # "Dlltsd_L1"
)

stage2vars <- c(
  "EmployedToUnemployed",
  "UnemployedToEmployed",
  "PersistentUnemployed",
  "NonPovertyToPoverty",
  "PovertyToNonPoverty",
  "PersistentPoverty",
  "RealIncomeChange",
  "FinancialDistress",
  "D_Econ_benefits_UC_Lhw_ZERO",
  "D_Econ_benefits_UC_Lhw_TEN",
  "D_Econ_benefits_UC_Lhw_TWENTY",
  "D_Econ_benefits_UC_Lhw_THIRTY",
  "D_Econ_benefits_UC_Lhw_FORTY",
  "D_Econ_benefits_NonUC",
  "Lhwsp_c6_ZERO",
  "Lhwsp_c6_TEN",
  "Lhwsp_c6_TWENTY",
  "Lhwsp_c6_THIRTY",
  "Lhwsp_c6_FORTY",
  "RealIncomeDecrease_D")

stage2 <- paste(stage2vars, collapse = " + ")

time <- "Year_transformed"

mcs <- "Dhe_mcs"
pcs <- "Dhe_pcs"
dls <- "Dls"
dhm <- "Dhm"

lag_vars <- function(...) {
  paste(paste(c(...), "_L1", sep = ""), collapse = " + ")
}

rename_lags <- function(lag_name) str_replace_all(lag_name, "l\\((\\w*).*\\)", "\\1_L1")
rename_diffs <- function(diff_name) str_replace_all(diff_name, "d\\((\\w*).*\\)", "\\1") |> str_replace("log_income", "RealIncomeChange")

mcs_controls <- lag_vars(age, mcs, pcs)
pcs_controls <- paste(paste(age, collapse = " + "), lag_vars(mcs, pcs), sep = " + ")
dls_controls <- lag_vars(age, dls, pcs)
dhm_controls <- lag_vars(age, dhm, pcs)


common_vars <- 
  paste(
    paste(baseline, collapse = " + "), 
    paste(places, collapse = " + "), 
    paste(ethnicity, collapse = " + "),
    # "Ethnicity",
    paste(lags, collapse = " + "),
    time,
    sep = " + "
  )

mcs1_formula <- as.formula(
  paste("Dhe_mcs ~",
        paste(
          0,
          common_vars,
          mcs_controls,
          sex,
          constant,
          sep = " + "
        ))
)  

pcs1_formula <- as.formula(
  paste("Dhe_pcs ~",
        paste(
          0,
          common_vars,
          pcs_controls,
          sex,
          constant,
          sep = " + "
        ))
)  

dls1_formula <- as.formula(
  paste("Dls ~",
        paste(
          0,
          common_vars,
          dls_controls,
          sex,
          constant,
          sep = " + "
        ))
)  

dhm1_formula <- as.formula(
  paste("Dhm ~",
        paste(
          0,
          common_vars,
          dhm_controls,
          sex,
          constant,
          sep = " + "
        ))
)  

stage1_mods <- tibble(
  variable = c("UK_DHE_MCS1", "UK_DHE_PCS1", "UK_DLS1", "UK_HM1_L"),
  formula = list(mcs1_formula, pcs1_formula, dls1_formula, dhm1_formula),
  wb = c(wb_health_wb, wb_health_wb, wb_health_wb, wb_mental_health)
) |>
  mutate(model = map(formula, \(mod_form) {
    feols(
      mod_form,
      data = data_in_panel,
      cluster = ~pidp,
      se = "cluster",
      weights = ~ weight_xw,
    )
  }),
  out_table = map(model, \(mod_out) {
    broom::tidy(mod_out) |>
      mutate(REGRESSOR = rename_lags(term) |> rename_diffs(),
             COEFFICIENT = estimate,
             .keep = "none") |>
      bind_cols(vcov(mod_out)) |>
      rename_with(compose(rename_lags, rename_diffs))
    
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
        ), "| pidp")
)  

pcs2_formula <- as.formula(
  paste("Dhe_pcs ~",
        paste(
          stage2,
          common_vars,
          pcs_controls,
          sep = " + "
        ), "| pidp")
)  

dls2_formula <- as.formula(
  paste("Dls ~",
        paste(
          stage2,
          common_vars,
          dls_controls,
          sep = " + "
        ), "| pidp")
)  

dhm2_formula <- as.formula(
  paste("Dhm ~",
        paste(
          stage2,
          common_vars,
          dhm_controls,
          sep = " + "
        ), "| pidp")
)  

first_mods <- tibble(
  variable = c("UK_DHE_MCS2_{sex}", "UK_DHE_PCS2_{sex}", "UK_DLS2_{sex}", "UK_HM2_{sex}_L"),
  formula = list(mcs2_formula, pcs2_formula, dls2_formula, dhm2_formula),
  wb = c(wb_health_wb, wb_health_wb, wb_health_wb, wb_mental_health)
) |> 
  cross_join(tibble(sex = c("Males", "Females"))) |> 
  mutate(variable = glue::glue(variable, sex = sex), 
         sex = as.numeric(sex == "Males"), 
         .by = c(variable, sex)) |> 
  mutate(model = map2(formula, sex, \(mod_form, sex_grp) {
    feols(
      mod_form,
      data = data_in_panel,
      weights = ~ weight_xw,
      cluster = ~pidp, se = "cluster",
      subset = as.formula(glue::glue("~Dgn == {sex_grp} & Dag %in% 25:64"))
    )
  }))

stage2_mods <- first_mods |> 
  mutate(
    out_table = map(model, \(mod_out) {
      broom::tidy(mod_out) |>
        mutate(REGRESSOR = rename_lags(term) |> rename_diffs(),
               COEFFICIENT = estimate,
               .keep = "none") |>
        filter(REGRESSOR %in% rename_diffs(stage2vars)) |> 
        bind_cols(
          var_matrix(mod_out, stage2vars)
        )  |>
        rename_with(compose(rename_lags, rename_diffs))
    })) 

stage2_mods |> 
  group_by(variable) |> 
  group_walk(\(results, sheet_name) {
    
    writeData(results$wb[[1]], sheet_name, results$out_table[[1]])
    
  })


saveWorkbook(wb_health_wb, "outfiles/reg_health_wellbeing_uc_effects.xlsx", overwrite = TRUE)
saveWorkbook(wb_mental_health, "outfiles/reg_health_mental_uc_effects.xlsx", overwrite = TRUE)


