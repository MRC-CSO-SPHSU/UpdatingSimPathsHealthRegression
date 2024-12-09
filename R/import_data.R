library(tidyverse)
library(haven)

## NOTES ## 
# Check estimate directions make sense
# Test whether odds between levels are equivalent


data <- read_dta('T:/projects/HEED/DataAnalysis/hfcovid_analysis.dta',
                 col_select = c(pidp, hidp, wave,
                                sex_dv, age_dv, age_sq, hiqual_dv,
                                scsf1, sclfsato, sf12pcs_dv, sf12mcs_dv,
                                gor_dv,
                                pidp,
                                intdaty_dv,
                                scghq1_dv,
                                econ_benefits, home_owner, mastat_dv, dnc, ydses_c5, dlltsd,
                                indscus_lw
                 ))


codes_translate <- 
  tribble(
    ~drgnl, ~gor_dv,
    "UKC", "North East",
    "UKD", "North West",
    "UKE", "Yorkshire and The Humber",
    "UKF", "East Midlands",
    "UKG", "West Midlands",
    "UKH", "East of England",
    "UKI", "London",
    "UKJ", "South East",
    "UKK", "South West",
    "UKL", "Wales",
    "UKM", "Scotland",
    "UKN", "Northern Ireland"
  )


data_ready <- data |> 
  transmute(
    pidp = pidp,
    D_Econ_benefits = econ_benefits,
    D_Home_owner = home_owner,
    Dcpst = as_factor(mastat_dv) |>
      fct_relabel( ~ str_extract(.x, "(?<=: ).*")) |> 
      fct_recode(Single = "Single and never married", PreviouslyPartnered = "Previously partnered"),
    Dnc = dnc,
    Dhe = scsf1,
    gor_dv = as_factor(gor_dv) |> str_extract("(?<=drgnl: ).*"),
    Ydses_c5 = factor(ydses_c5) |> fct_relabel(~paste0("Q", .x)),
    Dlltsd = dlltsd,
    Dgn = sex_dv,
    Dag = age_dv,
    Dag_sq = age_sq,
    Deh_c3 = as_factor(hiqual_dv) |> fct_relabel(~str_extract(.x, "(?<=: ).*")),
    Year_transformed = intdaty_dv |> zap_labels(),
    Dls = sclfsato,
    Dwb_mcs = sf12mcs_dv,
    Dwb_pcs = sf12pcs_dv,
    weight = indscus_lw,
    EmployedToUnemployed = sample(0:1, nrow(data), replace = TRUE),
    UnemployedToEmployed = sample(0:1, nrow(data), replace = TRUE),
    PersistentUnemployed = sample(0:1, nrow(data), replace = TRUE),
    NonPovertyToPoverty = sample(0:1, nrow(data), replace = TRUE),
    PovertyToNonPoverty = sample(0:1, nrow(data), replace = TRUE),
    PersistentPoverty = sample(0:1, nrow(data), replace = TRUE),
    RealIncomeChange = sample(0:1, nrow(data), replace = TRUE),
    RealIncomeDecrease_D = sample(0:1, nrow(data), replace = TRUE),
    Covid_2020_D = (intdaty_dv == 20) * 1,
    Covid_2021_D = (intdaty_dv == 21) * 1,
  ) |> 
  left_join(codes_translate, by = "gor_dv") |> 
  mutate(across(c(Dag, Dgn, Dls, Dwb_mcs, Dwb_pcs, Dhe, D_Econ_benefits, Dlltsd, D_Home_owner), zap_labels)) |> 
  mutate(drgnl = relevel(factor(drgnl), ref = "UKI")) |> 
  drop_na() |> 
  select(-gor_dv)

## lagged variables to be used as confounders: econ_benefits, home_owner, mastat_dv, dnc, gor_dv, and outcome measures

data_with_dummies_lags <- data_ready |> 
  fastDummies::dummy_cols(select_columns = c("Ydses_c5"), remove_first_dummy = TRUE) |> 
  fastDummies::dummy_cols(select_columns = c("Deh_c3", "Dcpst"), remove_first_dummy = TRUE) |> 
  fastDummies::dummy_cols(select_columns = c("drgnl"), remove_first_dummy = TRUE, omit_colname_prefix = TRUE) |> 
  mutate(across(everything(), lag, .names = "{.col}_L1"), Constant = 1) 
