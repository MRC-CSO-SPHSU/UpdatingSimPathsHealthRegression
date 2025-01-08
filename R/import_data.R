library(tidyverse)
library(haven)
library(data.table)

## NOTES ## 
# Check estimate directions make sense
# Test whether odds between levels are equivalent

# cols <- read_dta('T:/projects/HEED/DataAnalysis/hfcovid_analysis.dta', n_max = 100)

data <- read_dta('T:/projects/HEED/DataAnalysis/hfcovid_analysis.dta',
                 col_select = c(pidp, hidp, wave,
                                sex_dv, age_dv, age_sq, hiqual_dv,
                                scsf1, sclfsato, sf12pcs_dv, sf12mcs_dv, scghq1_dv,
                                gor_dv,
                                pidp,
                                intdaty_dv,
                                scghq1_dv,
                                econ_benefits, home_owner, mastat_dv, dnc, ydses_c5, dlltsd,
                                indscus_lw,
                                jbstat, fimnlabgrs_dv, fiyrinvinc_dv, econ_poverty, log_income, econ_realequivinc
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
  mutate(
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
    Dhm = scghq1_dv,
    Dhe_mcs = sf12mcs_dv,
    Dhe_pcs = sf12pcs_dv,
    weight = indscus_lw,
    les_c4 = case_when(
      jbstat %in% c(1, 2, 5, 12, 13, 14) ~ "Employed or self-employed",
      jbstat == 7 ~ "Student",
      jbstat %in% c(3, 6, 8, 10, 11, 97, 9) ~ "Not employed",
      jbstat == 4 ~ "Retired"
    )
  ) |> 
  left_join(codes_translate, by = "gor_dv") |> 
  mutate(across(c(Dag, Dgn, Dls, Dhe_mcs, Dhe_pcs, Dhm, Dhe, D_Econ_benefits, Dlltsd, D_Home_owner), zap_labels)) |> 
  mutate(drgnl = relevel(factor(drgnl), ref = "UKI")) |> 
  select(-gor_dv)

## lagged variables to be used as confounders: econ_benefits, home_owner, mastat_dv, dnc, gor_dv, and outcome measures

data_with_dummies <- data_ready |> 
  fastDummies::dummy_cols(select_columns = c("Ydses_c5"), remove_first_dummy = TRUE) |> 
  fastDummies::dummy_cols(select_columns = c("Deh_c3", "Dcpst"), remove_first_dummy = TRUE) |> 
  fastDummies::dummy_cols(select_columns = c("drgnl"), remove_first_dummy = TRUE, omit_colname_prefix = TRUE) 

library(dtplyr)

lagged_vars <- c(
  "Dnc", "Ydses_c5_Q2", "Ydses_c5_Q3", "Ydses_c5_Q4", "Ydses_c5_Q5", "Dlltsd", "Dag", "Dag_sq",
  "Dhe_mcs", "Dhe_pcs", "Dhm", "Dls",
  "les_c4", "econ_poverty", "log_income", "econ_realequivinc"
)

lagged_names <- paste0(lagged_vars, "_L1")

data_with_dummies_dt <- as.data.table(data_with_dummies)

data_with_dummies_dt[, (lagged_names) := shift(.SD), by = pidp, .SDcols = lagged_vars]

data_with_dummies_dt[, `:=`(
  EmployedToUnemployed = as.integer(
    les_c4_L1 ==
      "Employed or self-employed" &
      les_c4 == "Not employed" &
      dlltsd == 0
  ),
  UnemployedToEmployed = as.integer(
    les_c4_L1 ==
      "Not employed" &
      les_c4 == "Employed or self-employed" &
      dlltsd == 0
  ),
  PersistentUnemployed = as.integer(les_c4_L1 ==
                                      "Not employed" &
                                      les_c4 == "Not employed" & dlltsd == 0),
  NonPovertyToPoverty = as.integer(econ_poverty_L1 == 0 &
                                     econ_poverty ==
                                     1),
  PovertyToNonPoverty = as.integer(econ_poverty_L1 ==
                                     1 &
                                     econ_poverty == 0),
  PersistentPoverty = as.integer(econ_poverty_L1 ==
                                   1 &
                                   econ_poverty == 1),
  RealIncomeChange = log_income -
    log_income_L1,
  RealIncomeDecrease_D = as.integer(econ_realequivinc <
                                      econ_realequivinc_L1)
)]

data_with_dummies_lags <- data_with_dummies_dt |> 
  as_tibble() |> 
  filter(!if_any(!ends_with("L1"), is.na)) |> 
  select(
    pidp,
    wave,
    Year_transformed,
    weight,
    Dgn, 
    Dnc,
    Dnc_L1,
    starts_with("Ydses", ignore.case = FALSE),
    Dlltsd,
    Dlltsd_L1,
    Dag,
    Dag_L1,
    Dag_sq,
    Dag_sq_L1,
    Dhe_mcs,
    Dhe_mcs_L1,
    Dhe_pcs,
    Dhe_pcs_L1,
    Dhm,
    Dhm_L1,
    Dls,
    Dls_L1,
    EmployedToUnemployed,
    UnemployedToEmployed,
    PersistentUnemployed,
    NonPovertyToPoverty,
    PovertyToNonPoverty,
    PersistentPoverty,
    RealIncomeChange,
    RealIncomeDecrease_D,
    starts_with("UK", ignore.case = FALSE),
    D_Econ_benefits,
    D_Home_owner,
    Dcpst_Single,
    Dcpst_PreviouslyPartnered,
    starts_with("Deh", ignore.case = FALSE),
    -ends_with("NA"),
  ) |> 
  mutate(Constant = 1)
