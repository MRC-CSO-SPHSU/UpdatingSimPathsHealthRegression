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
                                exp_emp, exp_poverty, exp_incchange,
                                gor_dv,
                                pidp,
                                intdaty_dv,
                                scghq1_dv,
                                econ_benefits, home_owner, mastat_dv, dnc, ydses_c5, dlltsd,
                                indscus_lw,
                                # btype10,
                                jbstat, fimnlabgrs_dv, fiyrinvinc_dv, econ_poverty, log_income, econ_realequivinc
                 )) |> 
  arrange(pidp, wave)


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
    wave = wave,
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
    EmployedToUnemployed = as.numeric(exp_emp == 13),
    UnemployedToEmployed = as.numeric(exp_emp == 31),
    PersistentUnemployed = as.numeric(exp_emp == 33),
    NonPovertyToPoverty = as.numeric(exp_poverty == 1),
    PovertyToNonPoverty = as.numeric(exp_poverty == 2),
    PersistentPoverty = as.numeric(exp_poverty == 3),
    # RealIncomeChange = as.numeric(exp_incchange == 1),
    RealIncomeDecrease_D = exp_incchange,
    weight = indscus_lw,
    les_c4 = case_when(
      jbstat %in% c(1, 2, 5, 12, 13, 14) ~ "Employed or self-employed",
      jbstat == 7 ~ "Student",
      jbstat %in% c(3, 6, 8, 10, 11, 97, 9) ~ "Not employed",
      jbstat == 4 ~ "Retired"
    ),
    Constant = 1,
    les_c4 = les_c4,
    econ_poverty = econ_poverty,
    log_income = log_income,
    econ_realequivinc = econ_realequivinc,
    exp_emp = as_factor(exp_emp),
    exp_incchange = as_factor(exp_incchange),
    exp_poverty = as_factor(exp_poverty),
    mastat_dv = as_factor(mastat_dv),
    gor_dv = as_factor(gor_dv),
    ydses_c5 = as_factor(ydses_c5),
    hiqual_dv = as_factor(hiqual_dv),
    .keep = "used"
  ) |> 
  left_join(codes_translate, by = "gor_dv") |> 
  mutate(across(c(Dag, Dgn, Dls, Dhe_mcs, Dhe_pcs, Dhm, Dhe, D_Econ_benefits, Dlltsd, D_Home_owner), zap_labels)) |> 
  mutate(drgnl = relevel(factor(drgnl), ref = "UKI"))

## lagged variables to be used as confounders: econ_benefits, home_owner, mastat_dv, dnc, gor_dv, and outcome measures

data_with_dummies <- data_ready |> 
  fastDummies::dummy_cols(select_columns = c("Ydses_c5"), remove_first_dummy = TRUE) |> 
  fastDummies::dummy_cols(select_columns = c("Deh_c3", "Dcpst"), remove_first_dummy = TRUE) |> 
  fastDummies::dummy_cols(select_columns = c("drgnl"), remove_first_dummy = TRUE, omit_colname_prefix = TRUE) |> 
  select(
    pidp,
    wave,
    Year_transformed,
    weight,
    Dgn, 
    Dnc,
    starts_with("Ydses", ignore.case = FALSE),
    Dlltsd,
    Dag,
    Dag_sq,
    Dhe_mcs,
    Dhe_pcs,
    Dhm,
    Dls,
    EmployedToUnemployed,
    UnemployedToEmployed,
    PersistentUnemployed,
    NonPovertyToPoverty,
    PovertyToNonPoverty,
    PersistentPoverty,
    # RealIncomeChange,
    RealIncomeDecrease_D,
    starts_with("UK", ignore.case = FALSE),
    D_Econ_benefits,
    D_Home_owner,
    Dcpst_Single,
    Dcpst_PreviouslyPartnered,
    starts_with("Deh", ignore.case = FALSE),
    -ends_with("NA"),
    Constant, 
    les_c4, econ_poverty, log_income, econ_realequivinc,
    exp_emp, exp_incchange, exp_poverty, scghq1_dv, starts_with("sf12"), sclfsato,
    sex_dv, age_dv, age_sq,
    econ_benefits, home_owner, mastat_dv, dnc, sf12pcs_dv,
    gor_dv, age_dv, age_sq, ydses_c5,
    hiqual_dv, sex_dv, intdaty_dv
  ) 

library(dtplyr)

# econ_benefits_L1 + home_owner_L1 +
#   mastat_dv_L1 + dnc_L1 + sf12pcs_dv_L1 +
#   gor_dv_L1 + age_dv_L1 + age_sq + ydses_c5_L1 +
#   scghq1_dv_L1 +

lagged_vars <- c(
  "Dnc", "Ydses_c5_Q2", "Ydses_c5_Q3", "Ydses_c5_Q4", "Ydses_c5_Q5", "Dlltsd", "Dag", "Dag_sq",
  "Dhe_mcs", "Dhe_pcs", "Dhm", "Dls", "D_Home_owner",
  "D_Econ_benefits",
  "Dcpst_Single", "Dcpst_PreviouslyPartnered",
  "les_c4", "econ_poverty", "log_income", "econ_realequivinc",
  "scghq1_dv",
  "exp_emp",
  "exp_incchange",
  "exp_poverty",
  "econ_benefits",
  "home_owner",
  "mastat_dv",
  "dnc",
  "sf12pcs_dv",
  "gor_dv",
  "age_dv",
  "age_sq",
  "ydses_c5"
)

lagged_names <- paste0(lagged_vars, "_L1")

data_with_dummies_dt <- as.data.table(data_with_dummies) |> 
  panel(~pidp + wave)

data_with_dummies_dt[, (lagged_names) := .(
  l(Dnc),
  l(Ydses_c5_Q2),
  l(Ydses_c5_Q3),
  l(Ydses_c5_Q4),
  l(Ydses_c5_Q5),
  l(Dlltsd),
  l(Dag),
  l(Dag_sq),
  l(Dhe_mcs),
  l(Dhe_pcs),
  l(Dhm),
  l(Dls),
  l(D_Home_owner),
  l(D_Econ_benefits),
  l(Dcpst_Single),
  l(Dcpst_PreviouslyPartnered),
  l(les_c4),
  l(econ_poverty),
  l(log_income),
  l(econ_realequivinc),
  l(scghq1_dv),
  l(exp_emp),
  l(exp_incchange),
  l(exp_poverty),
  l(econ_benefits),
  l(home_owner),
  l(mastat_dv),
  l(dnc),
  l(sf12pcs_dv),
  l(gor_dv),
  l(age_dv),
  l(age_sq),
  l(ydses_c5)
)]

data_with_dummies_dt[, `:=`(RealIncomeChange = d(log_income), D_Econ_benefits = D_Econ_benefits_L1)]

data_in_panel <- copy(data_with_dummies_dt)

data_with_dummies_dt[, `:=`(
  EmployedToUnemployed = as.integer(
    les_c4_L1 ==
      "Employed or self-employed" &
      les_c4 == "Not employed" &
      Dlltsd == 0
  ),
  UnemployedToEmployed = as.integer(
    les_c4_L1 ==
      "Not employed" &
      les_c4 == "Employed or self-employed" &
      Dlltsd == 0
  ),
  PersistentUnemployed = as.integer(les_c4_L1 ==
                                      "Not employed" &
                                      les_c4 == "Not employed" & Dlltsd == 0),
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
    log_income_L1
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
    Constant
  )
