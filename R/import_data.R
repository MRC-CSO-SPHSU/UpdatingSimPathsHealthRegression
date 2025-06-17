library(tidyverse)
library(haven)
library(data.table)
library(fixest)

## NOTES ## 
# Check estimate directions make sense
# Test whether odds between levels are equivalent

cols <- read_dta('T:/projects/HEED/DataAnalysis/Daniel\'s working files/heed_analysis.dta', n_max = 100)
cols <- read_dta('T:/projects/HEED/DataAnalysis/hfcovid_analysis.dta', n_max = 100)
cols <- read_dta('T:/projects/HEED/Data/USoc prepared data/heed_analysis.dta', n_max = 100)

data <- read_dta('T:/projects/HEED/DataAnalysis/Daniel\'s working files/heed_analysis.dta',
                 col_select = c(pidp, hidp, wave, sppid,
                                sex_dv, age_dv,
                                # age_sq, 
                                deh_c3,
                                scsf1, sclfsato, sf12pcs_dv, sf12mcs_dv, scghq1_dv,
                                exp_emp, exp_poverty, exp_incchange,
                                lhw,
                                gor_dv,
                                ethn_dv,
                                intdaty_dv,
                                scghq1_dv,
                                econ_benefits, econ_benefits_uc, econ_benefits_nonuc,
                                # home_owner,
                                hsownd,
                                dcpst, dnc, ydses_c5, dlltsd,
                                dot, finnow,
                                indscus_lw,
                                jbstat, fimnlabgrs_dv, fiyrinvinc_dv, econ_poverty, log_income, econ_realequivinc
                 )) |> 
  arrange(pidp, wave)


codes_translate <- 
  tribble(
    ~drgnl, ~gor_dv,
    "UKC", "North East",
    "UKD", "North West",
    "UKE", "Yorkshire and the Humber",
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


# cut(
#     c(0, 5, 15, 25, 35, 45),
#     c(0, 5, 15, 25, 35, Inf),
#     right = TRUE,,
#     labels = c("ZERO", "TEN", "TWENTY", "THIRTY", "FORTY"),
#     include.lowest = TRUE
#   )

data_ready <- data |> 
  filter(!is.na(dot)) |> 
  mutate(
    pidp = pidp,
    wave = wave,
    hidp = hidp,
    sppid = sppid,
    D_Econ_benefits = as.integer(econ_benefits),
    D_Econ_benefits_UC = as.integer(econ_benefits_uc),
    D_Econ_benefits_NonUC = as.integer(econ_benefits_nonuc),
    D_Home_owner = as.integer(hsownd>=1 & hsownd<=3),
    # D_Home_owner = home_owner,
    Dcpst = as_factor(dcpst) |>
      fct_relabel( ~ str_extract(.x, "(?<=\\. ).*")) |> 
      fct_recode(Single = "Single never married", PreviouslyPartnered = "Previously partnered"),
    Dnc = dnc,
    Dhe = scsf1,
    Lhw = cut(
      lhw,
      c(0, 5, 15, 25, 35, Inf),
      right = TRUE,,
      labels = c("ZERO", "TEN", "TWENTY", "THIRTY", "FORTY"),
      include.lowest = TRUE
    ),
    gor_dv = as_factor(gor_dv) |> str_extract("(?<=\\. ).*"),
    Ydses_c5 = factor(ydses_c5) |> fct_relabel(~paste0("Q", .x)),
    Dlltsd = as.integer(dlltsd),
    Dgn = as.integer(sex_dv == 1),
    Dag = age_dv,
    age_sq = age_dv^2,
    Dag_sq = age_sq,
    Deh_c3 = as_factor(deh_c3) |> fct_relabel(~str_extract(.x, "(?<=\\. ).*")),
    Year_transformed = (intdaty_dv |> zap_labels()) - 2000,
    Dls = sclfsato,
    Dhm = scghq1_dv,
    Dhe_mcs = sf12mcs_dv,
    Dhe_pcs = sf12pcs_dv,
    FinancialDistress = as.integer(zap_label(finnow) %in% 4:5),
    Ethnicity = as_factor(dot) |> fct_relabel(~str_extract(.x, "^\\w*")),
    EmployedToUnemployed = as.integer(exp_emp == 13),
    UnemployedToEmployed = as.integer(exp_emp == 31),
    PersistentUnemployed = as.integer(exp_emp == 33),
    NonPovertyToPoverty = as.integer(exp_poverty == 1),
    PovertyToNonPoverty = as.integer(exp_poverty == 2),
    PersistentPoverty = as.integer(exp_poverty == 3),
    # RealIncomeChange = as.numeric(exp_incchange == 1),
    RealIncomeDecrease_D = as.integer(exp_incchange),
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
    dcpst = as_factor(dcpst),
    gor_dv = as_factor(gor_dv),
    ydses_c5 = as_factor(ydses_c5),
    .keep = "used"
  ) |> 
  left_join(codes_translate, by = "gor_dv") |> 
  mutate(across(c(Dag, Dls, Dhe_mcs, Dhe_pcs, Dhm, Dhe, starts_with("D_Econ_benefits"), Dlltsd, D_Home_owner, sppid), zap_labels)) |> 
  mutate(drgnl = relevel(factor(drgnl), ref = "UKI"))

partner_hours <- data_ready |> 
  select(hidp, pidp, sppid, wave, Lhw) |> 
  filter(sppid != -8) |> 
  mutate(Lhwsp_c6 = Lhw, sppid = pidp) |> 
  select(hidp, wave, Lhwsp_c6, sppid) 

data_ready <- left_join(data_ready, partner_hours, by = join_by(sppid == sppid, wave == wave, hidp == hidp)) |> 
  mutate(Lhwsp_c6 = if_else(sppid == -8, "NO_PARTNER", Lhwsp_c6))

## lagged variables to be used as confounders: econ_benefits, home_owner, dcpst, dnc, gor_dv, and outcome measures

data_with_dummies <- data_ready |> 
  fastDummies::dummy_cols(select_columns = c("Ydses_c5"), remove_first_dummy = TRUE) |> 
  fastDummies::dummy_cols(select_columns = c("Deh_c3", "Dcpst"), remove_first_dummy = TRUE) |> 
  fastDummies::dummy_cols(select_columns = c("Lhw", "Lhwsp_c6", "Ethnicity"), remove_first_dummy = FALSE) |> 
  fastDummies::dummy_cols(select_columns = c("drgnl"), remove_first_dummy = TRUE, omit_colname_prefix = TRUE) |> 
  mutate(
    across(starts_with("Lhw_"), ~as.integer(.x * D_Econ_benefits_UC), .names = "D_Econ_benefits_UC_{.col}")) |> 
  rename_with(\(eth_col) str_remove(eth_col, "_"), .cols = starts_with("Ethnicity")) |> 
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
    FinancialDistress,
    starts_with("UK", ignore.case = FALSE),
    D_Econ_benefits,
    starts_with("D_Econ_benefits_UC"),
    D_Econ_benefits_NonUC,
    D_Home_owner,
    Dcpst_Single,
    Dcpst_PreviouslyPartnered,
    starts_with("Deh", ignore.case = FALSE),
    starts_with("Ethnicity", ignore.case = FALSE),
    starts_with("Lhw"),
    -ends_with("NA"),
    Constant, 
    les_c4, econ_poverty, log_income, econ_realequivinc,
    exp_emp, exp_incchange, exp_poverty, scghq1_dv, starts_with("sf12"), sclfsato,
    sex_dv, age_dv, age_sq,
    econ_benefits, dcpst, dnc, sf12pcs_dv,
    gor_dv, age_dv, age_sq, ydses_c5,
    sex_dv, intdaty_dv
  ) 

library(dtplyr)

# econ_benefits_L1 + home_owner_L1 +
#   dcpst_L1 + dnc_L1 + sf12pcs_dv_L1 +
#   gor_dv_L1 + age_dv_L1 + age_sq + ydses_c5_L1 +
#   scghq1_dv_L1 +

lagged_vars <- c(
  "Dnc", "Ydses_c5_Q2", "Ydses_c5_Q3", "Ydses_c5_Q4", "Ydses_c5_Q5", "Dlltsd", "Dag", "Dag_sq",
  "Dhe_mcs", "Dhe_pcs", "Dhm", "Dls", "D_Home_owner",
  "D_Econ_benefits",
  "D_Econ_benefits_UC",
  "D_Econ_benefits_NonUC",
  "Dcpst_Single", "Dcpst_PreviouslyPartnered",
  "les_c4", "econ_poverty", "log_income", "econ_realequivinc",
  "scghq1_dv",
  "exp_emp",
  "exp_incchange",
  "exp_poverty",
  "econ_benefits",
  "dcpst",
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
  l(D_Econ_benefits_UC),
  l(D_Econ_benefits_NonUC),
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
  l(dcpst),
  l(dnc),
  l(sf12pcs_dv),
  l(gor_dv),
  l(age_dv),
  l(age_sq),
  l(ydses_c5)
)]

data_with_dummies_dt[, `:=`(RealIncomeChange = d(log_income), D_Econ_benefits = D_Econ_benefits_L1)]

data_in_panel <- copy(data_with_dummies_dt) |> 
  as_tibble()

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
    FinancialDistress,
    starts_with("UK", ignore.case = FALSE),
    D_Econ_benefits,
    D_Home_owner,
    Dcpst_Single,
    Dcpst_PreviouslyPartnered,
    starts_with("Deh", ignore.case = FALSE),
    starts_with("Ethnicity", ignore.case = FALSE),
    -ends_with("NA"),
    Constant
  )

alt_weights <- read_rds("data/cross_wave_weights.rds")[[6]] |> 
  select(pidp, weight_xw = f_indscui_xw)

panel_xs_weighted <- inner_join(data_in_panel, alt_weights, by = "pidp")

