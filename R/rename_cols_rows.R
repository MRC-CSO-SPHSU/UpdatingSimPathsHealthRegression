library(tidyverse)

hm1_table <- readxl::read_xlsx("data/reg_health_mental.xlsx", sheet = "UK_HM1_L")
hm2_table <- readxl::read_xlsx("data/reg_health_mental.xlsx", sheet = "UK_HM2_Males_L")

dwb_mcs1_estimates <- readxl::read_xlsx("data/sf12mcs_output.xlsx")
dwb_pcs1_estimates <- readxl::read_xlsx("data/sf12pcs_output.xlsx")
dls1_estimates <- readxl::read_xlsx("data/lfsat_ouput.xlsx")

colnames(hm1_table) |> cat(sep = "\n")
colnames(dwb_mcs1_estimates) |> cat(sep = "\n")
colnames(dwb_pcs1_estimates) |> cat(sep = "\n")
colnames(dls1_estimates) |> cat(sep = "\n")



rename_vars <- function(in_name) {
  name_matchup <- c(
    "REGRESSOR" = "REGRESSOR",
    "COEFFICIENT" = "COEFFICIENT",
  "econ_benefits.L1" = "D_Econ_benefits",
  "home_owner.L1" = "D_Home_owner",
  "dcpst.L2" = "Dcpst_Single",
  "dcpst.L3" = "Dcpst_PreviouslyPartnered",
  "dnc.L" = "Dnc_L1",
  # "" = "Dhe_L1",
  "drgnl.L1" = "UKC",
  "drgnl.L2" = "UKD",
  "drgnl.L4" = "UKE",
  "drgnl.L5" = "UKF",
  "drgnl.L6" = "UKG",
  "drgnl.L7" = "UKH",
  "drgnl.L9" = "UKJ",
  "drgnl.L10" = "UKK",
  "drgnl.L11" = "UKL",
  "drgnl.L12" = "UKM",
  "drgnl.L13" = "UKN",
  # "" = "Ydses_c5_Q2_L1",
  # "" = "Ydses_c5_Q3_L1",
  # "" = "Ydses_c5_Q4_L1",
  # "" = "Ydses_c5_Q5_L1",
  # "" = "Dlltsd_L1",
  # "" = "Dhm_L1",
  "sf12mcs_dv.L" = "Dwb_mcs_L1",
  "sf12pcs_dv.L" = "Dwb_pcs_L1",
  "dgn1" = "Dgn",
  "dag.L" = "Dag",
  "dag" = "Dag",
  "dag_sq.L" = "Dag_sq",
  "dag_sq" = "Dag_sq",
  "deh_c32" = "Deh_c3_Medium",
  "deh_c33" = "Deh_c3_Low",
  "intdaty_dv" = "Year_transformed",
  "(Intercept)" = "Constant"
  )

  return(name_matchup[in_name])

}

name_matchup[names(dwb_mcs1_estimates)]
name_matchup[names(dwb_pcs1_estimates)]

dwb_mcs1_vcov <- dwb_mcs1_estimates |> 
  rename_with(rename_vars) |> 
  mutate(across(REGRESSOR, rename_vars))

dwb_pcs1_vcov <- dwb_pcs1_estimates |> 
  rename_with(rename_vars) |> 
  mutate(across(REGRESSOR, rename_vars))

library(openxlsx)

wb_workbook <- createWorkbook()

addWorksheet(wb_workbook, "UK_DWB_MCS1")
addWorksheet(wb_workbook, "UK_DWB_PCS1")

writeData(wb_workbook, "UK_DWB_MCS1", dwb_mcs1_vcov)
writeData(wb_workbook, "UK_DWB_PCS1", dwb_pcs1_vcov)

saveWorkbook(wb_workbook, "outfiles/reg_wellbeing.xlsx")
