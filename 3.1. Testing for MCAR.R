
# Testing for MCAR - this is performed via chi-square tests

# Gender

# Observation period 

Analysis_DF$R_Gender <- ifelse(Analysis_DF$Gender == 1, 1, 0)
vars <- c("missing_history", "victim_history_sexual", "victim_history_physical", "Neglect_history", "Offence_history_NV", "Witness_history", "Offence_history_NV_sexual", "Offence_history_NV_Victim")

for (v in vars) {
  cat("\nChi-square test for Gender missingness vs", v, "\n")
  print(chisq.test(table(Analysis_DF$R_Gender, Analysis_DF[[v]])))
}

# Pre-14 

Analysis_DF$R_Gender <- ifelse(Analysis_DF$Gender == 1, 1, 0)
vars <- c("Neglect_history_pre_14", "victim_history_sexual_pre_14", "victim_history_physical_pre_14", "Offence_history_NV_pre_14", "missing_history_pre_14", "Witness_history_pre_14", "Offence_history_victim_pre_14", "Offence_history_sexual_pre_14")

for (v in vars) {
  cat("\nChi-square test for Gender missingness vs", v, "\n")
  print(chisq.test(table(Analysis_DF$R_Gender, Analysis_DF[[v]])))
}


# Ethnicity

# Observation period 

Analysis_DF$R_Ethnicity <- ifelse(Analysis_DF$Ethnicity == 0, 1, 0)
vars <- c("missing_history", "victim_history_sexual", "victim_history_physical", "Neglect_history", "Offence_history_NV", "Witness_history", "Offence_history_NV_sexual", "Offence_history_NV_Victim")

for (v in vars) {
  cat("\nChi-square test for Gender missingness vs", v, "\n")
  print(chisq.test(table(Analysis_DF$R_Ethnicity, Analysis_DF[[v]])))
}

# Pre-14

Analysis_DF$R_Ethnic <- ifelse(Analysis_DF$Ethnicity == 1, 1, 0)
vars <- c("Neglect_history_pre_14", "victim_history_sexual_pre_14", "victim_history_physical_pre_14", "Offence_history_NV_pre_14", "missing_history_pre_14", "Witness_history_pre_14", "Offence_history_victim_pre_14", "Offence_history_sexual_pre_14")

for (v in vars) {
  cat("\nChi-square test for Gender missingness vs", v, "\n")
  print(chisq.test(table(Analysis_DF$R_Ethnic, Analysis_DF[[v]])))
}

