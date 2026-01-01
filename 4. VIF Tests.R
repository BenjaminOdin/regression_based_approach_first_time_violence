# VIF Tests

# Option to filter dataset to include only specific gender (2 = Female and 3 = male)
#Combined_Data <- subset(Combined_Data, Gender == 3)
# Remember that if you filter the dataset to males or females, you need to remove (delete) Gender in the four models below

# Load required package and library

install.packages("car")
library(car)

model_lm1 <- lm(rep(1, nrow(Combined_Data))~ Gender + missing_history + victim_history_sexual + victim_history_physical + Neglect_history + Offence_history_NV + Witness_history + Offence_history_NV_sexual + Offence_history_NV_Victim, data = Combined_Data)  # Note: no outcome variable
vif(model_lm1)

model_lm2 <- lm(rep(1, nrow(Combined_Data))~ Gender + Missing_Frequency + Sexual_Assault_Frequency + Physical_Assault_Frequency + Neglect_Frequency + NV_Offence_Frequency + Witness_Offence_Frequency + NV_Offence_Frequency_Sexual + NV_Offence_Frequency_victim, data = Combined_Data)  # Note: no outcome variable
vif(model_lm2)

model_lm3 <- lm(rep(1, nrow(Combined_Data))~ Gender + Neglect_history_pre_14 + victim_history_sexual_pre_14 + victim_history_physical_pre_14 + Offence_history_NV_pre_14 + missing_history_pre_14 + Witness_history_pre_14 + Offence_history_victim_pre_14 + Offence_history_sexual_pre_14, data = Combined_Data)  # Note: no outcome variable
vif(model_lm3)

model_lm4 <- lm(rep(1, nrow(Combined_Data))~ Gender + Neglect_history_freq_Pre_14 + Sexual_history_freq_Pre_14 + Physical_Assault_Frequency_Pre_14 + Missing_history_freq_Pre_14 + NV_history_freq_Pre_14 + witness_freq_Pre_14 + nv_victim_history_freq_Pre_14 + sexual_history_freq_Pre_14, data = Combined_Data)  # Note: no outcome variable
vif(model_lm4)

