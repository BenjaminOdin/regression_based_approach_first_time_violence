# Regression Analysis (Cox models include Schoenfeld residual test)

# Filter dataset to include only specific gender (2 = Female and 3 = male)

# Combined_Data <- subset(Combined_Data, Gender == 2)
# When filtering for gender, remember to delete the gender parameters from the below models


# Cox PH Regression - Average Effect

cox_model_Average <- coxph(Surv(Time_All, First_Offence_1_to_5) ~ Gender + missing_history + victim_history_sexual + victim_history_physical + Neglect_history + Offence_history_NV + Witness_history + Offence_history_NV_sexual + Offence_history_NV_Victim, data = Combined_Data)

summary(cox_model_Average)

cox_zph_Average <- cox.zph(cox_model_Average)

cox_zph_Average


# Cox PH Regression - Frequency Effect

cox_model_Frequency <- coxph(Surv(Time_All, First_Offence_1_to_5) ~ Gender + Missing_Frequency + Sexual_Assault_Frequency + Physical_Assault_Frequency + Neglect_Frequency + NV_Offence_Frequency + Witness_Offence_Frequency + NV_Offence_Frequency_Sexual + NV_Offence_Frequency_victim, data = Combined_Data)

summary(cox_model_Frequency)

cox_zph_Freq <- cox.zph(cox_model_Frequency)

cox_zph_Freq


# Cox PH Regression - Average Effect Pre-14

cox_model_Pre_14 <- coxph(Surv(Time_All, first_violent_offence_pre_14) ~ Gender + Neglect_history_pre_14 + victim_history_sexual_pre_14 + victim_history_physical_pre_14 + Offence_history_NV_pre_14 + missing_history_pre_14 + Witness_history_pre_14 + Offence_history_victim_pre_14 + Offence_history_sexual_pre_14, data = Combined_Data)

summary(cox_model_Pre_14)

cox_zph_Pre_14 <- cox.zph(cox_model_Pre_14)

cox_zph_Pre_14


# Cox PH Regression - Frequency Effect Pre-14

cox_model_Pre_14_Freq <- coxph(Surv(Time_All, first_violent_offence_pre_14) ~ Gender + Neglect_history_freq_Pre_14 + Sexual_history_freq_Pre_14 + Physical_Assault_Frequency_Pre_14 + Missing_history_freq_Pre_14 + NV_history_freq_Pre_14 + witness_freq_Pre_14 + nv_victim_history_freq_Pre_14 + sexual_history_freq_Pre_14, data = Combined_Data)

summary(cox_model_Pre_14_Freq)

cox_zph_Pre_14_Freq <- cox.zph(cox_model_Pre_14_Freq)

cox_zph_Pre_14_Freq


# Cox PH Regression - Average Effect Post-14

cox_model_Average_mid <- coxph(Surv(Time_All, first_violent_offence_post_14) ~ Gender + missing_history + victim_history_sexual + victim_history_physical + Neglect_history + Offence_history_NV + Witness_history + Offence_history_NV_sexual + Offence_history_NV_Victim, data = Combined_Data)

summary(cox_model_Average_mid)

cox_zph_Average <- cox.zph(cox_model_Average_mid)

cox_zph_Average


# Cox PH Regression - Frequency Effect Post-14

cox_model_Frequency_mid <- coxph(Surv(Time_All, first_violent_offence_post_14) ~ Gender + Missing_Frequency + Sexual_Assault_Frequency + Physical_Assault_Frequency + Neglect_Frequency + NV_Offence_Frequency + Witness_Offence_Frequency + NV_Offence_Frequency_Sexual + NV_Offence_Frequency_victim, data = Combined_Data)

summary(cox_model_Frequency_mid)

cox_zph_Freq <- cox.zph(cox_model_Frequency_mid)

cox_zph_Freq




# Exponential AFT Regression - Average Effect

exponential_aft_model_Average <- survreg(Surv(Time_All, First_Offence_1_to_5) ~ Gender + missing_history + victim_history_sexual + victim_history_physical + Neglect_history + Offence_history_NV + Witness_history + Offence_history_NV_sexual + Offence_history_NV_Victim, data = Combined_Data, dist = "exponential")

summary(exponential_aft_model_Average)


# Exponential AFT Regression - Frequency Effect

exponential_aft_model_Frequency <- survreg(Surv(Time_All, First_Offence_1_to_5) ~ Gender + Missing_Frequency + Sexual_Assault_Frequency + Physical_Assault_Frequency + Neglect_Frequency + NV_Offence_Frequency + Witness_Offence_Frequency + NV_Offence_Frequency_Sexual + NV_Offence_Frequency_victim, data = Combined_Data, dist = "exponential")

summary(exponential_aft_model_Frequency)

# Exponential AFT Regression - Average Effect Pre-14

exponential_aft_model_Pre_14 <- survreg(Surv(Time_All, first_violent_offence_pre_14) ~ Gender + Neglect_history_pre_14 + victim_history_sexual_pre_14 + victim_history_physical_pre_14 + Offence_history_NV_pre_14 + missing_history_pre_14 + Witness_history_pre_14 + Offence_history_victim_pre_14 + Offence_history_sexual_pre_14, data = Combined_Data, dist = "exponential")

summary(exponential_aft_model_Pre_14)


# Exponential AFT Regression - Frequency Effect Pre-14

exponential_aft_model_Pre_14_Freq <- survreg(Surv(Time_All, first_violent_offence_pre_14) ~ Gender + Neglect_history_freq_Pre_14 + Sexual_history_freq_Pre_14 + Physical_Assault_Frequency_Pre_14 + Missing_history_freq_Pre_14 + NV_history_freq_Pre_14 + witness_freq_Pre_14 + nv_victim_history_freq_Pre_14 + sexual_history_freq_Pre_14, data = Combined_Data, dist = "exponential")

summary(exponential_aft_model_Pre_14_Freq)


# Exponential AFT Regression - Average Effect post-14

exponential_aft_model_Average_mid <- survreg(Surv(Time_All, first_violent_offence_post_14) ~ Gender + missing_history + victim_history_sexual + victim_history_physical + Neglect_history + Offence_history_NV + Witness_history + Offence_history_NV_sexual + Offence_history_NV_Victim, data = Combined_Data, dist = "exponential")

summary(exponential_aft_model_Average_mid)


# Exponential AFT Regression - Frequency Effect post-14

exponential_aft_model_Frequency_mid <- survreg(Surv(Time_All, first_violent_offence_post_14) ~ Gender + Missing_Frequency + Sexual_Assault_Frequency + Physical_Assault_Frequency + Neglect_Frequency + NV_Offence_Frequency + Witness_Offence_Frequency + NV_Offence_Frequency_Sexual + NV_Offence_Frequency_victim, data = Combined_Data, dist = "exponential")

summary(exponential_aft_model_Frequency_mid)




# Weibull AFT Regression - Average Effect

weibull_aft_model_Average <- survreg(Surv(Time_All, First_Offence_1_to_5) ~ Gender + missing_history + victim_history_sexual + victim_history_physical + Neglect_history + Offence_history_NV + Witness_history + Offence_history_NV_sexual + Offence_history_NV_Victim, data = Combined_Data, dist = "weibull")

summary(weibull_aft_model_Average)


# Weibull AFT Regression - Frequency Effect

weibull_aft_model_Frequency <- survreg(Surv(Time_All, First_Offence_1_to_5) ~ Gender + Missing_Frequency + Sexual_Assault_Frequency + Physical_Assault_Frequency + Neglect_Frequency + NV_Offence_Frequency + Witness_Offence_Frequency + NV_Offence_Frequency_Sexual + NV_Offence_Frequency_victim, data = Combined_Data, dist = "weibull")

summary(weibull_aft_model_Frequency)

# Weibull AFT Regression - Average Effect Pre-14

weibull_aft_model_Pre_14 <- survreg(Surv(Time_All, first_violent_offence_pre_14) ~ Gender + Neglect_history_pre_14 + victim_history_sexual_pre_14 + victim_history_physical_pre_14 + Offence_history_NV_pre_14 + missing_history_pre_14 + Witness_history_pre_14 + Offence_history_victim_pre_14 + Offence_history_sexual_pre_14, data = Combined_Data, dist = "weibull")

summary(weibull_aft_model_Pre_14)


# Weibull AFT Regression - Frequency Effect Pre-14

weibull_aft_model_Pre_14_Freq <- survreg(Surv(Time_All, first_violent_offence_pre_14) ~ Gender + Neglect_history_freq_Pre_14 + Sexual_history_freq_Pre_14 + Physical_Assault_Frequency_Pre_14 + Missing_history_freq_Pre_14 + NV_history_freq_Pre_14 + witness_freq_Pre_14 + nv_victim_history_freq_Pre_14 + sexual_history_freq_Pre_14, data = Combined_Data, dist = "weibull")

summary(weibull_aft_model_Pre_14_Freq)


# Weibull AFT Regression - Average Effect Post-14

weibull_aft_model_Average_mid <- survreg(Surv(Time_All, first_violent_offence_post_14) ~ Gender + missing_history + victim_history_sexual + victim_history_physical + Neglect_history + Offence_history_NV + Witness_history + Offence_history_NV_sexual + Offence_history_NV_Victim, data = Combined_Data, dist = "weibull")

summary(weibull_aft_model_Average_mid)


# Weibull AFT Regression - Frequency Effect Post-14

weibull_aft_model_Frequency_mid <- survreg(Surv(Time_All, first_violent_offence_post_14) ~ Gender + Missing_Frequency + Sexual_Assault_Frequency + Physical_Assault_Frequency + Neglect_Frequency + NV_Offence_Frequency + Witness_Offence_Frequency + NV_Offence_Frequency_Sexual + NV_Offence_Frequency_victim, data = Combined_Data, dist = "weibull")

summary(weibull_aft_model_Frequency_mid)




# Lognormal AFT Regression - Average Effect

lognormal_aft_model_Average <- survreg(Surv(Time_All, First_Offence_1_to_5) ~ Gender + missing_history + victim_history_sexual + victim_history_physical + Neglect_history + Offence_history_NV + Witness_history + Offence_history_NV_sexual + Offence_history_NV_Victim, data = Combined_Data, dist = "lognormal")

summary(lognormal_aft_model_Average)


# Lognormal AFT Regression - Frequency Effect

lognormal_aft_model_Frequency <- survreg(Surv(Time_All, First_Offence_1_to_5) ~ Gender + Missing_Frequency + Sexual_Assault_Frequency + Physical_Assault_Frequency + Neglect_Frequency + NV_Offence_Frequency + Witness_Offence_Frequency + NV_Offence_Frequency_Sexual + NV_Offence_Frequency_victim, data = Combined_Data, dist = "lognormal")

summary(lognormal_aft_model_Frequency)


# Lognormal AFT Regression - Average Effect Pre-14

lognormal_aft_model_Pre_14 <- survreg(Surv(Time_All, first_violent_offence_pre_14) ~ Gender + Neglect_history_pre_14 + victim_history_sexual_pre_14 + victim_history_physical_pre_14 + Offence_history_NV_pre_14 + missing_history_pre_14 + Witness_history_pre_14 + Offence_history_victim_pre_14 + Offence_history_sexual_pre_14, data = Combined_Data, dist = "lognormal")

summary(lognormal_aft_model_Pre_14)


# Lognormal AFT Regression - Frequency Effect Pre-14

lognormal_aft_model_Pre_14_Freq <- survreg(Surv(Time_All, first_violent_offence_pre_14) ~ Gender + Neglect_history_freq_Pre_14 + Sexual_history_freq_Pre_14 + Physical_Assault_Frequency_Pre_14 + Missing_history_freq_Pre_14 + NV_history_freq_Pre_14 + witness_freq_Pre_14 + nv_victim_history_freq_Pre_14 + sexual_history_freq_Pre_14, data = Combined_Data, dist = "lognormal")

summary(lognormal_aft_model_Pre_14_Freq)


# Lognormal AFT Regression - Average Effect Post-14

lognormal_aft_model_Average_mid <- survreg(Surv(Time_All, first_violent_offence_post_14) ~ Gender + missing_history + victim_history_sexual + victim_history_physical + Neglect_history + Offence_history_NV + Witness_history + Offence_history_NV_sexual + Offence_history_NV_Victim, data = Combined_Data, dist = "lognormal")

summary(lognormal_aft_model_Average_mid)


# Lognormal AFT Regression - Frequency Effect Post-14

lognormal_aft_model_Frequency_mid <- survreg(Surv(Time_All, first_violent_offence_post_14) ~ Gender + Missing_Frequency + Sexual_Assault_Frequency + Physical_Assault_Frequency + Neglect_Frequency + NV_Offence_Frequency + Witness_Offence_Frequency + NV_Offence_Frequency_Sexual + NV_Offence_Frequency_victim, data = Combined_Data, dist = "lognormal")

summary(lognormal_aft_model_Frequency_mid)




# Loglogistic AFT Regression - Average Effect

loglogistic_aft_model_Average <- survreg(Surv(Time_All, First_Offence_1_to_5) ~ Gender + missing_history + victim_history_sexual + victim_history_physical + Neglect_history + Offence_history_NV + Witness_history + Offence_history_NV_sexual + Offence_history_NV_Victim, data = Combined_Data, dist = "loglogistic")

summary(loglogistic_aft_model_Average)


# Loglogistic AFT Regression - Frequency Effect

loglogistic_aft_model_Frequency <- survreg(Surv(Time_All, First_Offence_1_to_5) ~ Gender + Missing_Frequency + Sexual_Assault_Frequency + Physical_Assault_Frequency + Neglect_Frequency + NV_Offence_Frequency + Witness_Offence_Frequency + NV_Offence_Frequency_Sexual + NV_Offence_Frequency_victim, data = Combined_Data, dist = "loglogistic")

summary(loglogistic_aft_model_Frequency)

# Loglogistic AFT Regression - Average Effect Pre-14

loglogistic_aft_model_Pre_14 <- survreg(Surv(Time_All, first_violent_offence_pre_14) ~ Gender + Neglect_history_pre_14 + victim_history_sexual_pre_14 + victim_history_physical_pre_14 + Offence_history_NV_pre_14 + missing_history_pre_14 + Witness_history_pre_14 + Offence_history_victim_pre_14 + Offence_history_sexual_pre_14, data = Combined_Data, dist = "loglogistic")

summary(loglogistic_aft_model_Pre_14)


# Loglogistic AFT Regression - Frequency Effect Pre-14

loglogistic_aft_model_Pre_14_Freq <- survreg(Surv(Time_All, first_violent_offence_pre_14) ~ Gender + Neglect_history_freq_Pre_14 + Sexual_history_freq_Pre_14 + Physical_Assault_Frequency_Pre_14 + Missing_history_freq_Pre_14 + NV_history_freq_Pre_14 + witness_freq_Pre_14 + nv_victim_history_freq_Pre_14 + sexual_history_freq_Pre_14, data = Combined_Data, dist = "loglogistic")

summary(loglogistic_aft_model_Pre_14_Freq)

# Loglogistic AFT Regression - Average Effect Post-14

loglogistic_aft_model_Average_mid <- survreg(Surv(Time_All, first_violent_offence_post_14) ~ Gender + missing_history + victim_history_sexual + victim_history_physical + Neglect_history + Offence_history_NV + Witness_history + Offence_history_NV_sexual + Offence_history_NV_Victim, data = Combined_Data, dist = "loglogistic")

summary(loglogistic_aft_model_Average_mid)


# Loglogistic AFT Regression - Frequency Effect Post-14

loglogistic_aft_model_Frequency_mid <- survreg(Surv(Time_All, first_violent_offence_post_14) ~ Gender + Missing_Frequency + Sexual_Assault_Frequency + Physical_Assault_Frequency + Neglect_Frequency + NV_Offence_Frequency + Witness_Offence_Frequency + NV_Offence_Frequency_Sexual + NV_Offence_Frequency_victim, data = Combined_Data, dist = "loglogistic")

summary(loglogistic_aft_model_Frequency_mid)




# Gamma AFT Regression - Average Effect

gamma_aft_model_Average <- flexsurvreg(Surv(Time_All, First_Offence_1_to_5) ~ Gender + missing_history + victim_history_sexual + victim_history_physical + Neglect_history + Offence_history_NV + Witness_history + Offence_history_NV_sexual + Offence_history_NV_Victim, data = Combined_Data, dist = "gengamma")

summary(gamma_aft_model_Average)
AIC(gamma_aft_model_Average)

# Gamma AFT Regression - Frequency Effect

gamma_aft_model_Frequency <- flexsurvreg(Surv(Time_All, First_Offence_1_to_5) ~ Gender + Missing_Frequency + Sexual_Assault_Frequency + Physical_Assault_Frequency + Neglect_Frequency + NV_Offence_Frequency + Witness_Offence_Frequency + NV_Offence_Frequency_Sexual + NV_Offence_Frequency_victim, data = Combined_Data, dist = "gengamma")

summary(gamma_aft_model_Frequency)
AIC(gamma_aft_model_Frequency)

# Gamma AFT Regression - Average Effect Pre-14

gamma_aft_model_Pre_14 <- flexsurvreg(Surv(Time_All, first_violent_offence_pre_14) ~ Gender + Neglect_history_pre_14 + victim_history_sexual_pre_14 + victim_history_physical_pre_14 + Offence_history_NV_pre_14 + missing_history_pre_14 + Witness_history_pre_14 + Offence_history_victim_pre_14 + Offence_history_sexual_pre_14, data = Combined_Data, dist = "gengamma")

summary(gamma_aft_model_Pre_14)
AIC(gamma_aft_model_Pre_14)


# Gamma AFT Regression - Frequency Effect Pre-14

gamma_aft_model_Pre_14_Freq <- flexsurvreg(Surv(Time_All, first_violent_offence_pre_14) ~ Gender + Neglect_history_freq_Pre_14 + Sexual_history_freq_Pre_14 + Physical_Assault_Frequency_Pre_14 + Missing_history_freq_Pre_14 + NV_history_freq_Pre_14 + witness_freq_Pre_14 + nv_victim_history_freq_Pre_14 + sexual_history_freq_Pre_14, data = Combined_Data, dist = "gengamma")

summary(gamma_aft_model_Pre_14_Freq)
AIC(gamma_aft_model_Pre_14_Freq)

# Gamma AFT Regression - Average Effect Post-14

gamma_aft_model_Average_mid <- flexsurvreg(Surv(Time_All, first_violent_offence_post_14) ~ Gender + missing_history + victim_history_sexual + victim_history_physical + Neglect_history + Offence_history_NV + Witness_history + Offence_history_NV_sexual + Offence_history_NV_Victim, data = Combined_Data, dist = "gengamma")

summary(gamma_aft_model_Average_mid)
AIC(gamma_aft_model_Average_mid)


# Gamma AFT Regression - Frequency Effect Post-14

gamma_aft_model_Frequency_mid <- flexsurvreg(Surv(Time_All, first_violent_offence_post_14) ~ Gender + Missing_Frequency + Sexual_Assault_Frequency + Physical_Assault_Frequency + Neglect_Frequency + NV_Offence_Frequency + Witness_Offence_Frequency + NV_Offence_Frequency_Sexual + NV_Offence_Frequency_victim, data = Combined_Data, dist = "gengamma")

summary(gamma_aft_model_Frequency_mid)
AIC(gamma_aft_model_Frequency_mid)



# Gompertz AFT Regression - Average Effect

Gompertz_aft_model_Average <- flexsurvreg(Surv(Time_All, First_Offence_1_to_5) ~ Gender + missing_history + victim_history_sexual + victim_history_physical + Neglect_history + Offence_history_NV + Witness_history + Offence_history_NV_sexual + Offence_history_NV_Victim, data = Combined_Data, dist = "gompertz")

summary(Gompertz_aft_model_Average)
AIC(Gompertz_aft_model_Average)

# Gompertz AFT Regression - Frequency Effect

Gompertz_aft_model_Frequency <- flexsurvreg(Surv(Time_All, First_Offence_1_to_5) ~ Gender + Missing_Frequency + Sexual_Assault_Frequency + Physical_Assault_Frequency + Neglect_Frequency + NV_Offence_Frequency + Witness_Offence_Frequency + NV_Offence_Frequency_Sexual + NV_Offence_Frequency_victim, data = Combined_Data, dist = "gompertz")

summary(Gompertz_aft_model_Frequency)
AIC(Gompertz_aft_model_Frequency)

# Gompertz AFT Regression - Average Effect Pre-14

Gompertz_aft_model_Pre_14 <- flexsurvreg(Surv(Time_All, first_violent_offence_pre_14) ~ Gender + Neglect_history_pre_14 + victim_history_sexual_pre_14 + victim_history_physical_pre_14 + Offence_history_NV_pre_14 + missing_history_pre_14 + Witness_history_pre_14 + Offence_history_victim_pre_14 + Offence_history_sexual_pre_14, data = Combined_Data, dist = "gompertz")

summary(Gompertz_aft_model_Pre_14)
AIC(Gompertz_aft_model_Pre_14)


# Gompertz AFT Regression - Frequency Effect Pre-14

Gompertz_aft_model_Pre_14_Freq <- flexsurvreg(Surv(Time_All, first_violent_offence_pre_14) ~ Gender + Neglect_history_freq_Pre_14 + Sexual_history_freq_Pre_14 + Physical_Assault_Frequency_Pre_14 + Missing_history_freq_Pre_14 + NV_history_freq_Pre_14 + witness_freq_Pre_14 + nv_victim_history_freq_Pre_14 + sexual_history_freq_Pre_14, data = Combined_Data, dist = "gompertz")

summary(Gompertz_aft_model_Pre_14_Freq)
AIC(Gompertz_aft_model_Pre_14_Freq)

# Gompertz AFT Regression - Average Effect Post-14

Gompertz_aft_model_Average_mid <- flexsurvreg(Surv(Time_All, first_violent_offence_post_14) ~ Gender + missing_history + victim_history_sexual + victim_history_physical + Neglect_history + Offence_history_NV + Witness_history + Offence_history_NV_sexual + Offence_history_NV_Victim, data = Combined_Data, dist = "gompertz")

summary(Gompertz_aft_model_Average_mid)
AIC(Gompertz_aft_model_Average_mid)


# Gompertz AFT Regression - Frequency Effect Post-14

Gompertz_aft_model_Frequency_mid <- flexsurvreg(Surv(Time_All, first_violent_offence_post_14) ~ Gender + Missing_Frequency + Sexual_Assault_Frequency + Physical_Assault_Frequency + Neglect_Frequency + NV_Offence_Frequency + Witness_Offence_Frequency + NV_Offence_Frequency_Sexual + NV_Offence_Frequency_victim, data = Combined_Data, dist = "gompertz")

summary(Gompertz_aft_model_Frequency_mid)
AIC(Gompertz_aft_model_Frequency_mid)



# Check AICs of TTE Models (Gamma and Gompertz already checked)

# AIC Checks- Average Effect

AIC_summary <- c(AIC(cox_model_Average), AIC(exponential_aft_model_Average), AIC(weibull_aft_model_Average), AIC(lognormal_aft_model_Average), AIC(loglogistic_aft_model_Average))

print(AIC_summary)

# AIC Checks- Frequency Effect

AIC_summary <- c(AIC(cox_model_Frequency), AIC(exponential_aft_model_Frequency), AIC(weibull_aft_model_Frequency), AIC(lognormal_aft_model_Frequency), AIC(loglogistic_aft_model_Frequency))

print(AIC_summary)

# AIC Checks- Age Effect - Exposure Pre-14

AIC_summary <- c(AIC(cox_model_Pre_14), AIC(exponential_aft_model_Pre_14), AIC(weibull_aft_model_Pre_14), AIC(lognormal_aft_model_Pre_14), AIC(loglogistic_aft_model_Pre_14))

print(AIC_summary)

# AIC Checks- Age Effect - Frequency - Exposure Pre-14

AIC_summary <- c(AIC(cox_model_Pre_14_Freq), AIC(exponential_aft_model_Pre_14_Freq), AIC(weibull_aft_model_Pre_14_Freq), AIC(lognormal_aft_model_Pre_14_Freq), AIC(loglogistic_aft_model_Pre_14_Freq))

print(AIC_summary)

# AIC Checks- Average Effect

AIC_summary <- c(AIC(cox_model_Average_mid), AIC(exponential_aft_model_Average_mid), AIC(weibull_aft_model_Average_mid), AIC(lognormal_aft_model_Average_mid), AIC(loglogistic_aft_model_Average_mid))

print(AIC_summary)

# AIC Checks- Frequency Effect

AIC_summary <- c(AIC(cox_model_Frequency_mid), AIC(exponential_aft_model_Frequency_mid), AIC(weibull_aft_model_Frequency_mid), AIC(lognormal_aft_model_Frequency_mid), AIC(loglogistic_aft_model_Frequency_mid))

print(AIC_summary)



# Run Logistic Regressions with Pseudo R2 and AUC (remember that AIC is automatically computed in the regression outputs)


# Logistic Regression - Average Effect

logit_model_Average <- glm(First_Offence_1_to_5 ~ Gender + missing_history + victim_history_sexual + victim_history_physical + Offence_history_NV_Victim + Neglect_history + Offence_history_NV + Witness_history + Offence_history_NV_sexual, data = Combined_Data, family = binomial(link = "logit"))

summary(logit_model_Average)

logLik_fitted <- logLik(logit_model_Average)

null_model <- glm(First_Offence_1_to_5 ~ 1, data = Combined_Data, family = "binomial")

logLik_null <- logLik(null_model)

mcfadden_r2 <- 1 - (as.numeric(logLik_fitted) / as.numeric(logLik_null))

print(paste("McFadden's R-squared:", round(mcfadden_r2, 4)))


# Logistic Regression - Frequency Effect

logit_model_Frequency <- glm(First_Offence_1_to_5 ~ Gender +  Missing_Frequency + Sexual_Assault_Frequency + Physical_Assault_Frequency + NV_Offence_Frequency_victim + Neglect_Frequency + NV_Offence_Frequency + Witness_Offence_Frequency + NV_Offence_Frequency_Sexual, data = Combined_Data, family = binomial(link = "logit"))

summary(logit_model_Frequency)

logLik_fitted <- logLik(logit_model_Frequency)

null_model <- glm(First_Offence_1_to_5 ~ 1, data = Combined_Data, family = "binomial")

logLik_null <- logLik(null_model)

mcfadden_r2 <- 1 - (as.numeric(logLik_fitted) / as.numeric(logLik_null))

print(paste("McFadden's R-squared:", round(mcfadden_r2, 4)))


# Logistic Regression - Average Effect Pre-14

logit_model_Pre_14 <- glm(first_violent_offence_pre_14 ~ Gender +  missing_history_pre_14 + victim_history_sexual_pre_14 + victim_history_physical_pre_14 + Offence_history_victim_pre_14 + Neglect_history_pre_14 + Offence_history_NV_pre_14 + Witness_history_pre_14 + Offence_history_sexual_pre_14, data = Combined_Data, family = binomial(link = "logit"))

summary(logit_model_Pre_14)

logLik_fitted <- logLik(logit_model_Pre_14)

null_model <- glm(first_violent_offence_pre_14 ~ 1, data = Combined_Data, family = "binomial")

logLik_null <- logLik(null_model)

mcfadden_r2 <- 1 - (as.numeric(logLik_fitted) / as.numeric(logLik_null))

print(paste("McFadden's R-squared:", round(mcfadden_r2, 4)))



# Logistic Regression - Frequency Effect Pre-14

logit_model_Pre_14_Freq <- glm(first_violent_offence_pre_14 ~ Gender +  Missing_history_freq_Pre_14 + Sexual_history_freq_Pre_14 + Physical_Assault_Frequency_Pre_14 + nv_victim_history_freq_Pre_14 + Neglect_history_freq_Pre_14 + NV_history_freq_Pre_14 + witness_freq_Pre_14 + sexual_history_freq_Pre_14, data = Combined_Data, family = binomial(link = "logit"))

summary(logit_model_Pre_14_Freq)

logLik_fitted <- logLik(logit_model_Pre_14_Freq)

null_model <- glm(first_violent_offence_pre_14 ~ 1, data = Combined_Data, family = "binomial")

logLik_null <- logLik(null_model)

mcfadden_r2 <- 1 - (as.numeric(logLik_fitted) / as.numeric(logLik_null))

print(paste("McFadden's R-squared:", round(mcfadden_r2, 4)))


# Logistic Regression - Average Effect Post 14

logit_model_Average_Post <- glm(first_violent_offence_post_14 ~ Gender +  missing_history + victim_history_sexual + victim_history_physical + Offence_history_NV_Victim + Neglect_history + Offence_history_NV + Witness_history + Offence_history_NV_sexual, data = Combined_Data, family = binomial(link = "logit"))

summary(logit_model_Average_Post)

logLik_fitted <- logLik(logit_model_Average_Post)

null_model <- glm(First_Offence_1_to_5 ~ 1, data = Combined_Data, family = "binomial")

logLik_null <- logLik(null_model)

mcfadden_r2 <- 1 - (as.numeric(logLik_fitted) / as.numeric(logLik_null))

print(paste("McFadden's R-squared:", round(mcfadden_r2, 4)))


# Logistic Regression - Frequency Effect Post 14

logit_model_Frequency_Post <- glm(first_violent_offence_post_14 ~ Gender +  Missing_Frequency + Sexual_Assault_Frequency + Physical_Assault_Frequency + NV_Offence_Frequency_victim + Neglect_Frequency + NV_Offence_Frequency + Witness_Offence_Frequency + NV_Offence_Frequency_Sexual, data = Combined_Data, family = binomial(link = "logit"))

summary(logit_model_Frequency_Post)

logLik_fitted <- logLik(logit_model_Frequency_Post)

null_model <- glm(First_Offence_1_to_5 ~ 1, data = Combined_Data, family = "binomial")

logLik_null <- logLik(null_model)

mcfadden_r2 <- 1 - (as.numeric(logLik_fitted) / as.numeric(logLik_null))

print(paste("McFadden's R-squared:", round(mcfadden_r2, 4)))



# BIC Code for appendices

# BIC Average Effect

BIC_summary <- c(
  BIC(cox_model_Average),
  BIC(exponential_aft_model_Average),
  BIC(weibull_aft_model_Average),
  BIC(lognormal_aft_model_Average),
  BIC(loglogistic_aft_model_Average), 
  BIC (gamma_aft_model_Average),
  BIC( Gompertz_aft_model_Average),
  BIC(logit_model_Average)
)

print(BIC_summary)

# BIC Frequency Effect

BIC_summary <- c(
  BIC(cox_model_Frequency),
  BIC(exponential_aft_model_Frequency),
  BIC(weibull_aft_model_Frequency),
  BIC(lognormal_aft_model_Frequency),
  BIC(loglogistic_aft_model_Frequency), 
  BIC (gamma_aft_model_Frequency),
  BIC( Gompertz_aft_model_Frequency),
  BIC(logit_model_Frequency)
)

print(BIC_summary)

# BIC Average Effect pre-14

BIC_summary <- c(
  BIC(cox_model_Pre_14),
  BIC(exponential_aft_model_Pre_14),
  BIC(weibull_aft_model_Pre_14),
  BIC(lognormal_aft_model_Pre_14),
  BIC(loglogistic_aft_model_Pre_14), 
  BIC (gamma_aft_model_Pre_14),
  BIC( Gompertz_aft_model_Pre_14),
  BIC(logit_model_Pre_14)
)

print(BIC_summary)

# BIC Frequency Effect pre-14

BIC_summary <- c(
  BIC(cox_model_Pre_14_Freq),
  BIC(exponential_aft_model_Pre_14_Freq),
  BIC(weibull_aft_model_Pre_14_Freq),
  BIC(lognormal_aft_model_Pre_14_Freq),
  BIC(loglogistic_aft_model_Pre_14_Freq), 
  BIC (gamma_aft_model_Pre_14_Freq),
  BIC( Gompertz_aft_model_Pre_14_Freq),
  BIC(logit_model_Pre_14_Freq)
)

print(BIC_summary)

# BIC Average Effect post-14

BIC_summary <- c(
  BIC(cox_model_Average_mid),
  BIC(exponential_aft_model_Average_mid),
  BIC(weibull_aft_model_Average_mid),
  BIC(lognormal_aft_model_Average_mid),
  BIC(loglogistic_aft_model_Average_mid), 
  BIC (gamma_aft_model_Average_mid),
  BIC( Gompertz_aft_model_Average_mid),
  BIC(logit_model_Average_Post)
)

print(BIC_summary)

# BIC Frequency Effect post-14

BIC_summary <- c(
  BIC(cox_model_Frequency_mid),
  BIC(exponential_aft_model_Frequency_mid),
  BIC(weibull_aft_model_Frequency_mid),
  BIC(lognormal_aft_model_Frequency_mid),
  BIC(loglogistic_aft_model_Frequency_mid), 
  BIC (gamma_aft_model_Frequency_mid),
  BIC( Gompertz_aft_model_Frequency_mid),
  BIC(logit_model_Frequency_Post)
)

print(BIC_summary)



# AUC Average Effect

png("ROC_Curve_with_AUC.png", width = 800, height = 600)


if ("Times New Roman" %in% windowsFonts()) {
  par(mar = c(5, 5, 4, 2) + 0.1, family = "Times New Roman") 
} else {
  par(mar = c(5, 5, 4, 2) + 0.1, family = "serif")  
}


pred_scores <- predict(logit_model_Average, type = "response")


true_outcomes <- Combined_Data$First_Offence_1_to_5


pred <- prediction(pred_scores, true_outcomes)


perf <- performance(pred, "tpr", "fpr")


auc <- performance(pred, "auc")
auc_value <- unlist(slot(auc, "y.values"))


print(paste("AUC:", auc_value))


# AUC Frequency Effect

png("ROC_Curve_with_AUC.png", width = 800, height = 600)


par(mar = c(5, 5, 4, 2) + 0.1, family = "serif")  


pred_scores <- predict(logit_model_Frequency, type = "response")


true_outcomes <- Combined_Data$First_Offence_1_to_5


pred <- prediction(pred_scores, true_outcomes)


perf <- performance(pred, "tpr", "fpr")


auc <- performance(pred, "auc")
auc_value <- unlist(slot(auc, "y.values"))


print(paste("AUC:", auc_value))



# AUC Average Effect Pre-14

png("ROC_Curve_with_AUC.png", width = 800, height = 600)


par(mar = c(5, 5, 4, 2) + 0.1, family = "serif")  


pred_scores <- predict(logit_model_Pre_14, type = "response")


true_outcomes <- Combined_Data$first_violent_offence_pre_14


pred <- prediction(pred_scores, true_outcomes)


perf <- performance(pred, "tpr", "fpr")


auc <- performance(pred, "auc")
auc_value <- unlist(slot(auc, "y.values"))


print(paste("AUC:", auc_value))


# AUC Frequency Effect Pre-14

png("ROC_Curve_with_AUC.png", width = 800, height = 600)


par(mar = c(5, 5, 4, 2) + 0.1, family = "serif")  


pred_scores <- predict(logit_model_Pre_14_Freq, type = "response")


true_outcomes <- Combined_Data$first_violent_offence_pre_14


pred <- prediction(pred_scores, true_outcomes)


perf <- performance(pred, "tpr", "fpr")


auc <- performance(pred, "auc")
auc_value <- unlist(slot(auc, "y.values"))


print(paste("AUC:", auc_value))


# AUC Average Effect Exposure Post-14

png("ROC_Curve_with_AUC.png", width = 800, height = 600)


par(mar = c(5, 5, 4, 2) + 0.1, family = "serif")  


pred_scores <- predict(logit_model_Average_Post, type = "response")


true_outcomes <- Combined_Data$first_violent_offence_post_14


pred <- prediction(pred_scores, true_outcomes)


perf <- performance(pred, "tpr", "fpr")


auc <- performance(pred, "auc")
auc_value <- unlist(slot(auc, "y.values"))


print(paste("AUC:", auc_value))



# AUC Frequency Effect Post-14

png("ROC_Curve_with_AUC.png", width = 800, height = 600)


par(mar = c(5, 5, 4, 2) + 0.1, family = "serif")  


pred_scores <- predict(logit_model_Frequency_Post, type = "response")


true_outcomes <- Combined_Data$first_violent_offence_post_14


pred <- prediction(pred_scores, true_outcomes)


perf <- performance(pred, "tpr", "fpr")


auc <- performance(pred, "auc")
auc_value <- unlist(slot(auc, "y.values"))


print(paste("AUC:", auc_value))



# Generate Confidence Intervals for Logistic Regression Models


# Average Effect


logit_model_Average <- glm(First_Offence_1_to_5 ~ Gender + missing_history + victim_history_sexual + victim_history_physical + 
                             Offence_history_NV_Victim + Neglect_history + Offence_history_NV + Witness_history + Offence_history_NV_sexual, 
                           data = Combined_Data, family = binomial(link = "logit"))


get_coef <- function(data, indices) {
  d <- data[indices, ]  
  model_boot_Average <- glm(First_Offence_1_to_5 ~ Gender + missing_history + victim_history_sexual + victim_history_physical + 
                              Offence_history_NV_Victim + Neglect_history + Offence_history_NV + Witness_history + Offence_history_NV_sexual, 
                            data = d, family = binomial(link = "logit"))  
  return(coef(model_boot_Average)) 
}


set.seed(123)


boot_results_Average <- boot(data = Combined_Data, statistic = get_coef, R = 1000)


print(boot_results_Average)


ci_list <- list()


for (i in 1:length(coef(logit_model_Average))) {
  ci <- boot.ci(boot_results_Average, type = "perc", index = i)  
  ci_list[[names(coef(logit_model_Average))[i]]] <- ci
}


print(ci_list)


ci_table <- data.frame(
  Parameter = names(coef(logit_model_Average)),  
  Lower_CI = sapply(ci_list, function(x) x$percent[4]),  
  Upper_CI = sapply(ci_list, function(x) x$percent[5])   
)


print(ci_table)


# Frequency Effect


logit_model_Frequency <- glm(First_Offence_1_to_5 ~ Gender + Missing_Frequency + Sexual_Assault_Frequency + 
                               Physical_Assault_Frequency + NV_Offence_Frequency_victim + Neglect_Frequency + NV_Offence_Frequency + 
                               Witness_Offence_Frequency + NV_Offence_Frequency_Sexual, 
                             data = Combined_Data, family = binomial(link = "logit"))


get_coef <- function(data, indices) {
  d <- data[indices, ]  
  model_boot_Average <- glm(First_Offence_1_to_5 ~ Gender + Missing_Frequency + Sexual_Assault_Frequency + 
                              Physical_Assault_Frequency + NV_Offence_Frequency_victim + Neglect_Frequency + NV_Offence_Frequency + 
                              Witness_Offence_Frequency + NV_Offence_Frequency_Sexual, 
                            data = d, family = binomial(link = "logit"))  
  return(coef(model_boot_Average))  
}


set.seed(123)


boot_results_Average <- boot(data = Combined_Data, statistic = get_coef, R = 1000)


print(boot_results_Average)


ci_list <- list()


for (i in 1:length(coef(logit_model_Frequency))) {
  ci <- boot.ci(boot_results_Average, type = "perc", index = i)  
  ci_list[[names(coef(logit_model_Frequency))[i]]] <- ci
}


print(ci_list)


ci_table <- data.frame(
  Parameter = names(coef(logit_model_Frequency)),  
  Lower_CI = sapply(ci_list, function(x) x$percent[4]),  
  Upper_CI = sapply(ci_list, function(x) x$percent[5])   
)


print(ci_table)




# Average Effect Pre-14


logit_model_Pre_14 <- glm(first_violent_offence_pre_14 ~ Gender + missing_history_pre_14 + victim_history_sexual_pre_14 + victim_history_physical_pre_14 + Offence_history_victim_pre_14 + Neglect_history_pre_14 + Offence_history_NV_pre_14 + Witness_history_pre_14 + Offence_history_sexual_pre_14, data = Combined_Data, family = binomial(link = "logit"))

summary(logit_model_Pre_14)


get_coef <- function(data, indices) {
  d <- data[indices, ]  
  model_boot_Average <- glm(first_violent_offence_pre_14 ~ Gender + missing_history_pre_14 + victim_history_sexual_pre_14 + victim_history_physical_pre_14 + Offence_history_victim_pre_14 + Neglect_history_pre_14 + Offence_history_NV_pre_14 + Witness_history_pre_14 + Offence_history_sexual_pre_14, data = d, family = binomial(link = "logit"))
  return(coef(model_boot_Average))  
}


set.seed(123)


boot_results_Average <- boot(data = Combined_Data, statistic = get_coef, R = 1000)


print(boot_results_Average)


ci_list <- list()


for (i in 1:length(coef(logit_model_Pre_14))) {
  ci <- boot.ci(boot_results_Average, type = "perc", index = i)  
  ci_list[[names(coef(logit_model_Pre_14))[i]]] <- ci
}


print(ci_list)


ci_table <- data.frame(
  Parameter = names(coef(logit_model_Pre_14)),  
  Lower_CI = sapply(ci_list, function(x) x$percent[4]),  
  Upper_CI = sapply(ci_list, function(x) x$percent[5])   
)


print(ci_table)




# Frequency Effect Pre-14


logit_model_Pre_14_Frequency <- glm(first_violent_offence_pre_14 ~ Gender + Missing_history_freq_Pre_14 + Sexual_history_freq_Pre_14 + Physical_Assault_Frequency_Pre_14 + nv_victim_history_freq_Pre_14 + Neglect_history_freq_Pre_14 + NV_history_freq_Pre_14 + witness_freq_Pre_14 + sexual_history_freq_Pre_14, data = Combined_Data, family = binomial(link = "logit"))
summary(logit_model_Pre_14_Frequency)


get_coef <- function(data, indices) {
  d <- data[indices, ]  
  model_boot_Average <- glm(first_violent_offence_pre_14 ~ Gender + Missing_history_freq_Pre_14 + Sexual_history_freq_Pre_14 + Physical_Assault_Frequency_Pre_14 + nv_victim_history_freq_Pre_14 + Neglect_history_freq_Pre_14 + NV_history_freq_Pre_14 + witness_freq_Pre_14 + sexual_history_freq_Pre_14, data = d, family = binomial(link = "logit"))
  return(coef(model_boot_Average))  
}


set.seed(123)


boot_results_Average <- boot(data = Combined_Data, statistic = get_coef, R = 1000)


print(boot_results_Average)


ci_list <- list()


for (i in 1:length(coef(logit_model_Pre_14_Frequency))) {
  ci <- boot.ci(boot_results_Average, type = "perc", index = i)  
  ci_list[[names(coef(logit_model_Pre_14_Frequency))[i]]] <- ci
}


print(ci_list)


ci_table <- data.frame(
  Parameter = names(coef(logit_model_Pre_14_Frequency)),  
  Lower_CI = sapply(ci_list, function(x) x$percent[4]),  
  Upper_CI = sapply(ci_list, function(x) x$percent[5])   
)


print(ci_table)



# Average Effect Post-14


logit_model_Average <- glm(first_violent_offence_post_14 ~ Gender + missing_history + victim_history_sexual + victim_history_physical + 
                             Offence_history_NV_Victim + Neglect_history + Offence_history_NV + Witness_history + Offence_history_NV_sexual, 
                           data = Combined_Data, family = binomial(link = "logit"))


get_coef <- function(data, indices) {
  d <- data[indices, ]  
  model_boot_Average <- glm(first_violent_offence_post_14 ~ Gender + missing_history + victim_history_sexual + victim_history_physical + 
                              Offence_history_NV_Victim + Neglect_history + Offence_history_NV + Witness_history + Offence_history_NV_sexual, 
                            data = d, family = binomial(link = "logit"))  
  return(coef(model_boot_Average)) 
}


set.seed(123)


boot_results_Average <- boot(data = Combined_Data, statistic = get_coef, R = 1000)


print(boot_results_Average)


ci_list <- list()


for (i in 1:length(coef(logit_model_Average))) {
  ci <- boot.ci(boot_results_Average, type = "perc", index = i)  
  ci_list[[names(coef(logit_model_Average))[i]]] <- ci
}


print(ci_list)


ci_table <- data.frame(
  Parameter = names(coef(logit_model_Average)),  
  Lower_CI = sapply(ci_list, function(x) x$percent[4]),  
  Upper_CI = sapply(ci_list, function(x) x$percent[5])   
)


print(ci_table)


# Frequency Effect Post-14


logit_model_Frequency <- glm(first_violent_offence_post_14 ~ Gender + Missing_Frequency + Sexual_Assault_Frequency + 
                               Physical_Assault_Frequency + NV_Offence_Frequency_victim + Neglect_Frequency + NV_Offence_Frequency + 
                               Witness_Offence_Frequency + NV_Offence_Frequency_Sexual, 
                             data = Combined_Data, family = binomial(link = "logit"))


get_coef <- function(data, indices) {
  d <- data[indices, ]  
  model_boot_Average <- glm(first_violent_offence_post_14 ~ Gender + Missing_Frequency + Sexual_Assault_Frequency + 
                              Physical_Assault_Frequency + NV_Offence_Frequency_victim + Neglect_Frequency + NV_Offence_Frequency + 
                              Witness_Offence_Frequency + NV_Offence_Frequency_Sexual, 
                            data = d, family = binomial(link = "logit"))  
  return(coef(model_boot_Average))  
}


set.seed(123)


boot_results_Average <- boot(data = Combined_Data, statistic = get_coef, R = 1000)


print(boot_results_Average)


ci_list <- list()


for (i in 1:length(coef(logit_model_Frequency))) {
  ci <- boot.ci(boot_results_Average, type = "perc", index = i)  
  ci_list[[names(coef(logit_model_Frequency))[i]]] <- ci
}


print(ci_list)


ci_table <- data.frame(
  Parameter = names(coef(logit_model_Frequency)),  
  Lower_CI = sapply(ci_list, function(x) x$percent[4]),  
  Upper_CI = sapply(ci_list, function(x) x$percent[5])   
)


print(ci_table)



# Gender-Exposure Interaction Effects 

# Average Effect

logit_model_Average <- glm(First_Offence_1_to_5 ~ ( Gender + missing_history + victim_history_sexual + victim_history_physical + Neglect_history + Offence_history_NV + Witness_history + Offence_history_NV_sexual + Offence_history_NV_Victim) * Gender, data = Combined_Data, family = binomial(link = "logit"))

summary(logit_model_Average)

logLik_fitted <- logLik(logit_model_Average)

null_model <- glm(First_Offence_1_to_5 ~ 1, data = Combined_Data, family = "binomial")

logLik_null <- logLik(null_model)

mcfadden_r2 <- 1 - (as.numeric(logLik_fitted) / as.numeric(logLik_null))

print(paste("McFadden's R-squared:", round(mcfadden_r2, 4)))


# Frequency Effect

logit_model_Frequency <- glm(First_Offence_1_to_5 ~ (Gender + Missing_Frequency + Sexual_Assault_Frequency + Physical_Assault_Frequency + Neglect_Frequency + NV_Offence_Frequency + Witness_Offence_Frequency + NV_Offence_Frequency_Sexual + NV_Offence_Frequency_victim) * Gender, data = Combined_Data, family = binomial(link = "logit"))

summary(logit_model_Frequency)

logLik_fitted <- logLik(logit_model_Frequency)

null_model <- glm(First_Offence_1_to_5 ~ 1, data = Combined_Data, family = "binomial")

logLik_null <- logLik(null_model)

mcfadden_r2 <- 1 - (as.numeric(logLik_fitted) / as.numeric(logLik_null))

print(paste("McFadden's R-squared:", round(mcfadden_r2, 4)))


# Average Effect Pre-14

logit_model_Pre_14 <- glm(first_violent_offence_pre_14 ~ ( Gender + missing_history_pre_14 + victim_history_sexual_pre_14 + victim_history_physical_pre_14 + Neglect_history_pre_14 + Offence_history_NV_pre_14 + Witness_history_pre_14 + Offence_history_victim_pre_14 + Offence_history_sexual_pre_14) * Gender, data = Combined_Data, family = binomial(link = "logit"))

summary(logit_model_Pre_14)

logLik_fitted <- logLik(logit_model_Pre_14)

null_model <- glm(first_violent_offence_pre_14 ~ 1, data = Combined_Data, family = "binomial")

logLik_null <- logLik(null_model)

mcfadden_r2 <- 1 - (as.numeric(logLik_fitted) / as.numeric(logLik_null))

print(paste("McFadden's R-squared:", round(mcfadden_r2, 4)))


# Frequency Effect Pre-14

logit_model_Pre_14_Freq <- glm(first_violent_offence_pre_14 ~ ( Gender + Missing_history_freq_Pre_14 + Sexual_history_freq_Pre_14 + Physical_Assault_Frequency_Pre_14 + Neglect_history_freq_Pre_14 + NV_history_freq_Pre_14 + witness_freq_Pre_14 + nv_victim_history_freq_Pre_14 + sexual_history_freq_Pre_14) * Gender, data = Combined_Data, family = binomial(link = "logit"))

summary(logit_model_Pre_14_Freq)

logLik_fitted <- logLik(logit_model_Pre_14_Freq)

null_model <- glm(first_violent_offence_pre_14 ~ 1, data = Combined_Data, family = "binomial")

logLik_null <- logLik(null_model)

mcfadden_r2 <- 1 - (as.numeric(logLik_fitted) / as.numeric(logLik_null))

print(paste("McFadden's R-squared:", round(mcfadden_r2, 4)))



# Average Effect Post-14

logit_model_Average_Post <- glm(first_violent_offence_post_14 ~ ( Gender + missing_history + victim_history_sexual + victim_history_physical + Neglect_history + Offence_history_NV + Witness_history + Offence_history_NV_sexual + Offence_history_NV_Victim) * Gender, data = Combined_Data, family = binomial(link = "logit"))

summary(logit_model_Average)

logLik_fitted <- logLik(logit_model_Average)

null_model <- glm(first_violent_offence_post_14 ~ 1, data = Combined_Data, family = "binomial")

logLik_null <- logLik(null_model)

mcfadden_r2 <- 1 - (as.numeric(logLik_fitted) / as.numeric(logLik_null))

print(paste("McFadden's R-squared:", round(mcfadden_r2, 4)))


# Frequency Effect Post-14

logit_model_Frequency_Post <- glm(first_violent_offence_post_14 ~ ( Gender + Missing_Frequency + Sexual_Assault_Frequency + Physical_Assault_Frequency + Neglect_Frequency + NV_Offence_Frequency + Witness_Offence_Frequency + NV_Offence_Frequency_Sexual + NV_Offence_Frequency_victim) * Gender, data = Combined_Data, family = binomial(link = "logit"))

summary(logit_model_Frequency)

logLik_fitted <- logLik(logit_model_Frequency)

null_model <- glm(first_violent_offence_post_14 ~ 1, data = Combined_Data, family = "binomial")

logLik_null <- logLik(null_model)

mcfadden_r2 <- 1 - (as.numeric(logLik_fitted) / as.numeric(logLik_null))

print(paste("McFadden's R-squared:", round(mcfadden_r2, 4)))




# This code generates confidence intervals for interaciton effects

install.packages("boot")
library(boot)

Combined_Data <- subset(Combined_Data, Gender == 3)


# Average Effect


logit_model_Average <- glm(First_Offence_1_to_5 ~ ( Gender + missing_history + victim_history_sexual + victim_history_physical + Neglect_history + Offence_history_NV + Witness_history + Offence_history_NV_sexual + Offence_history_NV_Victim) * Gender, data = Combined_Data, family = binomial(link = "logit"))
summary(logit_model_Average)

get_coef <- function(data, indices) {
  d <- data[indices, ]  
  model_boot_Average <- glm(First_Offence_1_to_5 ~ ( Gender + missing_history + victim_history_sexual + victim_history_physical + Neglect_history + Offence_history_NV + Witness_history + Offence_history_NV_sexual + Offence_history_NV_Victim) * Gender, data = d, family = binomial(link = "logit"))
  return(coef(model_boot_Average)) 
}


set.seed(123)


boot_results_Average <- boot(data = Combined_Data, statistic = get_coef, R = 1000)


print(boot_results_Average)


ci_list <- list()


for (i in 1:length(coef(logit_model_Average))) {
  ci <- boot.ci(boot_results_Average, type = "perc", index = i)  
  ci_list[[names(coef(logit_model_Average))[i]]] <- ci
}


print(ci_list)


ci_table <- data.frame(
  Parameter = names(coef(logit_model_Average)),  
  Lower_CI = sapply(ci_list, function(x) x$percent[4]),  
  Upper_CI = sapply(ci_list, function(x) x$percent[5])   
)


print(ci_table)


# Frequency Effect


logit_model_Frequency <- glm(First_Offence_1_to_5 ~ (Gender + Missing_Frequency + Sexual_Assault_Frequency + Physical_Assault_Frequency + Neglect_Frequency + NV_Offence_Frequency + Witness_Offence_Frequency + NV_Offence_Frequency_Sexual + NV_Offence_Frequency_victim) * Gender, data = Combined_Data, family = binomial(link = "logit"))


get_coef <- function(data, indices) {
  d <- data[indices, ]  
  model_boot_Average <- glm(First_Offence_1_to_5 ~ (Gender + Missing_Frequency + Sexual_Assault_Frequency + Physical_Assault_Frequency + Neglect_Frequency + NV_Offence_Frequency + Witness_Offence_Frequency + NV_Offence_Frequency_Sexual + NV_Offence_Frequency_victim) * Gender, data = d, family = binomial(link = "logit"))
  return(coef(model_boot_Average))  
}


set.seed(123)


boot_results_Average <- boot(data = Combined_Data, statistic = get_coef, R = 1000)


print(boot_results_Average)


ci_list <- list()


for (i in 1:length(coef(logit_model_Frequency))) {
  ci <- boot.ci(boot_results_Average, type = "perc", index = i)  
  ci_list[[names(coef(logit_model_Frequency))[i]]] <- ci
}


print(ci_list)


ci_table <- data.frame(
  Parameter = names(coef(logit_model_Frequency)),  
  Lower_CI = sapply(ci_list, function(x) x$percent[4]),  
  Upper_CI = sapply(ci_list, function(x) x$percent[5])   
)


print(ci_table)




# Average Effect Pre-14


logit_model_Pre_14 <- glm(first_violent_offence_pre_14 ~ ( Gender + missing_history + victim_history_sexual + victim_history_physical + Neglect_history + Offence_history_NV + Witness_history + Offence_history_NV_sexual + Offence_history_NV_Victim) * Gender, data = Combined_Data, family = binomial(link = "logit"))

summary(logit_model_Pre_14)


get_coef <- function(data, indices) {
  d <- data[indices, ]  
  model_boot_Average <- glm(first_violent_offence_pre_14 ~ ( Gender + missing_history + victim_history_sexual + victim_history_physical + Neglect_history + Offence_history_NV + Witness_history + Offence_history_NV_sexual + Offence_history_NV_Victim) * Gender, data = d, family = binomial(link = "logit"))
  return(coef(model_boot_Average))  
}


set.seed(123)


boot_results_Average <- boot(data = Combined_Data, statistic = get_coef, R = 1000)


print(boot_results_Average)


ci_list <- list()


for (i in 1:length(coef(logit_model_Pre_14))) {
  ci <- boot.ci(boot_results_Average, type = "perc", index = i)  
  ci_list[[names(coef(logit_model_Pre_14))[i]]] <- ci
}


print(ci_list)


ci_table <- data.frame(
  Parameter = names(coef(logit_model_Pre_14)),  
  Lower_CI = sapply(ci_list, function(x) x$percent[4]),  
  Upper_CI = sapply(ci_list, function(x) x$percent[5])   
)


print(ci_table)

# Combined_Data <- subset(Combined_Data, Gender == 2)



# Frequency Effect Pre-14


logit_model_Pre_14_Frequency <- glm(first_violent_offence_pre_14 ~ ( Gender + Missing_history_freq_Pre_14 + Sexual_history_freq_Pre_14 + Physical_Assault_Frequency_Pre_14 + Neglect_history_freq_Pre_14 + NV_history_freq_Pre_14 + witness_freq_Pre_14 + nv_victim_history_freq_Pre_14 + sexual_history_freq_Pre_14) * Gender, data = Combined_Data, family = binomial(link = "logit"))
summary(logit_model_Pre_14_Frequency)


get_coef <- function(data, indices) {
  d <- data[indices, ]  
  model_boot_Average <- glm(first_violent_offence_pre_14 ~ ( Gender + Missing_history_freq_Pre_14 + Sexual_history_freq_Pre_14 + Physical_Assault_Frequency_Pre_14 + Neglect_history_freq_Pre_14 + NV_history_freq_Pre_14 + witness_freq_Pre_14 + nv_victim_history_freq_Pre_14 + sexual_history_freq_Pre_14) * Gender, data = d, family = binomial(link = "logit"))
  return(coef(model_boot_Average))  
}


set.seed(123)


boot_results_Average <- boot(data = Combined_Data, statistic = get_coef, R = 1000)


print(boot_results_Average)


ci_list <- list()


for (i in 1:length(coef(logit_model_Pre_14_Frequency))) {
  ci <- boot.ci(boot_results_Average, type = "perc", index = i)  
  ci_list[[names(coef(logit_model_Pre_14_Frequency))[i]]] <- ci
}


print(ci_list)


ci_table <- data.frame(
  Parameter = names(coef(logit_model_Pre_14_Frequency)),  
  Lower_CI = sapply(ci_list, function(x) x$percent[4]),  
  Upper_CI = sapply(ci_list, function(x) x$percent[5])   
)


print(ci_table)



# Average Effect Post-14


logit_model_Average_Post <- glm(first_violent_offence_post_14 ~ ( Gender + missing_history + victim_history_sexual + victim_history_physical + Neglect_history + Offence_history_NV + Witness_history + Offence_history_NV_sexual + Offence_history_NV_Victim) * Gender, data = Combined_Data, family = binomial(link = "logit"))


get_coef <- function(data, indices) {
  d <- data[indices, ]  
  model_boot_Average <- glm(first_violent_offence_post_14 ~ ( Gender + missing_history + victim_history_sexual + victim_history_physical + Neglect_history + Offence_history_NV + Witness_history + Offence_history_NV_sexual + Offence_history_NV_Victim) * Gender, data = d, family = binomial(link = "logit"))
  return(coef(model_boot_Average)) 
}


set.seed(123)


boot_results_Average <- boot(data = Combined_Data, statistic = get_coef, R = 1000)


print(boot_results_Average)


ci_list <- list()


for (i in 1:length(coef(logit_model_Average_Post))) {
  ci <- boot.ci(boot_results_Average, type = "perc", index = i)  
  ci_list[[names(coef(logit_model_Average_Post))[i]]] <- ci
}


print(ci_list)


ci_table <- data.frame(
  Parameter = names(coef(logit_model_Average_Post)),  
  Lower_CI = sapply(ci_list, function(x) x$percent[4]),  
  Upper_CI = sapply(ci_list, function(x) x$percent[5])   
)


print(ci_table)


# Frequency Effect Post-14


logit_model_Frequency_Post <- glm(first_violent_offence_post_14 ~ ( Gender + Missing_Frequency + Sexual_Assault_Frequency + Physical_Assault_Frequency + Neglect_Frequency + NV_Offence_Frequency + Witness_Offence_Frequency + NV_Offence_Frequency_Sexual + NV_Offence_Frequency_victim) * Gender, data = Combined_Data, family = binomial(link = "logit"))


get_coef <- function(data, indices) {
  d <- data[indices, ]  
  model_boot_Average <- glm(first_violent_offence_post_14 ~ ( Gender + Missing_Frequency + Sexual_Assault_Frequency + Physical_Assault_Frequency + Neglect_Frequency + NV_Offence_Frequency + Witness_Offence_Frequency + NV_Offence_Frequency_Sexual + NV_Offence_Frequency_victim) * Gender, data = d, family = binomial(link = "logit"))
  return(coef(model_boot_Average))  
}


set.seed(123)


boot_results_Average <- boot(data = Combined_Data, statistic = get_coef, R = 1000)


print(boot_results_Average)


ci_list <- list()


for (i in 1:length(coef(logit_model_Frequency_Post))) {
  ci <- boot.ci(boot_results_Average, type = "perc", index = i)  
  ci_list[[names(coef(logit_model_Frequency_Post))[i]]] <- ci
}


print(ci_list)


ci_table <- data.frame(
  Parameter = names(coef(logit_model_Frequency_Post)),  
  Lower_CI = sapply(ci_list, function(x) x$percent[4]),  
  Upper_CI = sapply(ci_list, function(x) x$percent[5])   
)


print(ci_table)

