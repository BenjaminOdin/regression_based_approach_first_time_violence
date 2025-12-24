# Regression Analysis (Cox models include Schoenfeld residual test)

# Filter dataset to include only specific gender (2 = Female and 3 = male)

#Combined_Data <- subset(Combined_Data, Gender == 3)
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

exponential_aft_model_Average <- survreg(Surv(Time_All, First_Offence_1_to_5) ~ missing_history + victim_history_sexual + victim_history_physical + Neglect_history + Offence_history_NV + Witness_history + Offence_history_NV_sexual + Offence_history_NV_Victim, data = Combined_Data, dist = "exponential")

summary(exponential_aft_model_Average)


# Exponential AFT Regression - Frequency Effect

exponential_aft_model_Frequency <- survreg(Surv(Time_All, First_Offence_1_to_5) ~ Missing_Frequency + Sexual_Assault_Frequency + Physical_Assault_Frequency + Neglect_Frequency + NV_Offence_Frequency + Witness_Offence_Frequency + NV_Offence_Frequency_Sexual + NV_Offence_Frequency_victim, data = Combined_Data, dist = "exponential")

summary(exponential_aft_model_Frequency)

# Exponential AFT Regression - Average Effect Pre-14

exponential_aft_model_Pre_14 <- survreg(Surv(Time_All, first_violent_offence_pre_14) ~ Gender + Neglect_history_pre_14 + victim_history_sexual_pre_14 + victim_history_physical_pre_14 + Offence_history_NV_pre_14 + missing_history_pre_14 + Witness_history_pre_14 + Offence_history_victim_pre_14 + Offence_history_sexual_pre_14, data = Combined_Data, dist = "exponential")

summary(exponential_aft_model_Pre_14)


# Exponential AFT Regression - Frequency Effect Pre-14

exponential_aft_model_Pre_14_Freq <- survreg(Surv(Time_All, first_violent_offence_pre_14) ~ Gender + Neglect_history_freq_Pre_14 + Sexual_history_freq_Pre_14 + Physical_Assault_Frequency_Pre_14 + Missing_history_freq_Pre_14 + NV_history_freq_Pre_14 + witness_freq_Pre_14 + nv_victim_history_freq_Pre_14 + sexual_history_freq_Pre_14, data = Combined_Data, dist = "exponential")

summary(exponential_aft_model_Pre_14_Freq)


# Exponential AFT Regression - Average Effect post-14

exponential_aft_model_Average_mid <- survreg(Surv(Time_All, first_violent_offence_post_14) ~ missing_history + victim_history_sexual + victim_history_physical + Neglect_history + Offence_history_NV + Witness_history + Offence_history_NV_sexual + Offence_history_NV_Victim, data = Combined_Data, dist = "exponential")

summary(exponential_aft_model_Average_mid)


# Exponential AFT Regression - Frequency Effect post-14

exponential_aft_model_Frequency_mid <- survreg(Surv(Time_All, first_violent_offence_post_14) ~ Missing_Frequency + Sexual_Assault_Frequency + Physical_Assault_Frequency + Neglect_Frequency + NV_Offence_Frequency + Witness_Offence_Frequency + NV_Offence_Frequency_Sexual + NV_Offence_Frequency_victim, data = Combined_Data, dist = "exponential")

summary(exponential_aft_model_Frequency_mid)





# Weibull AFT Regression - Average Effect

weibull_aft_model_Average <- survreg(Surv(Time_All, First_Offence_1_to_5) ~ missing_history + victim_history_sexual + victim_history_physical + Neglect_history + Offence_history_NV + Witness_history + Offence_history_NV_sexual + Offence_history_NV_Victim, data = Combined_Data, dist = "weibull")

summary(weibull_aft_model_Average)


# Weibull AFT Regression - Frequency Effect

weibull_aft_model_Frequency <- survreg(Surv(Time_All, First_Offence_1_to_5) ~ Missing_Frequency + Sexual_Assault_Frequency + Physical_Assault_Frequency + Neglect_Frequency + NV_Offence_Frequency + Witness_Offence_Frequency + NV_Offence_Frequency_Sexual + NV_Offence_Frequency_victim, data = Combined_Data, dist = "weibull")

summary(weibull_aft_model_Frequency)

# Weibull AFT Regression - Average Effect Pre-14

weibull_aft_model_Pre_14 <- survreg(Surv(Time_All, first_violent_offence_pre_14) ~ Gender + Neglect_history_pre_14 + victim_history_sexual_pre_14 + victim_history_physical_pre_14 + Offence_history_NV_pre_14 + missing_history_pre_14 + Witness_history_pre_14 + Offence_history_victim_pre_14 + Offence_history_sexual_pre_14, data = Combined_Data, dist = "weibull")

summary(weibull_aft_model_Pre_14)


# Weibull AFT Regression - Frequency Effect Pre-14

weibull_aft_model_Pre_14_Freq <- survreg(Surv(Time_All, first_violent_offence_pre_14) ~ Gender + Neglect_history_freq_Pre_14 + Sexual_history_freq_Pre_14 + Physical_Assault_Frequency_Pre_14 + Missing_history_freq_Pre_14 + NV_history_freq_Pre_14 + witness_freq_Pre_14 + nv_victim_history_freq_Pre_14 + sexual_history_freq_Pre_14, data = Combined_Data, dist = "weibull")

summary(weibull_aft_model_Pre_14_Freq)


# Weibull AFT Regression - Average Effect Post-14

weibull_aft_model_Average_mid <- survreg(Surv(Time_All, first_violent_offence_post_14) ~ missing_history + victim_history_sexual + victim_history_physical + Neglect_history + Offence_history_NV + Witness_history + Offence_history_NV_sexual + Offence_history_NV_Victim, data = Combined_Data, dist = "weibull")

summary(weibull_aft_model_Average_mid)


# Weibull AFT Regression - Frequency Effect Post-14

weibull_aft_model_Frequency_mid <- survreg(Surv(Time_All, first_violent_offence_post_14) ~ Missing_Frequency + Sexual_Assault_Frequency + Physical_Assault_Frequency + Neglect_Frequency + NV_Offence_Frequency + Witness_Offence_Frequency + NV_Offence_Frequency_Sexual + NV_Offence_Frequency_victim, data = Combined_Data, dist = "weibull")

summary(weibull_aft_model_Frequency_mid)



# Lognormal AFT Regression - Average Effect

lognormal_aft_model_Average <- survreg(Surv(Time_All, First_Offence_1_to_5) ~ missing_history + victim_history_sexual + victim_history_physical + Neglect_history + Offence_history_NV + Witness_history + Offence_history_NV_sexual + Offence_history_NV_Victim, data = Combined_Data, dist = "lognormal")

summary(lognormal_aft_model_Average)


# Lognormal AFT Regression - Frequency Effect

lognormal_aft_model_Frequency <- survreg(Surv(Time_All, First_Offence_1_to_5) ~ Missing_Frequency + Sexual_Assault_Frequency + Physical_Assault_Frequency + Neglect_Frequency + NV_Offence_Frequency + Witness_Offence_Frequency + NV_Offence_Frequency_Sexual + NV_Offence_Frequency_victim, data = Combined_Data, dist = "lognormal")

summary(lognormal_aft_model_Frequency)


# Lognormal AFT Regression - Average Effect Pre-14

lognormal_aft_model_Pre_14 <- survreg(Surv(Time_All, first_violent_offence_pre_14) ~ Gender + Neglect_history_pre_14 + victim_history_sexual_pre_14 + victim_history_physical_pre_14 + Offence_history_NV_pre_14 + missing_history_pre_14 + Witness_history_pre_14 + Offence_history_victim_pre_14 + Offence_history_sexual_pre_14, data = Combined_Data, dist = "lognormal")

summary(lognormal_aft_model_Pre_14)


# Lognormal AFT Regression - Frequency Effect Pre-14

lognormal_aft_model_Pre_14_Freq <- survreg(Surv(Time_All, first_violent_offence_pre_14) ~ Gender + Neglect_history_freq_Pre_14 + Sexual_history_freq_Pre_14 + Physical_Assault_Frequency_Pre_14 + Missing_history_freq_Pre_14 + NV_history_freq_Pre_14 + witness_freq_Pre_14 + nv_victim_history_freq_Pre_14 + sexual_history_freq_Pre_14, data = Combined_Data, dist = "lognormal")

summary(lognormal_aft_model_Pre_14_Freq)


# Lognormal AFT Regression - Average Effect Post-14

lognormal_aft_model_Average_mid <- survreg(Surv(Time_All, first_violent_offence_post_14) ~ missing_history + victim_history_sexual + victim_history_physical + Neglect_history + Offence_history_NV + Witness_history + Offence_history_NV_sexual + Offence_history_NV_Victim, data = Combined_Data, dist = "lognormal")

summary(lognormal_aft_model_Average_mid)


# Lognormal AFT Regression - Frequency Effect Post-14

lognormal_aft_model_Frequency_mid <- survreg(Surv(Time_All, first_violent_offence_post_14) ~ Missing_Frequency + Sexual_Assault_Frequency + Physical_Assault_Frequency + Neglect_Frequency + NV_Offence_Frequency + Witness_Offence_Frequency + NV_Offence_Frequency_Sexual + NV_Offence_Frequency_victim, data = Combined_Data, dist = "lognormal")

summary(lognormal_aft_model_Frequency_mid)



# Loglogistic AFT Regression - Average Effect

loglogistic_aft_model_Average <- survreg(Surv(Time_All, First_Offence_1_to_5) ~ missing_history + victim_history_sexual + victim_history_physical + Neglect_history + Offence_history_NV + Witness_history + Offence_history_NV_sexual + Offence_history_NV_Victim, data = Combined_Data, dist = "loglogistic")

summary(loglogistic_aft_model_Average)


# Loglogistic AFT Regression - Frequency Effect

loglogistic_aft_model_Frequency <- survreg(Surv(Time_All, First_Offence_1_to_5) ~ Missing_Frequency + Sexual_Assault_Frequency + Physical_Assault_Frequency + Neglect_Frequency + NV_Offence_Frequency + Witness_Offence_Frequency + NV_Offence_Frequency_Sexual + NV_Offence_Frequency_victim, data = Combined_Data, dist = "loglogistic")

summary(loglogistic_aft_model_Frequency)

# Loglogistic AFT Regression - Average Effect Pre-14

loglogistic_aft_model_Pre_14 <- survreg(Surv(Time_All, first_violent_offence_pre_14) ~ Gender + Neglect_history_pre_14 + victim_history_sexual_pre_14 + victim_history_physical_pre_14 + Offence_history_NV_pre_14 + missing_history_pre_14 + Witness_history_pre_14 + Offence_history_victim_pre_14 + Offence_history_sexual_pre_14, data = Combined_Data, dist = "loglogistic")

summary(loglogistic_aft_model_Pre_14)


# Loglogistic AFT Regression - Frequency Effect Pre-14

loglogistic_aft_model_Pre_14_Freq <- survreg(Surv(Time_All, first_violent_offence_pre_14) ~ Gender + Neglect_history_freq_Pre_14 + Sexual_history_freq_Pre_14 + Physical_Assault_Frequency_Pre_14 + Missing_history_freq_Pre_14 + NV_history_freq_Pre_14 + witness_freq_Pre_14 + nv_victim_history_freq_Pre_14 + sexual_history_freq_Pre_14, data = Combined_Data, dist = "loglogistic")

summary(loglogistic_aft_model_Pre_14_Freq)

# Loglogistic AFT Regression - Average Effect Post-14

loglogistic_aft_model_Average_mid <- survreg(Surv(Time_All, first_violent_offence_post_14) ~ missing_history + victim_history_sexual + victim_history_physical + Neglect_history + Offence_history_NV + Witness_history + Offence_history_NV_sexual + Offence_history_NV_Victim, data = Combined_Data, dist = "loglogistic")

summary(loglogistic_aft_model_Average_mid)


# Loglogistic AFT Regression - Frequency Effect Post-14

loglogistic_aft_model_Frequency_mid <- survreg(Surv(Time_All, first_violent_offence_post_14) ~ Missing_Frequency + Sexual_Assault_Frequency + Physical_Assault_Frequency + Neglect_Frequency + NV_Offence_Frequency + Witness_Offence_Frequency + NV_Offence_Frequency_Sexual + NV_Offence_Frequency_victim, data = Combined_Data, dist = "loglogistic")

summary(loglogistic_aft_model_Frequency_mid)


# Check AICs of TTE Models

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

logit_model_Frequency <- glm(First_Offence_1_to_5 ~ Gender + Missing_Frequency + Sexual_Assault_Frequency + Physical_Assault_Frequency + NV_Offence_Frequency_victim + Neglect_Frequency + NV_Offence_Frequency + Witness_Offence_Frequency + NV_Offence_Frequency_Sexual, data = Combined_Data, family = binomial(link = "logit"))

summary(logit_model_Frequency)

logLik_fitted <- logLik(logit_model_Frequency)

null_model <- glm(First_Offence_1_to_5 ~ 1, data = Combined_Data, family = "binomial")

logLik_null <- logLik(null_model)

mcfadden_r2 <- 1 - (as.numeric(logLik_fitted) / as.numeric(logLik_null))

print(paste("McFadden's R-squared:", round(mcfadden_r2, 4)))


# Logistic Regression - Average Effect Pre-14

logit_model_Pre_14 <- glm(first_violent_offence_pre_14 ~ Gender + missing_history_pre_14 + victim_history_sexual_pre_14 + victim_history_physical_pre_14 + Offence_history_victim_pre_14 + Neglect_history_pre_14 + Offence_history_NV_pre_14 + Witness_history_pre_14 + Offence_history_sexual_pre_14, data = Combined_Data, family = binomial(link = "logit"))

summary(logit_model_Pre_14)

logLik_fitted <- logLik(logit_model_Pre_14)

null_model <- glm(first_violent_offence_pre_14 ~ 1, data = Combined_Data, family = "binomial")

logLik_null <- logLik(null_model)

mcfadden_r2 <- 1 - (as.numeric(logLik_fitted) / as.numeric(logLik_null))

print(paste("McFadden's R-squared:", round(mcfadden_r2, 4)))



# Logistic Regression - Frequency Effect Pre-14

logit_model_Pre_14_Freq <- glm(first_violent_offence_pre_14 ~ Gender + Missing_history_freq_Pre_14 + Sexual_history_freq_Pre_14 + Physical_Assault_Frequency_Pre_14 + nv_victim_history_freq_Pre_14 + Neglect_history_freq_Pre_14 + NV_history_freq_Pre_14 + witness_freq_Pre_14 + sexual_history_freq_Pre_14, data = Combined_Data, family = binomial(link = "logit"))

summary(logit_model_Pre_14_Freq)

logLik_fitted <- logLik(logit_model_Pre_14_Freq)

null_model <- glm(first_violent_offence_pre_14 ~ 1, data = Combined_Data, family = "binomial")

logLik_null <- logLik(null_model)

mcfadden_r2 <- 1 - (as.numeric(logLik_fitted) / as.numeric(logLik_null))

print(paste("McFadden's R-squared:", round(mcfadden_r2, 4)))



# Logistic Regression - Average Effect Post 14

logit_model_Average_Post <- glm(first_violent_offence_post_14 ~ Gender + missing_history + victim_history_sexual + victim_history_physical + Offence_history_NV_Victim + Neglect_history + Offence_history_NV + Witness_history + Offence_history_NV_sexual, data = Combined_Data, family = binomial(link = "logit"))

summary(logit_model_Average_Post)

logLik_fitted <- logLik(logit_model_Average_Post)

null_model <- glm(First_Offence_1_to_5 ~ 1, data = Combined_Data, family = "binomial")

logLik_null <- logLik(null_model)

mcfadden_r2 <- 1 - (as.numeric(logLik_fitted) / as.numeric(logLik_null))

print(paste("McFadden's R-squared:", round(mcfadden_r2, 4)))


# Logistic Regression - Frequency Effect Post 14

logit_model_Frequency_Post <- glm(first_violent_offence_post_14 ~ Gender + Missing_Frequency + Sexual_Assault_Frequency + Physical_Assault_Frequency + NV_Offence_Frequency_victim + Neglect_Frequency + NV_Offence_Frequency + Witness_Offence_Frequency + NV_Offence_Frequency_Sexual, data = Combined_Data, family = binomial(link = "logit"))

summary(logit_model_Frequency_Post)

logLik_fitted <- logLik(logit_model_Frequency_Post)

null_model <- glm(First_Offence_1_to_5 ~ 1, data = Combined_Data, family = "binomial")

logLik_null <- logLik(null_model)

mcfadden_r2 <- 1 - (as.numeric(logLik_fitted) / as.numeric(logLik_null))

print(paste("McFadden's R-squared:", round(mcfadden_r2, 4)))



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



# Test Logistic Regression Assumption of Linearity in the Logit

install.packages("car")   
library(car)   

Combined_Data <- subset(Combined_Data, Gender == 3)


# LOESS Curves for Observation Period

Logistic_Frequency <- glm(First_Offence_1_to_5 ~ Missing_Frequency + Sexual_Assault_Frequency + 
                            Physical_Assault_Frequency + NV_Offence_Frequency_victim + Neglect_Frequency + 
                            NV_Offence_Frequency + Witness_Offence_Frequency + NV_Offence_Frequency_Sexual,
                          family = binomial, data = Combined_Data)

vars <- c("Missing_Frequency",
          "Sexual_Assault_Frequency",
          "Physical_Assault_Frequency",
          "NV_Offence_Frequency_victim",
          "Neglect_Frequency",
          "NV_Offence_Frequency",
          "Witness_Offence_Frequency",
          "NV_Offence_Frequency_Sexual")

custom_labels <- c(
  "Missing Incidents",
  "Sexual Victimisation",
  "Physical Victimisation",
  "Non-Violent Victimisation",
  "Neglect History",
  "Non-Violent Offences",
  "Witnessing Violence",
  "Sexual Offences"
)

pdf("Total.pdf", family = "serif", width = 7, height = 5)

for (i in seq_along(vars)) {
  crPlot(Logistic_Frequency,
         variable = vars[i],
         xlab = custom_labels[i],
         ylab = "Component + Residual")
}

dev.off()


# LOESS Curves for Early Adolescence

Logistic_Frequency <- glm(first_violent_offence_pre_14 ~ Missing_Frequency + Sexual_Assault_Frequency + 
                            Physical_Assault_Frequency + NV_Offence_Frequency_victim + Neglect_Frequency + 
                            NV_Offence_Frequency + Witness_Offence_Frequency + NV_Offence_Frequency_Sexual,
                          family = binomial, data = Combined_Data)

vars <- c("Missing_Frequency",
          "Sexual_Assault_Frequency",
          "Physical_Assault_Frequency",
          "NV_Offence_Frequency_victim",
          "Neglect_Frequency",
          "NV_Offence_Frequency",
          "Witness_Offence_Frequency",
          "NV_Offence_Frequency_Sexual")

custom_labels <- c(
  "Missing Incidents",
  "Sexual Victimisation",
  "Physical Victimisation",
  "Non-Violent Victimisation",
  "Neglect History",
  "Non-Violent Offences",
  "Witnessing Violence",
  "Sexual Offences"
)

pdf("malePre.pdf", family = "serif", width = 7, height = 5)

for (i in seq_along(vars)) {
  crPlot(Logistic_Frequency,
         variable = vars[i],
         xlab = custom_labels[i],
         ylab = "Component + Residual")
}

dev.off()


# LOESS Curves for Middle Adolescence

Logistic_Frequency <- glm(first_violent_offence_post_14 ~ Missing_Frequency + Sexual_Assault_Frequency + 
                            Physical_Assault_Frequency + NV_Offence_Frequency_victim + Neglect_Frequency + 
                            NV_Offence_Frequency + Witness_Offence_Frequency + NV_Offence_Frequency_Sexual,
                          family = binomial, data = Combined_Data)


vars <- c("Missing_Frequency",
          "Sexual_Assault_Frequency",
          "Physical_Assault_Frequency",
          "NV_Offence_Frequency_victim",
          "Neglect_Frequency",
          "NV_Offence_Frequency",
          "Witness_Offence_Frequency",
          "NV_Offence_Frequency_Sexual")

custom_labels <- c(
  "Missing Incidents",
  "Sexual Victimisation",
  "Physical Victimisation",
  "Non-Violent Victimisation",
  "Neglect History",
  "Non-Violent Offences",
  "Witnessing Violence",
  "Sexual Offences"
)

pdf("malePost.pdf", family = "serif", width = 7, height = 5)

for (i in seq_along(vars)) {
  crPlot(Logistic_Frequency,
         variable = vars[i],
         xlab = custom_labels[i],
         ylab = "Component + Residual")
}

dev.off()



# Generate Confidence Intervals for Logistic Regression Models


# Average Effect


logit_model_Average <- glm(First_Offence_1_to_5 ~ Gender + missing_history + victim_history_sexual + victim_history_physical + 
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


# Frequency Effect


logit_model_Frequency <- glm(First_Offence_1_to_5 ~ Gender + Missing_Frequency + Sexual_Assault_Frequency + 
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

null_model <- glm(First_Offence_1_to_5 ~ 1, data = Combined_Data, family = "binomial")

logLik_null <- logLik(null_model)

mcfadden_r2 <- 1 - (as.numeric(logLik_fitted) / as.numeric(logLik_null))

print(paste("McFadden's R-squared:", round(mcfadden_r2, 4)))


# Frequency Effect Pre-14

logit_model_Pre_14_Freq <- glm(first_violent_offence_pre_14 ~ ( Gender + Missing_history_freq_Pre_14 + Sexual_history_freq_Pre_14 + Physical_Assault_Frequency_Pre_14 + Neglect_history_freq_Pre_14 + NV_history_freq_Pre_14 + witness_freq_Pre_14 + nv_victim_history_freq_Pre_14 + sexual_history_freq_Pre_14) * Gender, data = Combined_Data, family = binomial(link = "logit"))

summary(logit_model_Pre_14_Freq)

logLik_fitted <- logLik(logit_model_Pre_14_Freq)

null_model <- glm(First_Offence_1_to_5 ~ 1, data = Combined_Data, family = "binomial")

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

null_model <- glm(First_Offence_1_to_5 ~ 1, data = Combined_Data, family = "binomial")

logLik_null <- logLik(null_model)

mcfadden_r2 <- 1 - (as.numeric(logLik_fitted) / as.numeric(logLik_null))

print(paste("McFadden's R-squared:", round(mcfadden_r2, 4)))


results_list <- list(
  Model1 = tidy(logit_model_Average_Post, exponentiate = FALSE, conf.int = TRUE),  # Odds Ratios
  Model2 = tidy(logit_model_Frequency_Post, exponentiate = FALSE, conf.int = TRUE)
)

# To get Confidence Intervals, run the below code. They are generated in Excel. It was at this stage that the author realised the value of exporting to Excel!

install.packages("broom")
install.packages("writexl")

library(broom)
library(writexl)

results_list <- list(
  Model1 = tidy(logit_model_Average, exponentiate = FALSE, conf.int = TRUE),  # Odds Ratios
  Model2 = tidy(logit_model_Frequency, exponentiate = FALSE, conf.int = TRUE),
  Model3 = tidy(logit_model_Pre_14, exponentiate = FALSE, conf.int = TRUE),  # Odds Ratios
  Model4 = tidy(logit_model_Pre_14_Freq, exponentiate = FALSE, conf.int = TRUE),
  Model5 = tidy(logit_model_Average_Post, exponentiate = FALSE, conf.int = TRUE),  # Odds Ratios
  Model6 = tidy(logit_model_Frequency_Post, exponentiate = FALSE, conf.int = TRUE)
)

write_xlsx(results_list, path = "Interaciton_Regression.xlsx")

shell.exec("Interaciton_Regression.xlsx")
