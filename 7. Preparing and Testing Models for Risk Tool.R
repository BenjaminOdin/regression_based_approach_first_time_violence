# Generating the three-way interaction effect Logistic Regressions which will feed the risk tool

# 10-16 Aggregate Offences


vars <- c("Missing_Frequency", "Sexual_Assault_Frequency", "Physical_Assault_Frequency",
          "NV_Offence_Frequency_victim", "Neglect_Frequency", "NV_Offence_Frequency",
          "Witness_Offence_Frequency", "NV_Offence_Frequency_Sexual")

combinations <- combn(vars, 2, simplify = FALSE)

three_way_terms <- sapply(combinations, function(x) paste("Gender", x[1], x[2], sep = ":"))

formula_text <- paste0(
  "First_Offence_1_to_5 ~ (Gender + ", 
  paste(vars, collapse = " + "), 
  ")^2 + ",
  paste(three_way_terms, collapse = " + ")
)

Risk_Calculator_7 <- glm(
  formula = as.formula(formula_text),
  family = binomial(link = "logit"),
  data = Combined_Data
)
summary(Risk_Calculator_7)


predictors <- c(
  "Gender",
  "Missing_Frequency",
  "Sexual_Assault_Frequency",
  "Physical_Assault_Frequency",
  "NV_Offence_Frequency_victim",
  "Neglect_Frequency",
  "NV_Offence_Frequency",
  "Witness_Offence_Frequency",
  "NV_Offence_Frequency_Sexual"
)

full_formula <- as.formula(
  paste(
    "First_Offence_1_to_5 ~ (", 
    paste(predictors, collapse = " + "), 
    ")^3"
  )
)

Risk_Calculator_7 <- glm(
  formula = full_formula,
  family = binomial(link = "logit"),
  data = Combined_Data
)

summary(Risk_Calculator_7)





# AUC for 10-16 Years Olds and Aggregate Offences

png("ROC_Curve_with_AUC.png", width = 800, height = 600)

par(mar = c(5, 5, 4, 2) + 0.1, family = "serif")  

pred_scores <- predict(Risk_Calculator_7, type = "response")

true_outcomes <- Combined_Data$First_Offence_1_to_5

pred <- prediction(pred_scores, true_outcomes)

perf <- performance(pred, "tpr", "fpr")

auc <- performance(pred, "auc")

auc_value <- unlist(slot(auc, "y.values"))

print(paste("AUC:", auc_value))




# 10-13 Aggregate Offences

vars <- c("Missing_history_freq_Pre_14",
          "Sexual_history_freq_Pre_14",
          "Physical_Assault_Frequency_Pre_14",
          "nv_victim_history_freq_Pre_14",
          "Neglect_history_freq_Pre_14",
          "NV_history_freq_Pre_14",
          "witness_freq_Pre_14",
          "sexual_history_freq_Pre_14")

combinations <- combn(vars, 2, simplify = FALSE)

three_way_terms <- sapply(combinations, function(x) paste("Gender", x[1], x[2], sep = ":"))

formula_text <- paste0(
  "first_violent_offence_pre_14 ~ (Gender + ", 
  paste(vars, collapse = " + "), 
  ")^2 + ",
  paste(three_way_terms, collapse = " + ")
)

Risk_Calculator_8 <- glm(
  formula = as.formula(formula_text),
  family = binomial(link = "logit"),
  data = Combined_Data
)
summary(Risk_Calculator_8)


predictors <- c(
  "Gender",
  "Missing_history_freq_Pre_14",
  "Sexual_history_freq_Pre_14",
  "Physical_Assault_Frequency_Pre_14",
  "nv_victim_history_freq_Pre_14",
  "Neglect_history_freq_Pre_14",
  "NV_history_freq_Pre_14",
  "witness_freq_Pre_14",
  "sexual_history_freq_Pre_14"
)

full_formula <- as.formula(
  paste(
    "first_violent_offence_pre_14 ~ (", 
    paste(predictors, collapse = " + "), 
    ")^3"
  )
)

Risk_Calculator_8 <- glm(
  formula = full_formula,
  family = binomial(link = "logit"),
  data = Combined_Data
)

summary(Risk_Calculator_8)

# AUC for 10-13 Years Olds and Aggregate Offences

png("ROC_Curve_with_AUC.png", width = 800, height = 600)


par(mar = c(5, 5, 4, 2) + 0.1, family = "serif")  


pred_scores <- predict(Risk_Calculator_8, type = "response")


true_outcomes <- Combined_Data$first_violent_offence_pre_14


pred <- prediction(pred_scores, true_outcomes)


perf <- performance(pred, "tpr", "fpr")


auc <- performance(pred, "auc")
auc_value <- unlist(slot(auc, "y.values"))


print(paste("AUC:", auc_value))




# 14-16 Aggregate Offences


vars <- c( "Missing_Frequency",
           "Sexual_Assault_Frequency",
           "Physical_Assault_Frequency",
           "NV_Offence_Frequency_victim",
           "Neglect_Frequency",
           "NV_Offence_Frequency",
           "Witness_Offence_Frequency",
           "NV_Offence_Frequency_Sexual")

combinations <- combn(vars, 2, simplify = FALSE)

three_way_terms <- sapply(combinations, function(x) paste("Gender", x[1], x[2], sep = ":"))

formula_text <- paste0(
  "first_violent_offence_post_14 ~ (Gender + ", 
  paste(vars, collapse = " + "), 
  ")^2 + ",
  paste(three_way_terms, collapse = " + ")
)

Risk_Calculator_12 <- glm(
  formula = as.formula(formula_text),
  family = binomial(link = "logit"),
  data = Combined_Data
)
summary(Risk_Calculator_12)

predictors <- c(
  "Gender",
  "Missing_Frequency",
  "Sexual_Assault_Frequency",
  "Physical_Assault_Frequency",
  "NV_Offence_Frequency_victim",
  "Neglect_Frequency",
  "NV_Offence_Frequency",
  "Witness_Offence_Frequency",
  "NV_Offence_Frequency_Sexual"
)

full_formula <- as.formula(
  paste(
    "first_violent_offence_post_14 ~ (", 
    paste(predictors, collapse = " + "), 
    ")^3"
  )
)

Risk_Calculator_12 <- glm(
  formula = full_formula,
  family = binomial(link = "logit"),
  data = Combined_Data
)

summary(Risk_Calculator_12)



# AUC for 14-16 Years Olds and Aggregate Offences

png("ROC_Curve_with_AUC.png", width = 800, height = 600)

par(mar = c(5, 5, 4, 2) + 0.1, family = "serif")  

pred_scores <- predict(Risk_Calculator_12, type = "response")

true_outcomes <- Combined_Data$first_violent_offence_post_14

pred <- prediction(pred_scores, true_outcomes)

perf <- performance(pred, "tpr", "fpr")

auc <- performance(pred, "auc")

auc_value <- unlist(slot(auc, "y.values"))

print(paste("AUC:", auc_value))


# Save model outputs as RDS Files

saveRDS(Risk_Calculator_7, file = "C:/Users/T2303/Documents/RiskCalc/Risk_Calculator_7.rds")
saveRDS(Risk_Calculator_8, file = "C:/Users/T2303/Documents/RiskCalc/Risk_Calculator_8.rds")
saveRDS(Risk_Calculator_12, file = "C:/Users/T2303/Documents/RiskCalc/Risk_Calculator_12.rds")



# Metrics and K-Fold Cross Validation 

# Install the following two packages and libraries 

install.packages("pROC")
install.packages("caret")
library(pROC)
library(caret)


# First calculate Brier Score

pred <- predict(Risk_Calculator_7, type = "response")   

app_brier <- mean((Combined_Data$First_Offence_1_to_5 - pred)^2)

print(app_brier)

pred <- predict(Risk_Calculator_8, type = "response")   

app_brier <- mean((Combined_Data$first_violent_offence_pre_14 - pred)^2)

print(app_brier)

pred <- predict(Risk_Calculator_12, type = "response")   

app_brier <- mean((Combined_Data$first_violent_offence_post_14 - pred)^2)

print(app_brier)

# Now calculate Calibration slope and CITL

lp <- predict(Risk_Calculator_7, type = "link")  
cal_model <- glm(First_Offence_1_to_5 ~ lp, data = Combined_Data, family = binomial)

cal_intercept <- coef(cal_model)[1]   # CITL
cal_slope <- coef(cal_model)[2]       # calibration slope

print(cal_intercept)

print(cal_slope )


lp <- predict(Risk_Calculator_8, type = "link")  
cal_model <- glm(first_violent_offence_pre_14 ~ lp, data = Combined_Data, family = binomial)

cal_intercept <- coef(cal_model)[1]   # CITL
cal_slope <- coef(cal_model)[2]       # calibration slope

print(cal_intercept)

print(cal_slope )


lp <- predict(Risk_Calculator_12, type = "link")  
cal_model <- glm(first_violent_offence_post_14 ~ lp, data = Combined_Data, family = binomial)

cal_intercept <- coef(cal_model)[1]   # CITL
cal_slope <- coef(cal_model)[2]       # calibration slope

print(cal_intercept)

print(cal_slope )



# Sensitivity and Specificity at thresholds 0.05 to 0.30

pred <- predict(Risk_Calculator_7, type = "response")  

thresholds <- seq(0.05, 0.30, by = 0.05)

sens_spec <- lapply(thresholds, function(t) {
  pred_class <- ifelse(pred >= t, 1, 0)
  cm <- table(Predicted = pred_class, Actual = Combined_Data$First_Offence_1_to_5)
  
  # Handle cases with missing cells
  TP <- ifelse(!is.na(cm["1","1"]), cm["1","1"], 0)
  TN <- ifelse(!is.na(cm["0","0"]), cm["0","0"], 0)
  FP <- ifelse(!is.na(cm["1","0"]), cm["1","0"], 0)
  FN <- ifelse(!is.na(cm["0","1"]), cm["0","1"], 0)
  
  sensitivity <- ifelse((TP + FN) > 0, TP / (TP + FN), NA)
  specificity <- ifelse((TN + FP) > 0, TN / (TN + FP), NA)
  
  data.frame(Threshold = t, Sensitivity = sensitivity, Specificity = specificity)
})

sens_spec_df <- do.call(rbind, sens_spec)
print(sens_spec_df)



pred <- predict(Risk_Calculator_8, type = "response")  

thresholds <- seq(0.05, 0.30, by = 0.05)

sens_spec <- lapply(thresholds, function(t) {
  pred_class <- ifelse(pred >= t, 1, 0)
  cm <- table(Predicted = pred_class, Actual = Combined_Data$first_violent_offence_pre_14)
  
  # Handle cases with missing cells
  TP <- ifelse(!is.na(cm["1","1"]), cm["1","1"], 0)
  TN <- ifelse(!is.na(cm["0","0"]), cm["0","0"], 0)
  FP <- ifelse(!is.na(cm["1","0"]), cm["1","0"], 0)
  FN <- ifelse(!is.na(cm["0","1"]), cm["0","1"], 0)
  
  sensitivity <- ifelse((TP + FN) > 0, TP / (TP + FN), NA)
  specificity <- ifelse((TN + FP) > 0, TN / (TN + FP), NA)
  
  data.frame(Threshold = t, Sensitivity = sensitivity, Specificity = specificity)
})

sens_spec_df <- do.call(rbind, sens_spec)
print(sens_spec_df)



pred <- predict(Risk_Calculator_12, type = "response")  

thresholds <- seq(0.05, 0.30, by = 0.05)

sens_spec <- lapply(thresholds, function(t) {
  pred_class <- ifelse(pred >= t, 1, 0)
  cm <- table(Predicted = pred_class, Actual = Combined_Data$first_violent_offence_post_14)
  
  # Handle cases with missing cells
  TP <- ifelse(!is.na(cm["1","1"]), cm["1","1"], 0)
  TN <- ifelse(!is.na(cm["0","0"]), cm["0","0"], 0)
  FP <- ifelse(!is.na(cm["1","0"]), cm["1","0"], 0)
  FN <- ifelse(!is.na(cm["0","1"]), cm["0","1"], 0)
  
  sensitivity <- ifelse((TP + FN) > 0, TP / (TP + FN), NA)
  specificity <- ifelse((TN + FP) > 0, TN / (TN + FP), NA)
  
  data.frame(Threshold = t, Sensitivity = sensitivity, Specificity = specificity)
})

sens_spec_df <- do.call(rbind, sens_spec)
print(sens_spec_df)


# K-Fold Cross Validation Testing


Combined_Data$First_Offence_1_to_5 <- factor(Combined_Data$First_Offence_1_to_5, levels = c(0,1), labels = c("No", "Yes"))

ctrl <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = TRUE
)

cv_model <- train(
  formula(Risk_Calculator_7),
  data = Combined_Data,
  method = "glm",
  family = binomial,
  trControl = ctrl,
  metric = "ROC"
)

# Recalculate Brier Score 

brier_score <- mean((cv_model$pred$Yes - as.numeric(cv_model$pred$obs == "Yes"))^2)
print(paste("Brier score:", round(brier_score, 4)))

# Recalculate Calibration Slope and CITL

cv_model$pred$obs_num <- as.numeric(cv_model$pred$obs == "Yes")
logit <- function(p) { log(p / (1 - p)) }

calibration_model <- glm(
  obs_num ~ logit(Yes),
  data = cv_model$pred,
  family = binomial
)

calibration_intercept <- coef(calibration_model)[1]
calibration_slope <- coef(calibration_model)[2]

# Sensitivity and Specificity

thresholds <- c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4)
results <- data.frame(threshold=numeric(),
                      sensitivity=numeric(),
                      specificity=numeric())

obs_class <- factor(cv_model$pred$obs, levels=c("No","Yes"))  # "Yes" = positive class

for (t in thresholds) {
  
  pred_class <- ifelse(cv_model$pred$Yes >= t, "Yes", "No")
  pred_class <- factor(pred_class, levels=c("No","Yes"))
  
  cm <- confusionMatrix(pred_class, obs_class, positive="Yes")
  
  results <- rbind(results, data.frame(
    threshold = t,
    sensitivity = cm$byClass["Sensitivity"],
    specificity = cm$byClass["Specificity"]
  ))
}

all_results <- list(
  Brier_Score = round(brier_score, 4),
  Calibration_Intercept = round(calibration_intercept, 4),
  Calibration_Slope = round(calibration_slope, 4),
  Sensitivity_Specificity = results
)

all_results




Combined_Data$first_violent_offence_pre_14 <- factor(Combined_Data$first_violent_offence_pre_14, levels = c(0,1), labels = c("No", "Yes"))

ctrl <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = TRUE
)

cv_model <- train(
  formula(Risk_Calculator_8),
  data = Combined_Data,
  method = "glm",
  family = binomial,
  trControl = ctrl,
  metric = "ROC"
)

# Recalculate Brier Score 

brier_score <- mean((cv_model$pred$Yes - as.numeric(cv_model$pred$obs == "Yes"))^2)
print(paste("Brier score:", round(brier_score, 4)))

# Recalculate Calibration Slope and CITL

cv_model$pred$obs_num <- as.numeric(cv_model$pred$obs == "Yes")
logit <- function(p) { log(p / (1 - p)) }

calibration_model <- glm(
  obs_num ~ logit(Yes),
  data = cv_model$pred,
  family = binomial
)

calibration_intercept <- coef(calibration_model)[1]
calibration_slope <- coef(calibration_model)[2]

# Sensitivity and Specificity

thresholds <- c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4)
results <- data.frame(threshold=numeric(),
                      sensitivity=numeric(),
                      specificity=numeric())

obs_class <- factor(cv_model$pred$obs, levels=c("No","Yes"))  # "Yes" = positive class

for (t in thresholds) {
  
  pred_class <- ifelse(cv_model$pred$Yes >= t, "Yes", "No")
  pred_class <- factor(pred_class, levels=c("No","Yes"))
  
  cm <- confusionMatrix(pred_class, obs_class, positive="Yes")
  
  results <- rbind(results, data.frame(
    threshold = t,
    sensitivity = cm$byClass["Sensitivity"],
    specificity = cm$byClass["Specificity"]
  ))
}

all_results <- list(
  Brier_Score = round(brier_score, 4),
  Calibration_Intercept = round(calibration_intercept, 4),
  Calibration_Slope = round(calibration_slope, 4),
  Sensitivity_Specificity = results
)

all_results




Combined_Data$first_violent_offence_post_14 <- factor(Combined_Data$first_violent_offence_post_14, levels = c(0,1), labels = c("No", "Yes"))

ctrl <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = TRUE
)

cv_model <- train(
  formula(Risk_Calculator_12),
  data = Combined_Data,
  method = "glm",
  family = binomial,
  trControl = ctrl,
  metric = "ROC"
)

# Recalculate Brier Score 

brier_score <- mean((cv_model$pred$Yes - as.numeric(cv_model$pred$obs == "Yes"))^2)
print(paste("Brier score:", round(brier_score, 4)))

# Recalculate Calibration Slope and CITL

cv_model$pred$obs_num <- as.numeric(cv_model$pred$obs == "Yes")
logit <- function(p) { log(p / (1 - p)) }

calibration_model <- glm(
  obs_num ~ logit(Yes),
  data = cv_model$pred,
  family = binomial
)

calibration_intercept <- coef(calibration_model)[1]
calibration_slope <- coef(calibration_model)[2]

# Sensitivity and Specificity

thresholds <- c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4)
results <- data.frame(threshold=numeric(),
                      sensitivity=numeric(),
                      specificity=numeric())

obs_class <- factor(cv_model$pred$obs, levels=c("No","Yes"))  # "Yes" = positive class

for (t in thresholds) {
  
  pred_class <- ifelse(cv_model$pred$Yes >= t, "Yes", "No")
  pred_class <- factor(pred_class, levels=c("No","Yes"))
  
  cm <- confusionMatrix(pred_class, obs_class, positive="Yes")
  
  results <- rbind(results, data.frame(
    threshold = t,
    sensitivity = cm$byClass["Sensitivity"],
    specificity = cm$byClass["Specificity"]
  ))
}

all_results <- list(
  Brier_Score = round(brier_score, 4),
  Calibration_Intercept = round(calibration_intercept, 4),
  Calibration_Slope = round(calibration_slope, 4),
  Sensitivity_Specificity = results
)

all_results






