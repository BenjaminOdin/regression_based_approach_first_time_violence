# Sensitivity Testing

# Testing Common Offences, the exposure specification was manually updated on the second sub-test (in thesis) to alter the range of violent offences from 1:5 to 1:2 with the models below then re-run

# Combined_Data <- subset(Combined_Data, Gender == 3)

# Average Effect

logit_model_Average <- glm(First_Offence_Common ~ Gender + missing_history + victim_history_sexual + victim_history_physical + Offence_history_NV_Victim + Neglect_history + Offence_history_NV + Witness_history + Offence_history_NV_sexual, data = Combined_Data, family = binomial(link = "logit"))

summary(logit_model_Average)


# Frequency Effect

logit_model_Frequency <- glm(First_Offence_Common ~ Gender + Missing_Frequency + Sexual_Assault_Frequency + Physical_Assault_Frequency + NV_Offence_Frequency_victim + Neglect_Frequency + NV_Offence_Frequency + Witness_Offence_Frequency + NV_Offence_Frequency_Sexual, data = Combined_Data, family = binomial(link = "logit"))

summary(logit_model_Frequency)


# Average Effect Pre-14

logit_model_Pre_14 <- glm(first_violent_offence_pre_14_Common ~ Gender + missing_history_pre_14 + victim_history_sexual_pre_14 + victim_history_physical_pre_14 + Offence_history_victim_pre_14 + Neglect_history_pre_14 + Offence_history_NV_pre_14 + Witness_history_pre_14 + Offence_history_sexual_pre_14, data = Combined_Data, family = binomial(link = "logit"))

summary(logit_model_Pre_14)


# Frequency Effect Pre-14

logit_model_Pre_14_Freq <- glm(first_violent_offence_pre_14_Common ~ Gender + Missing_history_freq_Pre_14 + Sexual_history_freq_Pre_14 + Physical_Assault_Frequency_Pre_14 + nv_victim_history_freq_Pre_14 + Neglect_history_freq_Pre_14 + NV_history_freq_Pre_14 + witness_freq_Pre_14 + sexual_history_freq_Pre_14, data = Combined_Data, family = binomial(link = "logit"))

summary(logit_model_Pre_14_Freq)



# Average Effect Post-14 

logit_model_Average_post_14 <- glm(first_violent_offence_post_14_common ~ Gender + missing_history + victim_history_sexual + victim_history_physical + Offence_history_NV_Victim + Neglect_history + Offence_history_NV + Witness_history + Offence_history_NV_sexual, data = Combined_Data, family = binomial(link = "logit"))

summary(logit_model_Average_post_14)


# Frequency Effect Post-14

logit_model_Frequency_post_14 <- glm(first_violent_offence_post_14_common ~ Gender + Missing_Frequency + Sexual_Assault_Frequency + Physical_Assault_Frequency + NV_Offence_Frequency_victim + Neglect_Frequency + NV_Offence_Frequency + Witness_Offence_Frequency + NV_Offence_Frequency_Sexual, data = Combined_Data, family = binomial(link = "logit"))

summary(logit_model_Frequency_post_14)


# Testing TVP-Only data meant simply replacing the Combined_Data dataframe with the Analysis_DF which represented the TVP VRU dataset. 

# Analysis_DF <- subset(Analysis_DF, Gender == 2)

# Average Effect

logit_model_Average <- glm(First_Offence_1_to_5 ~ Gender + missing_history + victim_history_sexual + victim_history_physical + Neglect_history + Offence_history_NV + Witness_history + Offence_history_NV_sexual + Offence_history_NV_Victim, data = Analysis_DF, family = binomial(link = "logit"))

summary(logit_model_Average)


# Frequency Effect

logit_model_Frequency <- glm(First_Offence_1_to_5 ~ Gender + Missing_Frequency + Sexual_Assault_Frequency + Physical_Assault_Frequency + Neglect_Frequency + NV_Offence_Frequency + Witness_Offence_Frequency + NV_Offence_Frequency_Sexual + NV_Offence_Frequency_victim, data = Analysis_DF, family = binomial(link = "logit"))

summary(logit_model_Frequency)


# Average Effect Pre-14

logit_model_Pre_14 <- glm(first_violent_offence_pre_14 ~ Gender + missing_history_pre_14 + victim_history_sexual_pre_14 + victim_history_physical_pre_14 + Neglect_history_pre_14 + Offence_history_NV_pre_14 + Witness_history_pre_14 + Offence_history_victim_pre_14 + Offence_history_sexual_pre_14, data = Analysis_DF, family = binomial(link = "logit"))

summary(logit_model_Pre_14)


# Frequency Effect Pre-14

logit_model_Pre_14_Freq <- glm(first_violent_offence_pre_14 ~ Gender + Missing_history_freq_Pre_14 + Sexual_history_freq_Pre_14 + Physical_Assault_Frequency_Pre_14 + Neglect_history_freq_Pre_14 + NV_history_freq_Pre_14 + witness_freq_Pre_14 + nv_victim_history_freq_Pre_14 + sexual_history_freq_Pre_14, data = Analysis_DF, family = binomial(link = "logit"))

summary(logit_model_Pre_14_Freq)


# Average Effect Post-14

logit_model_Average_post_14 <- glm(first_violent_offence_post_14 ~ Gender + missing_history + victim_history_sexual + victim_history_physical + Neglect_history + Offence_history_NV + Witness_history + Offence_history_NV_sexual + Offence_history_NV_Victim, data = Analysis_DF, family = binomial(link = "logit"))

summary(logit_model_Average_post_14)


# Frequency Effect Post-14

logit_model_Frequency_post_14 <- glm(first_violent_offence_post_14 ~ Gender + Missing_Frequency + Sexual_Assault_Frequency + Physical_Assault_Frequency + Neglect_Frequency + NV_Offence_Frequency + Witness_Offence_Frequency + NV_Offence_Frequency_Sexual + NV_Offence_Frequency_victim, data = Analysis_DF, family = binomial(link = "logit"))

summary(logit_model_Frequency_post_14)


# Testing changes in the outcome assumption involved rerunning the entire code shown in 2. Coding the Explanatory and Dependent Variables, so that instead of excluding outcome = 10 via subset_df$Outcome != 10, it only included those with an outcome equal to 5 (summonsed or charged) via subset_df$Outcome == 5
# A snippet is included for reference below:

# Neglect History - Average Effect


calculate_neglect_history <- function(Personal_Occurence_DF_Descriptive) {
  Neglect_history <- numeric(nrow(Personal_Occurence_DF_Descriptive))
  for (i in unique(Personal_Occurence_DF_Descriptive$Person_ID)) {
    subset_df <- Personal_Occurence_DF_Descriptive[Personal_Occurence_DF_Descriptive$Person_ID == i, ]
    
    neglect_dates <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Offence == 8]
    first_offence_dates <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Victim_status == 0 & subset_df$Outcome == 5 & subset_df$Offence %in% 1:5]
    
    if (length(neglect_dates) > 0 && length(first_offence_dates) == 0) {
      Neglect_history[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 1
    } else if (length(neglect_dates) > 0 && length(first_offence_dates) > 0) {
      if (min(neglect_dates) < min(first_offence_dates)) {
        Neglect_history[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 1
      } else {
        Neglect_history[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
      }
    } else {
      Neglect_history[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
    }
  }
  
  return(Neglect_history)
}

Personal_Occurence_DF_Descriptive$Neglect_history <- calculate_neglect_history(Personal_Occurence_DF_Descriptive)


unique_count <- Personal_Occurence_DF_Descriptive %>%
  filter(Neglect_history == 1) %>%  
  summarise(unique_persons = n_distinct(Person_ID))  

print(unique_count)



# Neglect history - Average Effect Pre-14

calculate_neglect_history_pre_14 <- function(Personal_Occurence_DF_Descriptive) {
  Neglect_history_pre_14 <- numeric(nrow(Personal_Occurence_DF_Descriptive))
  
  for (i in unique(Personal_Occurence_DF_Descriptive$Person_ID)) {
    subset_df <- Personal_Occurence_DF_Descriptive[Personal_Occurence_DF_Descriptive$Person_ID == i, ]
    
    subset_df <- subset_df[subset_df$Age_at_incident <= 13, ]
    
    neglect_dates <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Offence == 8]
    first_offence_dates <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Victim_status == 0 & subset_df$Outcome == 5 & subset_df$Offence %in% 1:5]
    
    if (length(neglect_dates) > 0 && length(first_offence_dates) == 0) {
      Neglect_history_pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 1
    } else if (length(neglect_dates) > 0 && length(first_offence_dates) > 0) {
      if (min(neglect_dates) < min(first_offence_dates)) {
        Neglect_history_pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 1
      } else {
        Neglect_history_pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
      }
    } else {
      Neglect_history_pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
    }
  }
  
  return(Neglect_history_pre_14)
}

Personal_Occurence_DF_Descriptive$Neglect_history_pre_14 <- calculate_neglect_history_pre_14(Personal_Occurence_DF_Descriptive)


unique_count <- Personal_Occurence_DF_Descriptive %>%
  filter(Neglect_history_pre_14 == 1) %>%  
  summarise(unique_persons = n_distinct(Person_ID))  

print(unique_count)



# Neglect History - Frequency Effect

calculate_neglect_history_freq <- function(Personal_Occurence_DF_Descriptive) {
  Neglect_Frequency <- numeric(nrow(Personal_Occurence_DF_Descriptive))
  for (i in unique(Personal_Occurence_DF_Descriptive$Person_ID)) {
    subset_df <- Personal_Occurence_DF_Descriptive[Personal_Occurence_DF_Descriptive$Person_ID == i, ]
    
    if (any(subset_df$Neglect_history == 1)) {
      neglect_dates_freq <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Offence == 8]
      
      first_offence_dates <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Victim_status == 0 & subset_df$Outcome == 5 & subset_df$Offence %in% 1:5]
      
      if (length(neglect_dates_freq) > 0 && length(first_offence_dates) == 0) {
        Neglect_Frequency[Personal_Occurence_DF_Descriptive$Person_ID == i] <- length(neglect_dates_freq)
      } 
      else if (length(neglect_dates_freq) > 0 && length(first_offence_dates) > 0) {
        first_offence_date <- min(first_offence_dates, na.rm = TRUE)
        
        Neglect_before_first_offence <- sum(neglect_dates_freq < first_offence_date)
        
        Neglect_Frequency[Personal_Occurence_DF_Descriptive$Person_ID == i] <- Neglect_before_first_offence
      }
      else {
        Neglect_Frequency[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
      }
    }
  }
  
  return(Neglect_Frequency)
}

Personal_Occurence_DF_Descriptive$Neglect_Frequency <- calculate_neglect_history_freq(Personal_Occurence_DF_Descriptive)


unique_count <- Personal_Occurence_DF_Descriptive %>%
  filter(Neglect_Frequency > 0) %>%  
  summarise(unique_persons = n_distinct(Person_ID))  

print(unique_count)



# Neglect History - Frequency Effect Pre-14:


calculate_neglect_history_freq_Pre_14 <- function(Personal_Occurence_DF_Descriptive) {
  Neglect_history_freq_Pre_14 <- numeric(nrow(Personal_Occurence_DF_Descriptive))
  for (i in unique(Personal_Occurence_DF_Descriptive$Person_ID)) {
    subset_df <- Personal_Occurence_DF_Descriptive[Personal_Occurence_DF_Descriptive$Person_ID == i, ]
    if (subset_df$Neglect_history_pre_14[1] == 1) {
      neglect_dates_freq_Pre_14 <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Offence== 8]
      
      first_offence_dates <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Victim_status == 0 & subset_df$Outcome == 5 & subset_df$Offence %in% 1:5]
      
      if (length(neglect_dates_freq_Pre_14) > 0 && length(first_offence_dates) == 0) {
        Neglect_history_freq_Pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- length(neglect_dates_freq_Pre_14)
      } 
      else if (length(neglect_dates_freq_Pre_14) > 0 && length(first_offence_dates) > 0) {
        first_offence_date <- min(first_offence_dates, na.rm = TRUE)
        
        Neglect_before_first_offence <- sum(neglect_dates_freq_Pre_14 < first_offence_date)
        
        Neglect_history_freq_Pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- Neglect_before_first_offence
      }
      else {
        Neglect_history_freq_Pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
      }
    }
  }
  
  return(Neglect_history_freq_Pre_14)
}

Personal_Occurence_DF_Descriptive$Neglect_history_freq_Pre_14 <- calculate_neglect_history_freq_Pre_14(Personal_Occurence_DF_Descriptive)


unique_count <- Personal_Occurence_DF_Descriptive %>%
  filter(Neglect_history_freq_Pre_14 > 0) %>%  
  summarise(unique_persons = n_distinct(Person_ID))  

print(unique_count)


