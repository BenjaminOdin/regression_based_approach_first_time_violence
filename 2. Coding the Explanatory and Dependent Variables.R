# Create basic markers for exposures

# Victim history

Personal_Occurence_DF_Descriptive <- with(Personal_Occurence_DF_Descriptive, Personal_Occurence_DF_Descriptive[order(Person_ID), ])

calculate_victim_history <- function(df) {
  Victim_marker <- numeric(nrow(df))
  unique_persons <- unique(df$Person_ID)
  
  for (i in unique_persons) {
    victim_condition <- df$Victim_status == 1 & df$Offence != 8 & df$Person_ID == i
    Victim_marker[victim_condition] <- 1
  }
  
  return(Victim_marker)
}

# Witness history

Personal_Occurence_DF_Descriptive <- with(Personal_Occurence_DF_Descriptive, Personal_Occurence_DF_Descriptive[order(Person_ID), ])

calculate_witness_history <- function(df) {
  Witness_marker <- numeric(nrow(df))
  unique_persons <- unique(df$Person_ID)
  
  for (i in unique_persons) {
    witness_condition <- df$Witness == 1 & df$Offence %in% 1:5 & df$Person_ID == i
    Witness_marker[witness_condition] <- 1
  }
  
  return(Witness_marker)
}

Personal_Occurence_DF_Descriptive$Victim_ID <- calculate_victim_history(Personal_Occurence_DF_Descriptive)


# Neglect history

Personal_Occurence_DF_Descriptive <- with(Personal_Occurence_DF_Descriptive, Personal_Occurence_DF_Descriptive[order(Person_ID), ])

calculate_neglect_history <- function(df) {
  Neglect_marker <- numeric(nrow(df))
  unique_persons <- unique(df$Person_ID)
  
  for (i in unique_persons) {
    neglect_condition <- df$Offence == 8 & df$Person_ID == i
    Neglect_marker[neglect_condition] <- 1
  }
  
  return(Neglect_marker)
}

Personal_Occurence_DF_Descriptive$Neglect_ID <- calculate_neglect_history(Personal_Occurence_DF_Descriptive)


# Non-violent offence history

calculate_NV_offence_history <- function(df) {
  Offence_NV_marker <- numeric(nrow(df))
  unique_persons <- unique(df$Person_ID)
  
  for (i in unique_persons) {
    offence_NV_condition <- df$Victim_status == 0 & (df$Offence %in% 6:7 | df$Offence %in% 9:16) & df$Person_ID == i
    Offence_NV_marker[offence_condition] <- 1
  }
  
  return(Offence_NV_marker)
}


# Missing event history

calculate_missing_history <- function(df) {
  Missing_marker <- numeric(nrow(df))
  unique_persons <- unique(df$Person_ID)
  
  for (i in unique_persons) {
    missing_condition <- df$Missing_Incident == "Missing Incident" & df$Offence == 17 & df$Person_ID == i
    Missing_marker[missing_condition] <- 1
  }
  
  return(Missing_marker)
}

Personal_Occurence_DF_Descriptive$Missing_ID <- calculate_missing_history(Personal_Occurence_DF_Descriptive)

n_distinct(Personal_Occurence_DF_Descriptive$Person_ID)



unique_count <- Personal_Occurence_DF_Descriptive %>%
  filter(Missing_ID == 1) %>%
  group_by(Gender) %>%
  summarise(unique_persons = n_distinct(Person_ID))  

print(unique_count)


# Code the explanatory variables with average effects and frequency effects by stage of devleopment accounted for (only need to differ for early adolescence)

# Average Effect Witness History

calculate_witness_history <- function(Personal_Occurence_DF_Descriptive) {
  Witness_history <- numeric(nrow(Personal_Occurence_DF_Descriptive))
  for (i in unique(Personal_Occurence_DF_Descriptive$Person_ID)) {
    subset_df <- Personal_Occurence_DF_Descriptive[Personal_Occurence_DF_Descriptive$Person_ID == i, ]
    
    witness_dates <- subset_df$Time_of_incident[subset_df$Witness == 1 & subset_df$Offence %in% 1:5]
    first_offence_dates <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Victim_status == 0 & subset_df$Outcome != 10 & subset_df$Offence %in% 1:5]
    
    if (length(witness_dates) > 0 && length(first_offence_dates) == 0) {
      Witness_history[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 1
    } else if (length(witness_dates) > 0 && length(first_offence_dates) > 0) {
      if (min(witness_dates) < min(first_offence_dates)) {
        Witness_history[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 1
      } else {
        Witness_history[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
      }
    } else {
      Witness_history[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
    }
  }
  
  return(Witness_history)
}

Personal_Occurence_DF_Descriptive$Witness_history <- calculate_witness_history(Personal_Occurence_DF_Descriptive)


unique_count <- Personal_Occurence_DF_Descriptive %>%
  filter(Witness_history == 1) %>%
  group_by(Gender) %>%
  summarise(unique_persons = n_distinct(Person_ID))  

print(unique_count)


# Average Effect Pre-14 Witness History

calculate_witness_pre_14 <- function(Personal_Occurence_DF_Descriptive) {
  Witness_history_pre_14 <- numeric(nrow(Personal_Occurence_DF_Descriptive))
  
  for (i in unique(Personal_Occurence_DF_Descriptive$Person_ID)) {
    subset_df <- Personal_Occurence_DF_Descriptive[Personal_Occurence_DF_Descriptive$Person_ID == i, ]
    
    subset_df <- subset_df[subset_df$Age_at_incident <= 13, ]
    
    witness_dates <- subset_df$Time_of_incident[subset_df$Witness == 1 & subset_df$Offence %in% 1:5]
    first_offence_dates <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Victim_status == 0 & subset_df$Outcome != 10 & subset_df$Offence %in% 1:5]
    
    if (length(witness_dates) > 0 && length(first_offence_dates) == 0) {
      Witness_history_pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 1
    } else if (length(witness_dates) > 0 && length(first_offence_dates) > 0) {
      if (min(witness_dates) < min(first_offence_dates)) {
        Witness_history_pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 1
      } else {
        Witness_history_pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
      }
    } else {
      Witness_history_pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
    }
  }
  
  return(Witness_history_pre_14)
}

Personal_Occurence_DF_Descriptive$Witness_history_pre_14 <- calculate_witness_pre_14(Personal_Occurence_DF_Descriptive)


unique_count <- Personal_Occurence_DF_Descriptive %>%
  filter(Witness_history_pre_14 == 1) %>%
  group_by(Gender) %>%
  summarise(unique_persons = n_distinct(Person_ID))  

print(unique_count)


# Frequency Effect Witness History

calculate_witness_freq <- function(Personal_Occurence_DF_Descriptive) {
  Witness_Offence_Frequency <- numeric(nrow(Personal_Occurence_DF_Descriptive))
  for (i in unique(Personal_Occurence_DF_Descriptive$Person_ID)) {
    subset_df <- Personal_Occurence_DF_Descriptive[Personal_Occurence_DF_Descriptive$Person_ID == i, ]
    
    if (any(subset_df$Witness_history == 1)) {
      witness_times <- subset_df$Time_of_incident[subset_df$Witness == 1 & subset_df$Offence %in% 1:5]
      
      first_offence_dates <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Victim_status == 0 & subset_df$Outcome != 10 & subset_df$Offence %in% 1:5]
      
      if (length(witness_times) > 0 && length(first_offence_dates) == 0) {
        Witness_Offence_Frequency[Personal_Occurence_DF_Descriptive$Person_ID == i] <- length(witness_times)
      } 
      else if (length(witness_times) > 0 && length(first_offence_dates) > 0) {
        first_offence_date <- min(first_offence_dates, na.rm = TRUE)
        
        Witness_before_first_offence <- sum(witness_times < first_offence_date)
        
        Witness_Offence_Frequency[Personal_Occurence_DF_Descriptive$Person_ID == i] <- Witness_before_first_offence
      }
      else {
        Witness_Offence_Frequency[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
      }
    }
  }
  
  return(Witness_Offence_Frequency)
}

Personal_Occurence_DF_Descriptive$Witness_Offence_Frequency <- calculate_witness_freq(Personal_Occurence_DF_Descriptive)


unique_count <- Personal_Occurence_DF_Descriptive %>%
  filter(Witness_Offence_Frequency > 0) %>%  
  summarise(unique_persons = n_distinct(Person_ID))  

print(unique_count)


# Frequency Effect Witness History Pre-14

calculate_witness_freq_Pre_14 <- function(Personal_Occurence_DF_Descriptive) {
  witness_freq_Pre_14 <- numeric(nrow(Personal_Occurence_DF_Descriptive))
  for (i in unique(Personal_Occurence_DF_Descriptive$Person_ID)) {
    subset_df <- Personal_Occurence_DF_Descriptive[Personal_Occurence_DF_Descriptive$Person_ID == i, ]
    if (subset_df$Witness_history_pre_14[1] == 1) {
      subset_df <- subset_df[subset_df$Age_at_incident <= 13, ]
      witness_times <- subset_df$Time_of_incident[subset_df$Witness == 1 & subset_df$Offence %in% 1:5]
      
      first_offence_dates <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Victim_status == 0 & subset_df$Outcome != 10 & subset_df$Offence %in% 1:5]
      
      if (length(witness_times) > 0 && length(first_offence_dates) == 0) {
        witness_freq_Pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- length(witness_times)
      } 
      else if (length(witness_times) > 0 && length(first_offence_dates) > 0) {
        first_offence_date <- min(first_offence_dates, na.rm = TRUE)
        
        Witness_before_first_offence <- sum(witness_times < first_offence_date)
        
        witness_freq_Pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- Witness_before_first_offence
      }
      else {
        witness_freq_Pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
      }
    }
  }
  
  return(witness_freq_Pre_14)
}

Personal_Occurence_DF_Descriptive$witness_freq_Pre_14 <- calculate_witness_freq_Pre_14(Personal_Occurence_DF_Descriptive)


unique_count <- Personal_Occurence_DF_Descriptive %>%
  filter(witness_freq_Pre_14 > 0) %>%  
  summarise(unique_persons = n_distinct(Person_ID))  

print(unique_count)




# Average Effect Neglect History:


calculate_neglect_history <- function(Personal_Occurence_DF_Descriptive) {
  Neglect_history <- numeric(nrow(Personal_Occurence_DF_Descriptive))
  for (i in unique(Personal_Occurence_DF_Descriptive$Person_ID)) {
    subset_df <- Personal_Occurence_DF_Descriptive[Personal_Occurence_DF_Descriptive$Person_ID == i, ]
    
    neglect_dates <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Offence == 8]
    first_offence_dates <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Victim_status == 0 & subset_df$Outcome != 10 & subset_df$Offence %in% 1:5]
    
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
  group_by(Gender) %>%
  summarise(unique_persons = n_distinct(Person_ID))  

print(unique_count)




# Average Effect Neglect History Pre-14

calculate_neglect_history_pre_14 <- function(Personal_Occurence_DF_Descriptive) {
  Neglect_history_pre_14 <- numeric(nrow(Personal_Occurence_DF_Descriptive))
  
  for (i in unique(Personal_Occurence_DF_Descriptive$Person_ID)) {
    subset_df <- Personal_Occurence_DF_Descriptive[Personal_Occurence_DF_Descriptive$Person_ID == i, ]
    
    subset_df <- subset_df[subset_df$Age_at_incident <= 13, ]
    
    neglect_dates <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Offence == 8]
    first_offence_dates <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Victim_status == 0 & subset_df$Outcome != 10 & subset_df$Offence %in% 1:5]
    
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
  group_by(Gender) %>%
  summarise(unique_persons = n_distinct(Person_ID))  

print(unique_count)



# Frequency Effect Neglect History:

calculate_neglect_history_freq <- function(Personal_Occurence_DF_Descriptive) {
  Neglect_Frequency <- numeric(nrow(Personal_Occurence_DF_Descriptive))
  for (i in unique(Personal_Occurence_DF_Descriptive$Person_ID)) {
    subset_df <- Personal_Occurence_DF_Descriptive[Personal_Occurence_DF_Descriptive$Person_ID == i, ]
    
    if (any(subset_df$Neglect_history == 1)) {
      neglect_dates_freq <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Offence == 8]
      
      first_offence_dates <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Victim_status == 0 & subset_df$Outcome != 10 & subset_df$Offence %in% 1:5]
      
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



# Frequency Effect Neglect History Pre-14:


calculate_neglect_history_freq_Pre_14 <- function(Personal_Occurence_DF_Descriptive) {
  Neglect_history_freq_Pre_14 <- numeric(nrow(Personal_Occurence_DF_Descriptive))
  for (i in unique(Personal_Occurence_DF_Descriptive$Person_ID)) {
    subset_df <- Personal_Occurence_DF_Descriptive[Personal_Occurence_DF_Descriptive$Person_ID == i, ]
    if (subset_df$Neglect_history_pre_14[1] == 1) {
      subset_df <- subset_df[subset_df$Age_at_incident <= 13, ]
      neglect_dates_freq_Pre_14 <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Offence== 8]
      
      first_offence_dates <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Victim_status == 0 & subset_df$Outcome != 10 & subset_df$Offence %in% 1:5]
      
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




################################################# Sexual Victimisation 


# Average Effect Sexual Victim:

calculate_victim_history_sexual <- function(Personal_Occurence_DF_Descriptive) {
  victim_history_sexual <- numeric(nrow(Personal_Occurence_DF_Descriptive))
  for (i in unique(Personal_Occurence_DF_Descriptive$Person_ID)) {
    subset_df <- Personal_Occurence_DF_Descriptive[Personal_Occurence_DF_Descriptive$Person_ID == i, ]
    
    victim_sexual_dates <- subset_df$Time_of_incident[subset_df$Victim_status == 1 & subset_df$Offence %in% 6:7]
    first_offence_dates <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Victim_status == 0 & subset_df$Outcome != 10 & subset_df$Offence %in% 1:5]
    
    if (length(victim_sexual_dates) > 0 && length(first_offence_dates) == 0) {
      victim_history_sexual[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 1
    } else if (length(victim_sexual_dates) > 0 && length(first_offence_dates) > 0) {
      if (min(victim_sexual_dates) < min(first_offence_dates)) {
        victim_history_sexual[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 1
      } else {
        victim_history_sexual[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
      }
    } else {
      victim_history_sexual[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
    }
  }
  
  return(victim_history_sexual)
}

Personal_Occurence_DF_Descriptive$victim_history_sexual <- calculate_victim_history_sexual(Personal_Occurence_DF_Descriptive)


unique_count <- Personal_Occurence_DF_Descriptive %>%
  filter(victim_history_sexual == 1) %>%
  group_by(Gender) %>%
  summarise(unique_persons = n_distinct(Person_ID))  

print(unique_count)


# Average Effect Sexual Victim Pre-14

calculate_victim_history_sexual_pre_14 <- function(Personal_Occurence_DF_Descriptive) {
  victim_history_sexual_pre_14 <- numeric(nrow(Personal_Occurence_DF_Descriptive))
  
  for (i in unique(Personal_Occurence_DF_Descriptive$Person_ID)) {
    subset_df <- Personal_Occurence_DF_Descriptive[Personal_Occurence_DF_Descriptive$Person_ID == i, ]
    
    subset_df <- subset_df[subset_df$Age_at_incident <= 13, ]
    
    victim_sexual_dates <- subset_df$Time_of_incident[subset_df$Victim_status == 1 & subset_df$Offence %in% 6:7]
    first_offence_dates <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Victim_status == 0 & subset_df$Outcome != 10 & subset_df$Offence %in% 1:5]
    
    if (length(victim_sexual_dates) > 0 && length(first_offence_dates) == 0) {
      victim_history_sexual_pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 1
    } else if (length(victim_sexual_dates) > 0 && length(first_offence_dates) > 0) {
      if (min(victim_sexual_dates) < min(first_offence_dates)) {
        victim_history_sexual_pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 1
      } else {
        victim_history_sexual_pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
      }
    } else {
      victim_history_sexual_pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
    }
  }
  
  return(victim_history_sexual_pre_14)
}

Personal_Occurence_DF_Descriptive$victim_history_sexual_pre_14 <- calculate_victim_history_sexual_pre_14(Personal_Occurence_DF_Descriptive)


unique_count <- Personal_Occurence_DF_Descriptive %>%
  filter(victim_history_sexual_pre_14 == 1) %>%
  group_by(Gender) %>%
  summarise(unique_persons = n_distinct(Person_ID))  

print(unique_count)



# Frequency Effect Sexual Victim:

calculate_sexual_history_freq <- function(Personal_Occurence_DF_Descriptive) {
  Sexual_Assault_Frequency <- numeric(nrow(Personal_Occurence_DF_Descriptive))
  for (i in unique(Personal_Occurence_DF_Descriptive$Person_ID)) {
    subset_df <- Personal_Occurence_DF_Descriptive[Personal_Occurence_DF_Descriptive$Person_ID == i, ]
    
    if (any(subset_df$victim_history_sexual == 1)) {
      sexual_dates_freq <- subset_df$Time_of_incident[subset_df$Victim_status == 1 & subset_df$Offence %in% 6:7]
      
      first_offence_dates <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Victim_status == 0 & subset_df$Outcome != 10 & subset_df$Offence %in% 1:5]
      
      if (length(sexual_dates_freq) > 0 && length(first_offence_dates) == 0) {
        Sexual_Assault_Frequency[Personal_Occurence_DF_Descriptive$Person_ID == i] <- length(sexual_dates_freq)
      } 
      else if (length(sexual_dates_freq) > 0 && length(first_offence_dates) > 0) {
        first_offence_date <- min(first_offence_dates, na.rm = TRUE)
        
        Sexual_before_first_offence <- sum(sexual_dates_freq < first_offence_date)
        
        Sexual_Assault_Frequency[Personal_Occurence_DF_Descriptive$Person_ID == i] <- Sexual_before_first_offence
      }
      else {
        Sexual_Assault_Frequency[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
      }
    }
  }
  
  return(Sexual_Assault_Frequency)
}

Personal_Occurence_DF_Descriptive$Sexual_Assault_Frequency <- calculate_sexual_history_freq(Personal_Occurence_DF_Descriptive)


unique_count <- Personal_Occurence_DF_Descriptive %>%
  filter(Sexual_Assault_Frequency > 0) %>%  
  summarise(unique_persons = n_distinct(Person_ID))  

print(unique_count)



# Frequency Effect Sexual Victim Pre-14:

calculate_sexual_history_freq_Pre_14 <- function(Personal_Occurence_DF_Descriptive) {
  Sexual_history_freq_Pre_14 <- numeric(nrow(Personal_Occurence_DF_Descriptive))
  for (i in unique(Personal_Occurence_DF_Descriptive$Person_ID)) {
    subset_df <- Personal_Occurence_DF_Descriptive[Personal_Occurence_DF_Descriptive$Person_ID == i, ]
    if (subset_df$victim_history_sexual_pre_14[1] == 1) {
      subset_df <- subset_df[subset_df$Age_at_incident <= 13, ]
      sexual_dates_freq_pre_14 <- subset_df$Time_of_incident[subset_df$Victim_status == 1 & subset_df$Offence %in% 6:7]
      
      first_offence_dates <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Victim_status == 0 & subset_df$Outcome != 10 & subset_df$Offence %in% 1:5]
      
      if (length(sexual_dates_freq_pre_14) > 0 && length(first_offence_dates) == 0) {
        Sexual_history_freq_Pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- length(sexual_dates_freq_pre_14)
      } 
      else if (length(sexual_dates_freq_pre_14) > 0 && length(first_offence_dates) > 0) {
        first_offence_date <- min(first_offence_dates, na.rm = TRUE)
        
        Sexual_before_first_offence <- sum(sexual_dates_freq_pre_14 < first_offence_date)
        
        Sexual_history_freq_Pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- Sexual_before_first_offence
      }
      else {
        Sexual_history_freq_Pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
      }
    }
  }
  
  return(Sexual_history_freq_Pre_14)
}

Personal_Occurence_DF_Descriptive$Sexual_history_freq_Pre_14 <- calculate_sexual_history_freq_Pre_14(Personal_Occurence_DF_Descriptive)


unique_count <- Personal_Occurence_DF_Descriptive %>%
  filter(Sexual_history_freq_Pre_14 > 0) %>%  
  summarise(unique_persons = n_distinct(Person_ID))  

print(unique_count)



# Average Effect Non-Violent Offence History:

calculate_offence_history_NV <- function(Personal_Occurence_DF_Descriptive) {
  Offence_history_NV <- numeric(nrow(Personal_Occurence_DF_Descriptive))
  for (i in unique(Personal_Occurence_DF_Descriptive$Person_ID)) {
    subset_df <- Personal_Occurence_DF_Descriptive[Personal_Occurence_DF_Descriptive$Person_ID == i, ]
    
    Offence_history_NV_dates <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Victim_status == 0 & (subset_df$Offence %in% 9:16)]
    first_offence_dates <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Victim_status == 0 & subset_df$Outcome != 10 & subset_df$Offence %in% 1:5]
    
    if (length(Offence_history_NV_dates) > 0 && length(first_offence_dates) == 0) {
      Offence_history_NV[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 1
    } else if (length(Offence_history_NV_dates) > 0 && length(first_offence_dates) > 0) {
      if (min(Offence_history_NV_dates) < min(first_offence_dates)) {
        Offence_history_NV[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 1
      } else {
        Offence_history_NV[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
      }
    } else {
      Offence_history_NV[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
    }
  }
  
  return(Offence_history_NV)
}

Personal_Occurence_DF_Descriptive$Offence_history_NV <- calculate_offence_history_NV(Personal_Occurence_DF_Descriptive)



unique_count <- Personal_Occurence_DF_Descriptive %>%
  filter(Offence_history_NV == 1) %>%
  group_by(Gender) %>%
  summarise(unique_persons = n_distinct(Person_ID))  

print(unique_count)


# Average Effect Non-Violent Offence History Pre-14

calculate_offence_history_NV_pre_14 <- function(Personal_Occurence_DF_Descriptive) {
  Offence_history_NV_pre_14 <- numeric(nrow(Personal_Occurence_DF_Descriptive))
  
  for (i in unique(Personal_Occurence_DF_Descriptive$Person_ID)) {
    subset_df <- Personal_Occurence_DF_Descriptive[Personal_Occurence_DF_Descriptive$Person_ID == i, ]
    
    subset_df <- subset_df[subset_df$Age_at_incident <= 13, ]
    
    Offence_history_NV_dates <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Victim_status == 0 & (subset_df$Offence %in% 9:16)]
    first_offence_dates <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Victim_status == 0 & subset_df$Outcome != 10 & subset_df$Offence %in% 1:5]
    
    if (length(Offence_history_NV_dates) > 0 && length(first_offence_dates) == 0) {
      Offence_history_NV_pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 1
    } else if (length(Offence_history_NV_dates) > 0 && length(first_offence_dates) > 0) {
      if (min(Offence_history_NV_dates) < min(first_offence_dates)) {
        Offence_history_NV_pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 1
      } else {
        Offence_history_NV_pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
      }
    } else {
      Offence_history_NV_pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
    }
  }
  
  return(Offence_history_NV_pre_14)
}

Personal_Occurence_DF_Descriptive$Offence_history_NV_pre_14 <- calculate_offence_history_NV_pre_14(Personal_Occurence_DF_Descriptive)


unique_count <- Personal_Occurence_DF_Descriptive %>%
  filter(Offence_history_NV_pre_14 == 1) %>%
  group_by(Gender) %>%
  summarise(unique_persons = n_distinct(Person_ID))  

print(unique_count)


# Frequency Effect Non-Violent Offence History:

calculate_nv_history_freq <- function(Personal_Occurence_DF_Descriptive) {
  NV_Offence_Frequency <- numeric(nrow(Personal_Occurence_DF_Descriptive))
  for (i in unique(Personal_Occurence_DF_Descriptive$Person_ID)) {
    subset_df <- Personal_Occurence_DF_Descriptive[Personal_Occurence_DF_Descriptive$Person_ID == i, ]
    
    if (any(subset_df$Offence_history_NV == 1)) {
      NV_dates_freq <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Victim_status == 0 & (subset_df$Offence %in% 9:16)]
      
      first_offence_dates <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Victim_status == 0 & subset_df$Outcome != 10 & subset_df$Offence %in% 1:5]
      
      if (length(NV_dates_freq) > 0 && length(first_offence_dates) == 0) {
        NV_Offence_Frequency[Personal_Occurence_DF_Descriptive$Person_ID == i] <- length(NV_dates_freq)
      } 
      else if (length(NV_dates_freq) > 0 && length(first_offence_dates) > 0) {
        first_offence_date <- min(first_offence_dates, na.rm = TRUE)
        
        NV_before_first_offence <- sum(NV_dates_freq < first_offence_date)
        
        NV_Offence_Frequency[Personal_Occurence_DF_Descriptive$Person_ID == i] <- NV_before_first_offence
      }
      else {
        NV_Offence_Frequency[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
      }
    }
  }
  
  return(NV_Offence_Frequency)
}

Personal_Occurence_DF_Descriptive$NV_Offence_Frequency <- calculate_nv_history_freq(Personal_Occurence_DF_Descriptive)


unique_count <- Personal_Occurence_DF_Descriptive %>%
  filter(NV_Offence_Frequency > 0) %>%  
  summarise(unique_persons = n_distinct(Person_ID))  

print(unique_count)



# Frequency Effect Non-Violent Offence History Pre-14:

calculate_nv_history_freq_Pre_14 <- function(Personal_Occurence_DF_Descriptive) {
  NV_history_freq_Pre_14 <- numeric(nrow(Personal_Occurence_DF_Descriptive))
  for (i in unique(Personal_Occurence_DF_Descriptive$Person_ID)) {
    subset_df <- Personal_Occurence_DF_Descriptive[Personal_Occurence_DF_Descriptive$Person_ID == i, ]
    if (subset_df$Offence_history_NV_pre_14[1] == 1) {
      subset_df <- subset_df[subset_df$Age_at_incident <= 13, ]
      NV_dates_freq_pre_14 <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Victim_status == 0 & (subset_df$Offence %in% 9:16)]
      
      first_offence_dates <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Victim_status == 0 & subset_df$Outcome != 10 & subset_df$Offence %in% 1:5]
      
      if (length(NV_dates_freq_pre_14) > 0 && length(first_offence_dates) == 0) {
        NV_history_freq_Pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- length(NV_dates_freq_pre_14)
      } 
      else if (length(NV_dates_freq_pre_14) > 0 && length(first_offence_dates) > 0) {
        first_offence_date <- min(first_offence_dates, na.rm = TRUE)
        
        NV_before_first_offence <- sum(NV_dates_freq_pre_14 < first_offence_date)
        
        NV_history_freq_Pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- NV_before_first_offence
      }
      else {
        NV_history_freq_Pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
      }
    }
  }
  
  return(NV_history_freq_Pre_14)
}

Personal_Occurence_DF_Descriptive$NV_history_freq_Pre_14 <- calculate_nv_history_freq_Pre_14(Personal_Occurence_DF_Descriptive)


unique_count <- Personal_Occurence_DF_Descriptive %>%
  filter(NV_history_freq_Pre_14 > 0) %>%  
  summarise(unique_persons = n_distinct(Person_ID))  

print(unique_count)




# Average Effect Missing History:

calculate_missing_history <- function(Personal_Occurence_DF_Descriptive) {
  missing_history <- numeric(nrow(Personal_Occurence_DF_Descriptive))
  for (i in unique(Personal_Occurence_DF_Descriptive$Person_ID)) {
    subset_df <- Personal_Occurence_DF_Descriptive[Personal_Occurence_DF_Descriptive$Person_ID == i, ]
    
    missing_dates <- subset_df$Time_of_incident[subset_df$Missing_ID == 1]
    first_offence_dates <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Victim_status == 0 & subset_df$Outcome != 10 & subset_df$Offence %in% 1:5]
    
    if (length(missing_dates) > 0 && length(first_offence_dates) == 0) {
      missing_history[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 1
    } else if (length(missing_dates) > 0 && length(first_offence_dates) > 0) {
      if (min(missing_dates) < min(first_offence_dates)) {
        missing_history[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 1
      } else {
        missing_history[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
      }
    } else {
      missing_history[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
    }
  }
  
  return(missing_history)
}

Personal_Occurence_DF_Descriptive$missing_history <- calculate_missing_history(Personal_Occurence_DF_Descriptive)


unique_count <- Personal_Occurence_DF_Descriptive %>%
  filter(missing_history == 1) %>%
  group_by(Gender) %>%
  summarise(unique_persons = n_distinct(Person_ID))  

print(unique_count)



# Average Effect Missing History pre-14

calculate_missing_history_Pre_14 <- function(Personal_Occurence_DF_Descriptive) {
  missing_history_pre_14 <- numeric(nrow(Personal_Occurence_DF_Descriptive))
  
  for (i in unique(Personal_Occurence_DF_Descriptive$Person_ID)) {
    subset_df <- Personal_Occurence_DF_Descriptive[Personal_Occurence_DF_Descriptive$Person_ID == i, ]
    
    subset_df <- subset_df[subset_df$Age_at_incident <= 13, ]
    
    missing_dates <- subset_df$Time_of_incident[subset_df$Missing_ID == 1]
    first_offence_dates <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Victim_status == 0 & subset_df$Outcome != 10 & subset_df$Offence %in% 1:5]
    
    if (length(missing_dates) > 0 && length(first_offence_dates) == 0) {
      missing_history_pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 1
    } else if (length(missing_dates) > 0 && length(first_offence_dates) > 0) {
      if (min(missing_dates) < min(first_offence_dates)) {
        missing_history_pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 1
      } else {
        missing_history_pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
      }
    } else {
      missing_history_pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
    }
  }
  
  return(missing_history_pre_14)
}

Personal_Occurence_DF_Descriptive$missing_history_pre_14 <- calculate_missing_history_Pre_14(Personal_Occurence_DF_Descriptive)


unique_count <- Personal_Occurence_DF_Descriptive %>%
  filter(missing_history_pre_14 == 1) %>%
  group_by(Gender) %>%
  summarise(unique_persons = n_distinct(Person_ID))  

print(unique_count)



# Frequency Effect Missing History:

calculate_missing_history_freq <- function(Personal_Occurence_DF_Descriptive) {
  Missing_Frequency <- numeric(nrow(Personal_Occurence_DF_Descriptive))
  for (i in unique(Personal_Occurence_DF_Descriptive$Person_ID)) {
    subset_df <- Personal_Occurence_DF_Descriptive[Personal_Occurence_DF_Descriptive$Person_ID == i, ]
    
    if (any(subset_df$missing_history == 1)) {
      Missing_dates_freq <- subset_df$Time_of_incident[subset_df$Missing_ID == 1]
      
      first_offence_dates <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Victim_status == 0 & subset_df$Outcome != 10 & subset_df$Offence %in% 1:5]
      
      if (length(Missing_dates_freq) > 0 && length(first_offence_dates) == 0) {
        Missing_Frequency[Personal_Occurence_DF_Descriptive$Person_ID == i] <- length(Missing_dates_freq)
      } 
      else if (length(Missing_dates_freq) > 0 && length(first_offence_dates) > 0) {
        first_offence_date <- min(first_offence_dates, na.rm = TRUE)
        
        Missing_before_first_offence <- sum(Missing_dates_freq < first_offence_date)
        
        Missing_Frequency[Personal_Occurence_DF_Descriptive$Person_ID == i] <- Missing_before_first_offence
      }
      else {
        Missing_Frequency[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
      }
    }
  }
  
  return(Missing_Frequency)
}

Personal_Occurence_DF_Descriptive$Missing_Frequency <- calculate_missing_history_freq(Personal_Occurence_DF_Descriptive)


unique_count <- Personal_Occurence_DF_Descriptive %>%
  filter(Missing_Frequency > 0) %>%  
  summarise(unique_persons = n_distinct(Person_ID))  

print(unique_count)


# Frequency Effect Missing History Pre-14:

calculate_missing_history_freq_Pre_14 <- function(Personal_Occurence_DF_Descriptive) {
  Missing_history_freq_Pre_14 <- numeric(nrow(Personal_Occurence_DF_Descriptive))
  for (i in unique(Personal_Occurence_DF_Descriptive$Person_ID)) {
    subset_df <- Personal_Occurence_DF_Descriptive[Personal_Occurence_DF_Descriptive$Person_ID == i, ]
    if (subset_df$missing_history_pre_14[1] == 1) {
      subset_df <- subset_df[subset_df$Age_at_incident <= 13, ]
      Missing_dates_freq_Pre_14 <- subset_df$Time_of_incident[subset_df$Missing_ID == 1]
      
      first_offence_dates <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Victim_status == 0 & subset_df$Outcome != 10 & subset_df$Offence %in% 1:5]
      
      if (length(Missing_dates_freq_Pre_14) > 0 && length(first_offence_dates) == 0) {
        Missing_history_freq_Pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- length(Missing_dates_freq_Pre_14)
      } 
      else if (length(Missing_dates_freq_Pre_14) > 0 && length(first_offence_dates) > 0) {
        first_offence_date <- min(first_offence_dates, na.rm = TRUE)
        
        Missing_before_first_offence <- sum(Missing_dates_freq_Pre_14 < first_offence_date)
        
        Missing_history_freq_Pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- Missing_before_first_offence
      }
      else {
        Missing_history_freq_Pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
      }
    }
  }
  
  return(Missing_history_freq_Pre_14)
}

Personal_Occurence_DF_Descriptive$Missing_history_freq_Pre_14 <- calculate_missing_history_freq_Pre_14(Personal_Occurence_DF_Descriptive)


unique_count <- Personal_Occurence_DF_Descriptive %>%
  filter(Missing_history_freq_Pre_14 > 0) %>%  
  summarise(unique_persons = n_distinct(Person_ID))  

print(unique_count)



# Average Effect Physical Victim:

calculate_victim_history_physical <- function(Personal_Occurence_DF_Descriptive) {
  victim_history_physical <- numeric(nrow(Personal_Occurence_DF_Descriptive))
  
  for (i in unique(Personal_Occurence_DF_Descriptive$Person_ID)) {
    subset_df <- Personal_Occurence_DF_Descriptive[Personal_Occurence_DF_Descriptive$Person_ID == i, ]
    
    victim_physical_dates <- subset_df$Time_of_incident[subset_df$Victim_status == 1 & subset_df$Offence %in% 1:5]
    
    first_offence_dates <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Victim_status == 0 & subset_df$Outcome != 10 & subset_df$Offence %in% 1:5]
    
    if (length(victim_physical_dates) > 0 && length(first_offence_dates) == 0) {
      victim_history_physical[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 1
    } else if (length(victim_physical_dates) > 0 && length(first_offence_dates) > 0) {
      if (min(victim_physical_dates) < min(first_offence_dates)) {
        victim_history_physical[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 1
      } else {
        victim_history_physical[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
      }
    } else {
      victim_history_physical[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
    }
  }
  
  return(victim_history_physical)
}

Personal_Occurence_DF_Descriptive$victim_history_physical <- calculate_victim_history_physical(Personal_Occurence_DF_Descriptive)


unique_count <- Personal_Occurence_DF_Descriptive %>%
  filter(victim_history_physical == 1) %>%
  group_by(Gender) %>%
  summarise(unique_persons = n_distinct(Person_ID))  

print(unique_count)




# Average Effect Physical Victim Pre-14:

calculate_victim_history_physical_Pre_14 <- function(Personal_Occurence_DF_Descriptive) {
  victim_history_physical_pre_14 <- numeric(nrow(Personal_Occurence_DF_Descriptive))
  
  for (i in unique(Personal_Occurence_DF_Descriptive$Person_ID)) {
    subset_df <- Personal_Occurence_DF_Descriptive[Personal_Occurence_DF_Descriptive$Person_ID == i, ]
    
    subset_df <- subset_df[subset_df$Age_at_incident <= 13, ]
    
    victim_physical_dates <- subset_df$Time_of_incident[subset_df$Victim_status == 1 & subset_df$Offence %in% 1:5]
    
    first_offence_dates <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Victim_status == 0 & subset_df$Outcome != 10 & subset_df$Offence %in% 1:5]
    
    if (length(victim_physical_dates) > 0 && length(first_offence_dates) == 0) {
      victim_history_physical_pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 1
    } else if (length(victim_physical_dates) > 0 && length(first_offence_dates) > 0) {
      if (min(victim_physical_dates) < min(first_offence_dates)) {
        victim_history_physical_pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 1
      } else {
        victim_history_physical_pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
      }
    } else {
      victim_history_physical_pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
    }
  }
  
  return(victim_history_physical_pre_14)
}

Personal_Occurence_DF_Descriptive$victim_history_physical_pre_14 <- calculate_victim_history_physical_Pre_14(Personal_Occurence_DF_Descriptive)


unique_count <- Personal_Occurence_DF_Descriptive %>%
  filter(victim_history_physical_pre_14 == 1) %>%
  group_by(Gender) %>%
  summarise(unique_persons = n_distinct(Person_ID))  

print(unique_count)



# Frequency Effect Physical Victim:

calculate_physical_history_freq_All <- function(Personal_Occurence_DF_Descriptive) {
  Physical_Assault_Frequency <- numeric(nrow(Personal_Occurence_DF_Descriptive))
  
  for (i in unique(Personal_Occurence_DF_Descriptive$Person_ID)) {
    
    subset_df <- Personal_Occurence_DF_Descriptive[Personal_Occurence_DF_Descriptive$Person_ID == i, ]
    
    if (any(subset_df$victim_history_physical == 1)) {
      
      victim_physical_dates <- subset_df$Time_of_incident[subset_df$Victim_status == 1 & subset_df$Offence %in% 1:5]
      
      first_offence_dates <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Victim_status == 0 & subset_df$Outcome != 10 & subset_df$Offence %in% 1:5]
      
      if (length(victim_physical_dates) > 0 && length(first_offence_dates) == 0) {
        Physical_Assault_Frequency[Personal_Occurence_DF_Descriptive$Person_ID == i] <- length(victim_physical_dates)
      } 
      else if (length(victim_physical_dates) > 0 && length(first_offence_dates) > 0) {
        first_offence_date <- min(first_offence_dates, na.rm = TRUE)
        
        physical_before_first_offence <- sum(victim_physical_dates < first_offence_date)
        
        Physical_Assault_Frequency[Personal_Occurence_DF_Descriptive$Person_ID == i] <- physical_before_first_offence
      }
      else {
        Physical_Assault_Frequency[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
      }
    }
  }
  
  return(Physical_Assault_Frequency)
}

Personal_Occurence_DF_Descriptive$Physical_Assault_Frequency <- calculate_physical_history_freq_All(Personal_Occurence_DF_Descriptive)


unique_count <- Personal_Occurence_DF_Descriptive %>%
  filter(Physical_Assault_Frequency > 0) %>%  
  summarise(unique_persons = n_distinct(Person_ID))  

print(unique_count)



# Frequency Effect Physical Victim Pre-14:

calculate_physical_history_freq_Pre_All_14 <- function(Personal_Occurence_DF_Descriptive) {
  Physical_Assault_Frequency_Pre_14 <- numeric(nrow(Personal_Occurence_DF_Descriptive))
  
  for (i in unique(Personal_Occurence_DF_Descriptive$Person_ID)) {
    
    subset_df <- Personal_Occurence_DF_Descriptive[Personal_Occurence_DF_Descriptive$Person_ID == i, ]
    
    if (subset_df$victim_history_physical_pre_14[1] == 1) {
      subset_df <- subset_df[subset_df$Age_at_incident <= 13, ]
      victim_physical_dates <- subset_df$Time_of_incident[subset_df$Victim_status == 1 & subset_df$Offence %in% 1:5]
      
      first_offence_dates <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Victim_status == 0 & subset_df$Outcome != 10 & subset_df$Offence %in% 1:5]
      
      if (length(victim_physical_dates) > 0 && length(first_offence_dates) == 0) {
        Physical_Assault_Frequency_Pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- length(victim_physical_dates)
      } 
      else if (length(victim_physical_dates) > 0 && length(first_offence_dates) > 0) {
        first_offence_date <- min(first_offence_dates, na.rm = TRUE)
        
        physical_before_first_offence <- sum(victim_physical_dates < first_offence_date)
        
        Physical_Assault_Frequency_Pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- physical_before_first_offence
      }
      else {
        Physical_Assault_Frequency_Pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
      }
    }
  }
  
  return(Physical_Assault_Frequency_Pre_14)
}

Personal_Occurence_DF_Descriptive$Physical_Assault_Frequency_Pre_14 <- calculate_physical_history_freq_Pre_All_14(Personal_Occurence_DF_Descriptive)


unique_count <- Personal_Occurence_DF_Descriptive %>%
  filter(Physical_Assault_Frequency_Pre_14 > 0) %>%  
  summarise(unique_persons = n_distinct(Person_ID))  

print(unique_count)



# Average Effect Sexual Offences:

calculate_offence_history_NV_sexual <- function(Personal_Occurence_DF_Descriptive) {
  Offence_history_NV_sexual <- numeric(nrow(Personal_Occurence_DF_Descriptive))
  for (i in unique(Personal_Occurence_DF_Descriptive$Person_ID)) {
    subset_df <- Personal_Occurence_DF_Descriptive[Personal_Occurence_DF_Descriptive$Person_ID == i, ]
    
    Offence_history_NV_dates <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Victim_status == 0 & (subset_df$Offence %in% 6:7)]
    first_offence_dates <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Victim_status == 0 & subset_df$Outcome != 10 & subset_df$Offence %in% 1:5]
    
    if (length(Offence_history_NV_dates) > 0 && length(first_offence_dates) == 0) {
      Offence_history_NV_sexual[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 1
    } else if (length(Offence_history_NV_dates) > 0 && length(first_offence_dates) > 0) {
      if (min(Offence_history_NV_dates) < min(first_offence_dates)) {
        Offence_history_NV_sexual[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 1
      } else {
        Offence_history_NV_sexual[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
      }
    } else {
      Offence_history_NV_sexual[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
    }
  }
  
  return(Offence_history_NV_sexual)
}

Personal_Occurence_DF_Descriptive$Offence_history_NV_sexual <- calculate_offence_history_NV_sexual(Personal_Occurence_DF_Descriptive)


unique_count <- Personal_Occurence_DF_Descriptive %>%
  filter(Offence_history_NV_sexual == 1) %>%
  group_by(Gender) %>%
  summarise(unique_persons = n_distinct(Person_ID))  

print(unique_count)


# Average Effect Sexual Offences Pre-14

calculate_offence_history_sexual_pre_14 <- function(Personal_Occurence_DF_Descriptive) {
  Offence_history_sexual_pre_14 <- numeric(nrow(Personal_Occurence_DF_Descriptive))
  
  for (i in unique(Personal_Occurence_DF_Descriptive$Person_ID)) {
    subset_df <- Personal_Occurence_DF_Descriptive[Personal_Occurence_DF_Descriptive$Person_ID == i, ]
    
    subset_df <- subset_df[subset_df$Age_at_incident <= 13, ]
    
    Offence_history_NV_dates <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Victim_status == 0 & (subset_df$Offence %in% 6:7)]
    first_offence_dates <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Victim_status == 0 & subset_df$Outcome != 10 & subset_df$Offence %in% 1:5]
    
    if (length(Offence_history_NV_dates) > 0 && length(first_offence_dates) == 0) {
      Offence_history_sexual_pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 1
    } else if (length(Offence_history_NV_dates) > 0 && length(first_offence_dates) > 0) {
      if (min(Offence_history_NV_dates) < min(first_offence_dates)) {
        Offence_history_sexual_pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 1
      } else {
        Offence_history_sexual_pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
      }
    } else {
      Offence_history_sexual_pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
    }
  }
  
  return(Offence_history_sexual_pre_14)
}

Personal_Occurence_DF_Descriptive$Offence_history_sexual_pre_14 <- calculate_offence_history_sexual_pre_14(Personal_Occurence_DF_Descriptive)


unique_count <- Personal_Occurence_DF_Descriptive %>%
  filter(Offence_history_sexual_pre_14 == 1) %>%
  group_by(Gender) %>%
  summarise(unique_persons = n_distinct(Person_ID))  

print(unique_count)


# Frequency Effect Sexual Offences:

calculate_nv_history_freq_sexual <- function(Personal_Occurence_DF_Descriptive) {
  NV_Offence_Frequency_Sexual <- numeric(nrow(Personal_Occurence_DF_Descriptive))
  for (i in unique(Personal_Occurence_DF_Descriptive$Person_ID)) {
    subset_df <- Personal_Occurence_DF_Descriptive[Personal_Occurence_DF_Descriptive$Person_ID == i, ]
    
    if (any(subset_df$Offence_history_NV_sexual == 1)) {
      NV_dates_freq <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Victim_status == 0 & (subset_df$Offence %in% 6:7)]
      
      first_offence_dates <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Victim_status == 0 & subset_df$Outcome != 10 & subset_df$Offence %in% 1:5]
      
      if (length(NV_dates_freq) > 0 && length(first_offence_dates) == 0) {
        NV_Offence_Frequency_Sexual[Personal_Occurence_DF_Descriptive$Person_ID == i] <- length(NV_dates_freq)
      } 
      else if (length(NV_dates_freq) > 0 && length(first_offence_dates) > 0) {
        first_offence_date <- min(first_offence_dates, na.rm = TRUE)
        
        NV_before_first_offence <- sum(NV_dates_freq < first_offence_date)
        
        NV_Offence_Frequency_Sexual[Personal_Occurence_DF_Descriptive$Person_ID == i] <- NV_before_first_offence
      }
      else {
        NV_Offence_Frequency_Sexual[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
      }
    }
  }
  
  return(NV_Offence_Frequency_Sexual)
}

Personal_Occurence_DF_Descriptive$NV_Offence_Frequency_Sexual <- calculate_nv_history_freq_sexual(Personal_Occurence_DF_Descriptive)


unique_count <- Personal_Occurence_DF_Descriptive %>%
  filter(NV_Offence_Frequency_Sexual > 0) %>%  
  summarise(unique_persons = n_distinct(Person_ID))  

print(unique_count)


# Frequency Effect Sexual Offences Pre-14:

calculate_sexual_history_freq_Pre_14 <- function(Personal_Occurence_DF_Descriptive) {
  sexual_history_freq_Pre_14 <- numeric(nrow(Personal_Occurence_DF_Descriptive))
  for (i in unique(Personal_Occurence_DF_Descriptive$Person_ID)) {
    subset_df <- Personal_Occurence_DF_Descriptive[Personal_Occurence_DF_Descriptive$Person_ID == i, ]
    if (subset_df$Offence_history_sexual_pre_14[1] == 1) {
      subset_df <- subset_df[subset_df$Age_at_incident <= 13, ]
      NV_dates_freq_pre_14 <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Victim_status == 0 & (subset_df$Offence %in% 6:7)]
      
      first_offence_dates <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Victim_status == 0 & subset_df$Outcome != 10 & subset_df$Offence %in% 1:5]
      
      if (length(NV_dates_freq_pre_14) > 0 && length(first_offence_dates) == 0) {
        sexual_history_freq_Pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- length(NV_dates_freq_pre_14)
      } 
      else if (length(NV_dates_freq_pre_14) > 0 && length(first_offence_dates) > 0) {
        first_offence_date <- min(first_offence_dates, na.rm = TRUE)
        
        NV_before_first_offence <- sum(NV_dates_freq_pre_14 < first_offence_date)
        
        sexual_history_freq_Pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- NV_before_first_offence
      }
      else {
        sexual_history_freq_Pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
      }
    }
  }
  
  return(sexual_history_freq_Pre_14)
}

Personal_Occurence_DF_Descriptive$sexual_history_freq_Pre_14 <- calculate_sexual_history_freq_Pre_14(Personal_Occurence_DF_Descriptive)


unique_count <- Personal_Occurence_DF_Descriptive %>%
  filter(sexual_history_freq_Pre_14 > 0) %>%  
  summarise(unique_persons = n_distinct(Person_ID))  

print(unique_count)



# Average Effect Non-Violent Offence Victim:

calculate_offence_history_NV_Victim <- function(Personal_Occurence_DF_Descriptive) {
  Offence_history_NV_Victim <- numeric(nrow(Personal_Occurence_DF_Descriptive))
  for (i in unique(Personal_Occurence_DF_Descriptive$Person_ID)) {
    subset_df <- Personal_Occurence_DF_Descriptive[Personal_Occurence_DF_Descriptive$Person_ID == i, ]
    
    Offence_history_NV_dates <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Victim_status == 1 & (subset_df$Offence %in% 9:16)]
    first_offence_dates <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Victim_status == 0 & subset_df$Outcome != 10 & subset_df$Offence %in% 1:5]
    
    if (length(Offence_history_NV_dates) > 0 && length(first_offence_dates) == 0) {
      Offence_history_NV_Victim[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 1
    } else if (length(Offence_history_NV_dates) > 0 && length(first_offence_dates) > 0) {
      if (min(Offence_history_NV_dates) < min(first_offence_dates)) {
        Offence_history_NV_Victim[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 1
      } else {
        Offence_history_NV_Victim[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
      }
    } else {
      Offence_history_NV_Victim[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
    }
  }
  
  return(Offence_history_NV_Victim)
}

Personal_Occurence_DF_Descriptive$Offence_history_NV_Victim <- calculate_offence_history_NV_Victim(Personal_Occurence_DF_Descriptive)


unique_count <- Personal_Occurence_DF_Descriptive %>%
  filter(Offence_history_NV_Victim == 1) %>%
  group_by(Gender) %>%
  summarise(unique_persons = n_distinct(Person_ID))  

print(unique_count)



# Average Effect Non-Violent Offence Victim Pre-14:

calculate_offence_history_victim_pre_14 <- function(Personal_Occurence_DF_Descriptive) {
  Offence_history_victim_pre_14 <- numeric(nrow(Personal_Occurence_DF_Descriptive))
  
  for (i in unique(Personal_Occurence_DF_Descriptive$Person_ID)) {
    subset_df <- Personal_Occurence_DF_Descriptive[Personal_Occurence_DF_Descriptive$Person_ID == i, ]
    
    subset_df <- subset_df[subset_df$Age_at_incident <= 13, ]
    
    Offence_history_NV_dates <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Victim_status == 1 & (subset_df$Offence %in% 9:16)]
    first_offence_dates <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Victim_status == 0 & subset_df$Outcome != 10 & subset_df$Offence %in% 1:5]
    
    if (length(Offence_history_NV_dates) > 0 && length(first_offence_dates) == 0) {
      Offence_history_victim_pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 1
    } else if (length(Offence_history_NV_dates) > 0 && length(first_offence_dates) > 0) {
      if (min(Offence_history_NV_dates) < min(first_offence_dates)) {
        Offence_history_victim_pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 1
      } else {
        Offence_history_victim_pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
      }
    } else {
      Offence_history_victim_pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
    }
  }
  
  return(Offence_history_victim_pre_14)
}

Personal_Occurence_DF_Descriptive$Offence_history_victim_pre_14 <- calculate_offence_history_victim_pre_14(Personal_Occurence_DF_Descriptive)


unique_count <- Personal_Occurence_DF_Descriptive %>%
  filter(Offence_history_victim_pre_14 == 1) %>%
  group_by(Gender) %>%
  summarise(unique_persons = n_distinct(Person_ID))  

print(unique_count)



# Frequency Effect Non-Violent Offence Victim:

calculate_nv_history_freq_victim <- function(Personal_Occurence_DF_Descriptive) {
  NV_Offence_Frequency_victim <- numeric(nrow(Personal_Occurence_DF_Descriptive))
  for (i in unique(Personal_Occurence_DF_Descriptive$Person_ID)) {
    subset_df <- Personal_Occurence_DF_Descriptive[Personal_Occurence_DF_Descriptive$Person_ID == i, ]
    
    if (any(subset_df$Offence_history_NV_Victim == 1)) {
      NV_dates_freq <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Victim_status == 1 & subset_df$Offence %in% 9:16]
      
      first_offence_dates <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Victim_status == 0 & subset_df$Outcome != 10 & subset_df$Offence %in% 1:5]
      
      if (length(NV_dates_freq) > 0 && length(first_offence_dates) == 0) {
        NV_Offence_Frequency_victim[Personal_Occurence_DF_Descriptive$Person_ID == i] <- length(NV_dates_freq)
      } 
      else if (length(NV_dates_freq) > 0 && length(first_offence_dates) > 0) {
        first_offence_date <- min(first_offence_dates, na.rm = TRUE)
        
        NV_before_first_offence <- sum(NV_dates_freq < first_offence_date)
        
        NV_Offence_Frequency_victim[Personal_Occurence_DF_Descriptive$Person_ID == i] <- NV_before_first_offence
      }
      else {
        NV_Offence_Frequency_victim[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
      }
    }
  }
  
  return(NV_Offence_Frequency_victim)
}

Personal_Occurence_DF_Descriptive$NV_Offence_Frequency_victim <- calculate_nv_history_freq_victim(Personal_Occurence_DF_Descriptive)


unique_count <- Personal_Occurence_DF_Descriptive %>%
  filter(NV_Offence_Frequency_victim > 0) %>%  
  summarise(unique_persons = n_distinct(Person_ID))  

print(unique_count)


# Frequency Effect Non-Violent Offence Victim Pre-14:

calculate_nv_victim_history_freq_Pre_14 <- function(Personal_Occurence_DF_Descriptive) {
  nv_victim_history_freq_Pre_14 <- numeric(nrow(Personal_Occurence_DF_Descriptive))
  for (i in unique(Personal_Occurence_DF_Descriptive$Person_ID)) {
    subset_df <- Personal_Occurence_DF_Descriptive[Personal_Occurence_DF_Descriptive$Person_ID == i, ]
    if (subset_df$Offence_history_victim_pre_14[1] == 1) {
      subset_df <- subset_df[subset_df$Age_at_incident <= 13, ]
      NV_dates_freq_pre_14 <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Victim_status == 1 & (subset_df$Offence %in% 9:16)]
      
      first_offence_dates <- subset_df$Time_of_incident[subset_df$Witness == 0 & subset_df$Victim_status == 0 & subset_df$Outcome != 10 & subset_df$Offence %in% 1:5]
      
      if (length(NV_dates_freq_pre_14) > 0 && length(first_offence_dates) == 0) {
        nv_victim_history_freq_Pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- length(NV_dates_freq_pre_14)
      } 
      else if (length(NV_dates_freq_pre_14) > 0 && length(first_offence_dates) > 0) {
        first_offence_date <- min(first_offence_dates, na.rm = TRUE)
        
        NV_before_first_offence <- sum(NV_dates_freq_pre_14 < first_offence_date)
        
        nv_victim_history_freq_Pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- NV_before_first_offence
      }
      else {
        nv_victim_history_freq_Pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
      }
    }
  }
  
  return(nv_victim_history_freq_Pre_14)
}

Personal_Occurence_DF_Descriptive$nv_victim_history_freq_Pre_14 <- calculate_nv_victim_history_freq_Pre_14(Personal_Occurence_DF_Descriptive)


unique_count <- Personal_Occurence_DF_Descriptive %>%
  filter(nv_victim_history_freq_Pre_14 > 0) %>%  
  summarise(unique_persons = n_distinct(Person_ID))  

print(unique_count)



# Code violent offence dependent variables 


# Aggregate Violent Offences

Personal_Occurence_DF_Descriptive <- Personal_Occurence_DF_Descriptive %>%
  group_by(Person_ID) %>%
  mutate(First_Offence_1_to_5 = case_when(
    
    
    any(Offence %in% 1:5 & Victim_status == 0 & Witness == 0 & Eliminated_from_investigation == 0 & Outcome != 10) ~ 1,
    
    TRUE ~ 0
  )) %>%
  ungroup()

unique_count <- Personal_Occurence_DF_Descriptive %>%
  filter(First_Offence_1_to_5 == 1) %>%
  group_by(Gender) %>%
  summarise(unique_persons = n_distinct(Person_ID))  

print(unique_count)



# Common Violent Offences:


Personal_Occurence_DF_Descriptive <- Personal_Occurence_DF_Descriptive %>%
  group_by(Person_ID) %>%
  mutate(First_Offence_Common = case_when(
    
    any(Offence %in% 1:2 & Victim_status == 0 & Witness == 0 & Eliminated_from_investigation == 0 & Outcome != 10) ~ {
      
      # First common offence date
      offence_1_dates <- Time_of_incident[Offence %in% 1:2 & Victim_status == 0 & Witness == 0 & Eliminated_from_investigation == 0 & Outcome != 10]
      
      if (length(offence_1_dates) > 0) {
        1  # Person has at least one common offence
      } else {
        0
      }
    },
    TRUE ~ 0
  )) %>%
  ungroup()



# Aggregate Violent Offences pre-14

calculate_first_violent_offence_pre_14 <- function(Personal_Occurence_DF_Descriptive) {
  first_violent_offence_pre_14 <- numeric(nrow(Personal_Occurence_DF_Descriptive))
  
  for (i in unique(Personal_Occurence_DF_Descriptive$Person_ID)) {
    
    
    subset_df <- Personal_Occurence_DF_Descriptive[
      Personal_Occurence_DF_Descriptive$Person_ID == i & 
        Personal_Occurence_DF_Descriptive$Victim_status == 0 & 
        Personal_Occurence_DF_Descriptive$Witness == 0 & 
        Personal_Occurence_DF_Descriptive$Eliminated_from_investigation == 0 &
        Personal_Occurence_DF_Descriptive$Outcome != 10, ]
    
    
    violent_offence_dates_pre_14 <- subset_df$Time_of_incident[subset_df$Offence %in% 1:5  & subset_df$Age_at_incident <= 13]
    
    
    all_violent_offence_dates <- subset_df$Time_of_incident[subset_df$Offence %in% 1:5]
    
    
    if (length(violent_offence_dates_pre_14) > 0 && min(violent_offence_dates_pre_14) == min(all_violent_offence_dates)) {
      first_violent_offence_pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 1
    } else {
      first_violent_offence_pre_14[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
    }
  }
  
  return(first_violent_offence_pre_14)
}

Personal_Occurence_DF_Descriptive$first_violent_offence_pre_14 <- calculate_first_violent_offence_pre_14(Personal_Occurence_DF_Descriptive)

unique_count <- Personal_Occurence_DF_Descriptive %>%
  filter(first_violent_offence_pre_14 == 1) %>%  
  summarise(unique_persons = n_distinct(Person_ID))

print(unique_count)




# Aggregate Violent Offences post-14

calculate_first_violent_offence_post_14 <- function(df) {
  result <- rep(0, nrow(df)) 
  
  for (person_id in unique(df$Person_ID)) {
    person_rows <- which(df$Person_ID == person_id)
    
    
    person_df <- df[person_rows, ]
    
    
    filtered_df <- person_df %>%
      filter(Victim_status == 0, Witness == 0, Eliminated_from_investigation == 0, Outcome != 10)
    
    
    violent_df <- filtered_df %>%
      filter(Offence %in% 1:5)
    
    if (nrow(violent_df) == 0) next 
    
    
    first_violent_date <- min(violent_df$Time_of_incident, na.rm = TRUE)
    
   
    violent_after_14 <- violent_df %>% filter(Age_at_incident > 13)
    
    if (nrow(violent_after_14) > 0 &&
        min(violent_after_14$Time_of_incident, na.rm = TRUE) == first_violent_date) {
      
      result[person_rows] <- 1
    }
  }
  
  return(result)
}

Personal_Occurence_DF_Descriptive$first_violent_offence_post_14 <- calculate_first_violent_offence_post_14(Personal_Occurence_DF_Descriptive)

unique_count <- Personal_Occurence_DF_Descriptive %>%
  filter(first_violent_offence_post_14 == 1) %>%  
  summarise(unique_persons = n_distinct(Person_ID))

print(unique_count)


# Common violent offence as first pre-14

calculate_first_violent_offence_pre_14 <- function(Personal_Occurence_DF_Descriptive) {
  first_violent_offence_pre_14_Common <- numeric(nrow(Personal_Occurence_DF_Descriptive))
  
  for (i in unique(Personal_Occurence_DF_Descriptive$Person_ID)) {
    
    subset_df <- Personal_Occurence_DF_Descriptive[
      Personal_Occurence_DF_Descriptive$Person_ID == i & 
        Personal_Occurence_DF_Descriptive$Victim_status == 0 & 
        Personal_Occurence_DF_Descriptive$Witness == 0 & 
        Personal_Occurence_DF_Descriptive$Eliminated_from_investigation == 0 &
        Personal_Occurence_DF_Descriptive$Outcome != 10, ]
    
    violent_offence_dates_pre_14 <- subset_df$Time_of_incident[subset_df$Offence %in% 1:2 & subset_df$Age_at_incident <= 13]
    
    if (length(violent_offence_dates_pre_14) > 0) {
      first_violent_offence_pre_14_Common[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 1
    } else {
      first_violent_offence_pre_14_Common[Personal_Occurence_DF_Descriptive$Person_ID == i] <- 0
    }
  }
  
  return(first_violent_offence_pre_14_Common)
}

Personal_Occurence_DF_Descriptive$first_violent_offence_pre_14_Common <- calculate_first_violent_offence_pre_14(Personal_Occurence_DF_Descriptive)


unique_count <- Personal_Occurence_DF_Descriptive %>%
  filter(first_violent_offence_pre_14_Common == 1) %>%  
  summarise(unique_persons = n_distinct(Person_ID))

print(unique_count)




# Common Violent Offence as first post-14

calculate_first_violent_offence_post_14_common <- function(df) {
  result <- rep(0, nrow(df)) # initialize vector to hold 0 or 1 per row
  
  for (person_id in unique(df$Person_ID)) {
    person_rows <- which(df$Person_ID == person_id)
    
    # Subset only this person’s rows
    person_df <- df[person_rows, ]
    
    # Filter to only rows where person was not a victim, witness, or eliminated
    filtered_df <- person_df %>%
      filter(Victim_status == 0, Witness == 0, Eliminated_from_investigation == 0, Outcome != 10)
    
    # Only look at violent offences (codes 1:5)
    violent_df <- filtered_df %>%
      filter(Offence %in% 1:2)
    
    if (nrow(violent_df) == 0) next # No violent offences, skip
    
    # First ever violent offence (any age)
    first_violent_date <- min(violent_df$Time_of_incident, na.rm = TRUE)
    
    # Violent offences after age 14
    violent_after_14 <- violent_df %>% filter(Age_at_incident > 13)
    
    if (nrow(violent_after_14) > 0 &&
        min(violent_after_14$Time_of_incident, na.rm = TRUE) == first_violent_date) {
      # If the first violent offence happened after age 14
      result[person_rows] <- 1
    }
  }
  
  return(result)
}

Personal_Occurence_DF_Descriptive$first_violent_offence_post_14_common <- calculate_first_violent_offence_post_14_common(Personal_Occurence_DF_Descriptive)

unique_count <- Personal_Occurence_DF_Descriptive %>%
  filter(first_violent_offence_post_14_common == 1) %>%  
  summarise(unique_persons = n_distinct(Person_ID))

print(unique_count)



# Create a time variable which then support TTE regression analysis

# Time Violent Offences

start_date <- as.Date("2014-03-25")
end_date <- as.Date("2021-03-25")

Personal_Occurence_DF_Descriptive <- Personal_Occurence_DF_Descriptive %>%
  group_by(Person_ID) %>%
  mutate(
    Time_All = case_when(
      
      any(Offence %in% 1:5 & Victim_status == 0 & Witness == 0 & Eliminated_from_investigation == 0 & Outcome !=10) ~ {
        
        first_offense_dates <- Time_of_incident[Offence %in% 1:5 & Victim_status == 0 & Witness == 0 & Eliminated_from_investigation == 0 & Outcome !=10]
        
        
        if (length(first_offense_dates) > 0) {
          first_offense_date <- min(first_offense_dates, na.rm = TRUE)  
          
          
          time_to_event <- as.numeric(first_offense_date - start_date) / 365  # Convert to years
          
          
          time_to_event <- pmin(time_to_event, as.numeric(end_date - start_date) / 365)
          
          round(time_to_event, 3)
        } else {
          8  
        }
      },
      TRUE ~ 8 
    )
  ) %>%
  ungroup()




