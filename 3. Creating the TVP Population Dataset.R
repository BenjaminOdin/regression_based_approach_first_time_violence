# Create the TVP VRU Final dataset which is referred to as Analysis_DF

Analysis_DF <- Personal_Occurence_DF_Descriptive %>% dplyr::select(Person_ID, Gender, Age_at_incident, Ethnicity, first_violent_offence_post_14_common, first_violent_offence_post_14, First_Offence_1_to_5, first_violent_offence_pre_14, First_Offence_Common, first_violent_offence_pre_14_Common, Neglect_history, victim_history_sexual, victim_history_physical, Offence_history_NV, missing_history, Neglect_history_pre_14, victim_history_sexual_pre_14, victim_history_physical_pre_14, Offence_history_NV_pre_14, missing_history_pre_14, Witness_history_pre_14, Neglect_Frequency, Sexual_Assault_Frequency, Physical_Assault_Frequency, Missing_Frequency, NV_Offence_Frequency, Witness_Offence_Frequency, Time_All, Time_of_Day_label, Witness_history, Month, Season, Neglect_history_freq_Pre_14, Sexual_history_freq_Pre_14, Physical_Assault_Frequency_Pre_14, Missing_history_freq_Pre_14, NV_history_freq_Pre_14, witness_freq_Pre_14, Offence_history_NV_sexual, Offence_history_NV_Victim, NV_Offence_Frequency_Sexual, NV_Offence_Frequency_victim, nv_victim_history_freq_Pre_14, Offence_history_victim_pre_14, sexual_history_freq_Pre_14, Offence_history_sexual_pre_14) 
Analysis_DF <- Analysis_DF %>%
  distinct(Person_ID, .keep_all = TRUE)

# Create the remaining population from Census data and merge with the Analysis_DF

count_males <- Analysis_DF %>%
  filter(Gender == 3) %>%
  summarise(count = n())

print(count_males)


count_females <- Analysis_DF %>%
  filter(Gender == 2) %>%
  summarise(count = n())

print(count_females)


num_males <- 9479
num_females <- 9741
total_individuals <- num_males + num_females

random_ids <- sample(0:999999999999999, total_individuals, replace = FALSE)

gender <- c(rep(3, num_males), rep(2, num_females)) 

time_all <- rep(8, total_individuals)

Ancillary_Data <- data.frame(
  Person_ID = random_ids,
  Gender = gender,
  Age_at_incident = 18,
  first_violent_offence_post_14_common = 0,
  first_violent_offence_post_14 = 0,
  First_Offence_1_to_5 = 0,
  first_violent_offence_pre_14 = 0,
  First_Offence_Common = 0,
  first_violent_offence_pre_14_Common = 0,
  Neglect_history = 0,
  victim_history_sexual = 0,
  victim_history_physical = 0,
  Offence_history_NV = 0,
  missing_history = 0,
  Neglect_Frequency = 0,
  Sexual_Assault_Frequency = 0,
  Physical_Assault_Frequency = 0,
  Missing_Frequency = 0,
  NV_Offence_Frequency = 0,
  Witness_Offence_Frequency = 0,
  Neglect_history_pre_14 = 0,
  victim_history_sexual_pre_14 = 0,
  victim_history_physical_pre_14 = 0,
  Offence_history_NV_pre_14 = 0,
  missing_history_pre_14 = 0,
  Witness_history_pre_14 = 0,
  Time_All = time_all,
  Time_of_Day_label = rep(0, total_individuals),
  Season = rep(0, total_individuals),
  Month = rep(0, total_individuals),
  Witness_history = 0,
  Neglect_history_freq_Pre_14 = 0,
  Sexual_history_freq_Pre_14 = 0, 
  Physical_Assault_Frequency_Pre_14 = 0, 
  Missing_history_freq_Pre_14 = 0,
  NV_history_freq_Pre_14 = 0,
  witness_freq_Pre_14 = 0,
  Offence_history_NV_sexual = 0, 
  Offence_history_NV_Victim = 0, 
  NV_Offence_Frequency_Sexual = 0, 
  NV_Offence_Frequency_victim = 0,
  nv_victim_history_freq_Pre_14 = 0,
  Offence_history_victim_pre_14 = 0, 
  sexual_history_freq_Pre_14 = 0, 
  Offence_history_sexual_pre_14 = 0)



Ancillary_Data$Gender <- as.factor(as.character(Ancillary_Data$Gender))
Analysis_DF$Time_of_Day_label <- as.factor(Analysis_DF$Time_of_Day_label)
Ancillary_Data$Time_of_Day_label <- as.factor(Ancillary_Data$Time_of_Day_label)
Ancillary_Data$Season <- as.factor(as.character(Ancillary_Data$Gender))

Combined_Data <- bind_rows(Analysis_DF, Ancillary_Data)

Combined_Data$Time_of_Day_label <- as.factor(Combined_Data$Time_of_Day_label)
Combined_Data$Witness_history <- as.numeric(Combined_Data$Witness_history)
Combined_Data$Gender <- droplevels(Combined_Data$Gender)

