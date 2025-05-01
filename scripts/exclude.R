# filter_participants.R
library(dplyr)
library(readr)

# Reject person if their mean RT is outside of range of everyone's RT mean +- 2SD
rejectOutlierParticipants <- function(df) {
  # Step 1: Compute Global Mean Duration (globalRTmean)
  globalRTmean <- mean(df$duration, na.rm = TRUE)
  
  # Step 2: Compute Mean Duration for Each Participant
  df_participant_means <- df %>%
    group_by(participant_id) %>%
    summarise(mean_duration = mean(duration, na.rm = TRUE), .groups = "drop")
  
  # Step 3: Compute Standard Deviation of Participant Mean Durations
  sd_duration_means <- sd(df_participant_means$mean_duration, na.rm = TRUE)
  
  # Step 4: Identify Participants to Remove (mean_duration > globalRTmean + 2SD)
  participants_to_remove <- df_participant_means %>%
    filter(mean_duration > globalRTmean + 2 * sd_duration_means) %>%
    pull(participant_id)  # Extracts participant IDs to remove
  
  cat(length(participants_to_remove), " participants removed by outlier:\n")
  print(participants_to_remove)
  
  # Step 5: Remove Participants from df
  df_cleaned <- df %>%
    filter(!participant_id %in% participants_to_remove)
  
  # View Results
  return(df_cleaned)  # or View(df_cleaned) if using RStudio
  
}

# Reject trials outside of a person's average +-2SD range
excludePersonalOutliers <- function(df) {
  df_filtered <- df %>%
    group_by(participant_id) %>%
    mutate(
      mean_duration = mean(duration, na.rm = TRUE),
      sd_duration = sd(duration, na.rm = TRUE)
    ) %>%
    filter(duration >= mean_duration - 2 * sd_duration & 
             duration <= mean_duration + 2 * sd_duration) %>%
    dplyr::select(-mean_duration, -sd_duration)  # Remove extra columns
  return(df_filtered)
}

# Exclude Participant if they failed an easy practice
excludeByPracticeFail <- function(df) {
  data_path <- file.path("data", "PracticeFail.csv")
  df_practiceFail <- read_csv(data_path)
  
  df_ex <- df %>%
    filter(participant_id %in% df_practiceFail$participant_id) %>%
    distinct(participant_id)
  
  cat(length(df_ex$participant_id), " participants removed by practice fail:\n")
  print(df_ex$participant_id)
  
  df_cleaned <- df %>%
    filter(!participant_id %in% df_practiceFail$participant_id)
  
  return(df_cleaned)
}

# Exclude participant if their engagement check accuracy is under 0.75
excludeByEngagementCheck <- function(df) {
  # Compute engagement check accuracy per participant
  accuracy_df <- df %>%
    filter(stimuli_number == -999) %>%
    group_by(participant_id) %>%
    summarize(engagement_accuracy = mean(correct, na.rm = TRUE)) %>%
    filter(engagement_accuracy < 0.75)
  
  cat(length(accuracy_df$participant_id), " participants removed by engagement check:\n")
  print(accuracy_df$participant_id)
  
  # Keep only participants who passed the engagement check
  df_filtered <- df %>%
    filter(!participant_id %in% accuracy_df$participant_id)
  
  return(df_filtered)
}

applyExclusion <- function(df) {
  df <- excludeByPracticeFail(df)
  df <- excludeByEngagementCheck(df)
  df <- rejectOutlierParticipants(df)
  df <- excludePersonalOutliers(df)
  return(df)
}
