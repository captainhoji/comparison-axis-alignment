source("scripts/wrangling.R")
source("scripts/exclude.R")

library(dplyr)
library(tidyverse)

data_path <- file.path("data", "Trial.csv")
df_raw <- read_csv(data_path)

df_raw <- wrangleRange(df_raw) #data preprocessing
df <- applyExclusion(df_raw) #excluding based on exclusion criteria


# Removing engagement checks from analysis
{
  df <- df %>%
    filter(stimuli_number != -999)
  df_raw <- df_raw %>%
    filter(stimuli_number != -999)
}

df_correct <- df %>%
  filter(correct == 1)
df_raw_correct <- df_raw %>%
  filter(correct == 1)
df_correct_index <- df_correct %>%
  filter(task == "compare_index")
df_correct_height <- df_correct %>%
  filter(task == "compare_height")
df_correct_length <- df_correct %>%
  filter(task == "compare_length")
df_withoutLength <- df_correct %>%
  filter(task != "compare_length")
df_withoutLength_all <- df %>%
  filter(task != "compare_length")
  