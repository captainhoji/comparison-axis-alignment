library(dplyr)
library(tidyverse)

wrangleRange <- function(df) {
  # Extract the within-block index (0 to 31)
  df$condition_code <- df$stimuli_number %% 32
  
  df$higherSide  <- (df$condition_code %/% 16) + 1
  df$laterSide   <- (df$condition_code %% 16 %/% 8) + 1
  df$longerSide  <- (df$condition_code %% 8 %/% 4) + 1
  df$darkerSide  <- (df$condition_code %% 4 %/% 2) + 1
  df$difficulty  <- 3 - ((df$condition_code %% 2) + 1)
  
  df <- df %>%
    distinct(participant_id, trial_number, layout, orientation, .keep_all = TRUE) %>%
    mutate(
      answer = ifelse(correct == 1, response, 3-response)
    ) %>%
    mutate(
      correct = as.numeric(correct),  # Convert TRUE/FALSE to 1/0
      participant_id = as.character(participant_id),  # Ensure ID is not a number
      logRT = log10(duration),
      congruence = ifelse(
        (task == "compare_height" & ((layout == "horizontal" & orientation == "vertical") | (layout == "vertical" & orientation == "horizontal")))
          | (task == "compare_index" & ((layout == "vertical" & orientation == "vertical") | (layout == "horizontal" & orientation == "horizontal")))
          | (task == "compare_length" & ((layout == "horizontal" & orientation == "vertical") | (layout == "vertical" & orientation == "horizontal"))),
        "congruent", "incongruent"
      ),
      condition = paste(orientation, layout, sep = "-"),
      answerSide = ifelse(
        (layout == "horizontal" & answer == 2) | (layout == "vertical" & answer == 1),
        "top/right", "bottom/left"
      ),
      directionalConflict = ifelse(
        (task == "compare_height" & ((layout == "horizontal" & orientation == "horizontal") | (layout == "vertical" & orientation == "vertical")))
          | (task == "compare_index" & ((layout == "vertical" & orientation == "horizontal") | (layout == "horizontal" & orientation == "vertical"))),
        "conflict_possible", "conflict_unlikely"
      ),
      answerInterference = ifelse(
        directionalConflict == "conflict_possible" & answerSide == "bottom/left", "interference", "no_interference"
      ),
      layout = recode(layout, "horizontal" = "side-by-side", "vertical" = "stacked"),
      isAnswerHigher = ifelse(answer == higherSide, TRUE, FALSE),
      isAnswerLater = ifelse(answer == laterSide, TRUE, FALSE),
      isAnswerLonger = ifelse(answer == longerSide, TRUE, FALSE),
      isAnswerDarker = ifelse(answer == darkerSide, TRUE, FALSE)
    ) %>%
    filter(!is.na(duration)) # Remove missing RT values
  
  # insert "block_order" column
  df <- df %>% 
    arrange(participant_id, time_when) %>%  # Sort trials by participant and time
    group_by(participant_id) %>%
    mutate(block_order = as.integer(factor(condition, levels = unique(condition)))) %>%
    ungroup()
  
  # converting binary factors to 1 and -1 for better linear model fit
  df <- df %>%
    mutate(taskCode = ifelse(task == "compare_index", 1, -1),
         layoutCode = ifelse(layout == "stacked", 1, -1),
         orientationCode = ifelse(orientation == "vertical", 1, -1),
         answerSideCode = ifelse(answerSide == "top/right", 1, -1),
         alignmentCode = ifelse(congruence == "congruent", 1, -1),
         isAnswerHigherCode = ifelse(isAnswerHigher, 1, -1),
         isAnswerDarkerCode = ifelse(isAnswerDarker, 1, -1),
         isAnswerLongerCode = ifelse(isAnswerLonger, 1, -1),
         isAnswerLaterCode = ifelse(isAnswerLater, 1, -1),
         difficultyCode = ifelse(difficulty==1, 1, -1))

  return(df)
}