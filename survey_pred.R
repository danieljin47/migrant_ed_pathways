library(googlesheets4)
library(tidyverse)
library(caret)
sheet_df <- read_sheet("https://docs.google.com/spreadsheets/d/14o2AP8JwCpbuttgh4QqvCQoi86B_bbsK7ZiD4vOU5BI/edit#gid=0")

#colSums(!is.na(sheet_df))
#unique
#count
#filter

# sheet_df2 <-
#   sheet_df %>% select(-c(return_time, voc_reason, resources))

pre_survey <- 
  sheet_df %>% 
  pivot_longer(q1:q13, names_to = "questions", values_to = "responses")

measure_mean <-
  pre_survey %>% 
  mutate(measure = case_when(
    questions %in% c("q1", "q6", "q7") ~ "par",
    questions %in% c("q2", "q8", "q9") ~ "rel",
    questions %in% c("q3", "q10") ~ "tea",
    questions %in% c("q4", "q12") ~ "peer",
    questions %in% c("q5", "q13") ~ "none",
    TRUE ~ NA_character_
  ))

measure_mean <-
  measure_mean %>% 
  group_by(student_id, measure) %>% 
  mutate(mean_response = mean(responses, na.rm = TRUE))

measure_mean <-
  measure_mean %>%
  pivot_wider(names_from = c("questions"), values_from = c("responses"))

measure_mean <-
  measure_mean %>% select(-(q1:q13))

measure_mean <-
  measure_mean %>% 
  pivot_wider(names_from = "measure", values_from = "mean_response")

measure_mean <-
  measure_mean %>% select(-(`NA`))

measure_mean <- measure_mean[-73,]

ssi_mean <-
  measure_mean %>%
  group_by(student_id) %>%
  mutate(ssi = mean(c(par,rel,tea,peer), na.rm = TRUE))

ssi_mean <-
  ssi_mean %>% select(-(par:none))

fsi_mean <-
  ssi_mean %>% 
  group_by(student_id) %>% 
  mutate(fsi = mean(c(fs1,fs2,fs3,fs4), na.rm = TRUE))

# vocational <-
#   fsi_mean %>% filter(apply_voc >= 4)

dat <-
  fsi_mean %>% select(
    hukou,
    return_b4,
    grad_plan, 
    voc_imp,
    ssi,
    fsi
  )

#dat$grad_plan <- relevel(as.factor(dat$grad_plan), ref = "stay")

dat <-
  dat %>% mutate_if(is.character, as.factor)

dat <- na.omit(dat)
  
train_control <- 
  trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 10
  )

set.seed(3060)  
fit <- train(grad_plan ~ ., 
             data = dat, 
             method = "multinom", 
             trControl = train_control, 
             trace = FALSE)

fit

fit$finalModel

varImp(fit)
