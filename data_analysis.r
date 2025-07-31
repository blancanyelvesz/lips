#2025.08.01

# Load libraries
library(dplyr)
library(stringr)
library(lubridate)

# Read data
data <- read.csv(file = "datademographics3.csv", head = TRUE, sep = ";")

# Rename columns with dplyr::rename()
data <- data %>%
  rename(
    participant = praat_id,
    group = type_praat_participant,
    age = participant_age,
    gender = gender_participant,
    education = yoe_self_manual,
    mother_education = education_finished_mother,
    father_education = education_finished_father,
    start_date = diag_startdate,
    primary_diagnosis = diag_primarydiagchoice,
    disorder = diag_psychoticdisorder,
    diagnosis_descriptive = diagnosis_descriptive,
    PANSS_total = panss_total_all,
    PANSS_positive = panss_totalpositive,
    PANSS_negative = panss_totalnegative,
    PANSS_general = panss_totalgeneral
  ) %>%
  select(-redcap_event_name) 

# Edit columns
data <- data %>%
  mutate(
    duration = as.numeric(format(Sys.Date(), "%Y")) - as.numeric(substr(start_date, 1, 4))
  ) %>%
  mutate(
    group = factor(group, levels = c(1, 2), labels = c("control", "psychosis"))
  ) %>%
  mutate(
    gender = factor(gender, levels =c(1, 2, 4), labels = c("man", "woman", "trans man"))
  ) 


# Read list of IDs from txt file
keep_ids <- scan("saved_all.txt", what = "", quiet = TRUE)

# Keep only rows where participant ID is in keep_ids
data_filtered <- data %>% 
  filter(participant %in% keep_ids)

# Find missing IDs
missing_ids <- setdiff(keep_ids, data$participant)

missing_ids

control_group <- subset(data_filtered, group == "control") 
psychosis_group <- subset(data_filtered, group == "psychosis") 


