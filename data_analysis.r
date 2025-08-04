# demographics
# Author: Blanca Gonçalves
# Date: 2025-07-31

# Setup
library(dplyr)
library(rstatix)
library(stringr)
library(lubridate)
library(ggplot2)
library(pastecs)
library(effsize)

# Read data
data_raw <- read.csv(file = "datademographics.csv", head = TRUE, sep = ";")

# Rename columns
data_raw <- data_raw %>%
  rename(
    participant = praat_id,
    group = type_praat_participant,
    age = participant_age,
    gender = gender_participant,
    education = yoe_self_manual,
    mother_education = education_finished_mother,
    father_education = education_finished_father,
    duration = diag_startdate,
    primary_diagnosis = diag_primarydiagchoice,
    disorder = diag_psychoticdisorder,
    diagnosis_descriptive = diagnosis_descriptive,
    panss_total = panss_total_all,
    panss_positive = panss_totalpositive,
    panss_negative = panss_totalnegative,
    panss_general = panss_totalgeneral
  ) %>%
  select(-redcap_event_name)

# Clean and mutate
data_raw <- data_raw %>%
  mutate(
    duration = 2025 - as.numeric(substr(duration, 1, 4)),
    group = factor(group, levels = c(1, 2), labels = c("control", "psychosis")),
    gender = factor(gender, levels = c(1, 2, 4), labels = c("man", "woman", "trans man")),
    mother_education = ifelse(mother_education %in% c(0, 1), NA, mother_education),
    father_education = ifelse(father_education %in% c(0, 1), NA, father_education),
    mother_education = rowMeans(cbind(mother_education, father_education), na.rm = TRUE),
    mother_education = replace(mother_education, is.nan(mother_education), NA),
    primary_diagnosis = factor(primary_diagnosis, levels = c(1, 2), labels = c("mood", "psychosis")),
    disorder = factor(disorder, levels = 1:8, labels = c(
      "schizoaffective", "schizofrenia", "schizofreniform", "psychosis NOS",
      "short psychosis", "other", "delusional", "check"))
  ) %>%
  rename(avg_parent_edu = mother_education) %>%
  select(-father_education)

# Filter participants
keep_ids <- scan("saved_all.txt", what = "", quiet = TRUE)
data <- data_raw %>% filter(participant %in% keep_ids)
missing_ids <- setdiff(keep_ids, data_raw$participant)
print(missing_ids)
write.csv(data, "to_edit.csv", row.names = FALSE, fileEncoding = "UTF-8")

# Descriptives
data %>%
  group_by(group) %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE),
    mean_edu = mean(education, na.rm = TRUE),
    sd_edu = sd(education, na.rm = TRUE),
    mean_paredu = mean(avg_parent_edu, na.rm = TRUE),
    sd_paredu = sd(avg_parent_edu, na.rm = TRUE)
  )

# Gender distribution
data %>%
  group_by(group) %>%
  summarise(
    total = n(),
    n_women = sum(gender == "woman", na.rm = TRUE),
    percent_women = round(100 * n_women / total, 2)
  )

data %>%
  group_by(group, gender) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(group) %>%
  mutate(percent = round(100 * n / sum(n), 2)) %>%
  arrange(group, desc(n))

# Diagnosis distribution
data %>%
  filter(group == "psychosis") %>%
  count(disorder) %>%
  mutate(percent = round(100 * n / sum(n), 1)) %>%
  arrange(desc(n))

# PANSS descriptives
data %>%
  filter(group == "psychosis") %>%
  summarise(
    panss_total_mean = mean(panss_total, na.rm = TRUE),
    panss_total_sd   = sd(panss_total, na.rm = TRUE),
    panss_positive_mean = mean(panss_positive, na.rm = TRUE),
    panss_positive_sd   = sd(panss_positive, na.rm = TRUE),
    panss_negative_mean = mean(panss_negative, na.rm = TRUE),
    panss_negative_sd   = sd(panss_negative, na.rm = TRUE),
    panss_general_mean = mean(panss_general, na.rm = TRUE),
    panss_general_sd   = sd(panss_general, na.rm = TRUE)
  )

# AGE plots
ggplot(data, aes(x = age, fill = group)) +
  geom_histogram(binwidth = 1, color = "white", alpha = 0.8) +
  facet_wrap(~ group, ncol = 1) +
  scale_fill_manual(values = c("control" = "#1B55D4", "psychosis" = "#D41B55")) +
  labs(title = "Age distribution by group", x = "Age", y = "Count") +
  theme_minimal(base_size = 14) +
  theme(strip.text = element_text(face = "bold", size = 14), legend.position = "none")

ggplot(data, aes(x = age, fill = group)) +
  geom_histogram(alpha = 0.5, position = "identity", binswidth = 1) +
  scale_fill_manual(values = c("control" = "#1B55D4", "psychosis" = "#D41B55")) +
  labs(title = "Age Distribution by Group", x = "Age", y = "Count", fill = "Group") +
  theme_minimal(base_size = 14)

# AGE tests
print(by(data$age, data$group, stat.desc, basic = FALSE, norm = TRUE))
print(t.test(age ~ group, data = data))
print(wilcox.test(age ~ group, data = data))
print(cohen.d(age ~ group, data = data))
print(wilcox_effsize(data, age ~ group))

# EDUCATION plots
ggplot(data, aes(x = education, fill = group)) +
  geom_histogram(binwidth = 1, color = "white", alpha = 0.8) +
  facet_wrap(~ group, ncol = 1) +
  scale_fill_manual(values = c("control" = "#1B55D4", "psychosis" = "#D41B55")) +
  labs(title = "Education distribution by group", x = "Education", y = "Count") +
  theme_minimal(base_size = 14) +
  theme(strip.text = element_text(face = "bold", size = 14), legend.position = "none")

ggplot(data, aes(x = education, fill = group)) +
  geom_histogram(alpha = 0.5, position = "identity", binwidth = 1) +
  scale_fill_manual(values = c("control" = "#1B55D4", "psychosis" = "#D41B55")) +
  labs(title = "Education Distribution by Group", x = "Education", y = "Count", fill = "Group") +
  theme_minimal(base_size = 14)

# EDUCATION tests
print(by(data$education, data$group, stat.desc, basic = FALSE, norm = TRUE))
print(t.test(education ~ group, data = data))
print(wilcox.test(education ~ group, data = data))
print(cohen.d(education ~ group, data = data))
print(wilcox_effsize(data, education ~ group))

# PARENTAL EDUCATION plots
ggplot(data, aes(x = avg_parent_edu, fill = group)) +
  geom_histogram(binwidth = 1, color = "white", alpha = 0.8) +
  facet_wrap(~ group, ncol = 1) +
  scale_fill_manual(values = c("control" = "#1B55D4", "psychosis" = "#D41B55")) +
  labs(title = "Average Parent Education distribution by group", x = "Average Parent Education", y = "Count") +
  theme_minimal(base_size = 14) +
  theme(strip.text = element_text(face = "bold", size = 14), legend.position = "none")

ggplot(data, aes(x = avg_parent_edu, fill = group)) +
  geom_histogram(alpha = 0.5, position = "identity", binwidth = 1) +
  scale_fill_manual(values = c("control" = "#1B55D4", "psychosis" = "#D41B55")) +
  labs(title = "Average Parent Education Distribution by Group", x = "Average Parent Education", y = "Count", fill = "Group") +
  theme_minimal(base_size = 14)

# PARENTAL EDUCATION tests
print(by(data$avg_parent_edu, data$group, stat.desc, basic = FALSE, norm = TRUE))
print(t.test(avg_parent_edu ~ group, data = data))
print(wilcox.test(avg_parent_edu ~ group, data = data))
print(cohen.d(avg_parent_edu ~ group, data = data))
print(wilcox_effsize(data, avg_parent_edu ~ group))

# Chi-squared test for gender
chi_result <- chisq.test(table(data$gender, data$group))
print(chi_result)
