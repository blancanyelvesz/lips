# demographics.R

# Author: Blanca Gonçalves
# Date: 2025-07-31

# --- Setup -------------------------------------------------------------------
# Load libraries
library(dplyr)
library(rstatix)
library(stringr)
library(lubridate)
library(ggplot2)
library(pastecs)
library(effsize)

# --- Load Data ---------------------------------------------------------------
data_raw <- read.csv(file = "Praat-Blancademographics_DATA_2025-08-07_1952.csv", 
                     head = TRUE, sep = ";")

# --- Rename Columns ----------------------------------------------------------
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

# --- Recode and clean variables ----------------------------------------------
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
    disorder = factor(disorder, levels = 1:8,
                      labels = c("schizoaffective", "schizophrenia", "schizofreniform",
                                 "psychosis NOS", "psychosis NOS", "psychosis NOS",
                                 "psychosis NOS", "psychosis NOS"))
  ) %>%
  rename(avg_parent_edu = mother_education) %>%
  select(-father_education)

# --- Filter participants with transcripts ------------------------------------
keep_ids <- scan("saved_all.txt", what = "", quiet = TRUE)

data <- data_raw %>% 
  filter(participant %in% keep_ids)

# Find missing
missing_ids <- setdiff(keep_ids, data_raw$participant)
print(missing_ids)

# Save for manual editing
write.csv(data, "to_edit.csv", row.names = FALSE, fileEncoding = "UTF-8")

# --- Load manually edited data -----------------------------------------------
data <- read.csv(file = "edited_data.csv", head = TRUE, sep = ",")

# --- Descriptives for age, education, parental education ---------------------
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

# --- Gender distribution -----------------------------------------------------
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

# --- Diagnosis distribution in psychosis group -------------------------------
data %>%
  filter(group == "psychosis") %>%
  count(disorder) %>%
  mutate(percent = round(100 * n / sum(n), 1)) %>%
  arrange(desc(n))

# --- PANSS scores descriptives -----------------------------------------------
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

# --- AGE Analysis ------------------------------------------------------------
ggplot(data, aes(x = age, fill = group)) +
  geom_histogram(binwidth = 1, color = "white", alpha = 0.8) +
  facet_wrap(~ group, ncol = 1) +
  scale_fill_manual(values = c("control" = "#1B55D4", "psychosis" = "#D41B55")) +
  labs(title = "Age distribution by group", x = "Age", y = "Count") +
  theme_minimal(base_size = 14) +
  theme(strip.text = element_text(face = "bold", size = 14),
        legend.position = "none")

# Normality and tests
statdesc_age <- by(data$age, data$group, stat.desc, basic = FALSE, norm = TRUE)
print(statdesc_age)

t_test_age <- t.test(age ~ group, data = data)
print(t_test_age)

wilcox_age <- wilcox.test(age ~ group, data = data)
print(wilcox_age)

cohen_d_age <- cohen.d(age ~ group, data = data)
print(cohen_d_age)

r_age <- wilcox_effsize(data, age ~ group)
print(r_age)

# --- EDUCATION Analysis ------------------------------------------------------
ggplot(data, aes(x = education, fill = group)) +
  geom_histogram(binwidth = 1, color = "white", alpha = 0.8) +
  facet_wrap(~ group, ncol = 1) +
  scale_fill_manual(values = c("control" = "#1B55D4", "psychosis" = "#D41B55")) +
  labs(title = "Education distribution by group", x = "Education", y = "Count") +
  theme_minimal(base_size = 14) +
  theme(strip.text = element_text(face = "bold", size = 14),
        legend.position = "none")

statdesc_education <- by(data$education, data$group, stat.desc, basic = FALSE, norm = TRUE)
print(statdesc_education)

t_test_education <- t.test(education ~ group, data = data)
print(t_test_education)

wilcox_education <- wilcox.test(education ~ group, data = data)
print(wilcox_education)

cohen_d_education <- cohen.d(education ~ group, data = data)
print(cohen_d_education)

r_education <- wilcox_effsize(data, education ~ group)
print(r_education)

# --- PARENTAL EDUCATION Analysis ---------------------------------------------
ggplot(data, aes(x = avg_parent_edu, fill = group)) +
  geom_histogram(binwidth = 1, color = "white", alpha = 0.8) +
  facet_wrap(~ group, ncol = 1) +
  scale_fill_manual(values = c("control" = "#1B55D4", "psychosis" = "#D41B55")) +
  labs(title = "Average Parent Education distribution by group",
       x = "Average Parent Education", y = "Count") +
  theme_minimal(base_size = 14) +
  theme(strip.text = element_text(face = "bold", size = 14),
        legend.position = "none")

statdesc_avg_parent_edu <- by(data$avg_parent_edu, data$group, stat.desc, basic = FALSE, norm = TRUE)
print(statdesc_avg_parent_edu)

t_test_avg_parent_edu <- t.test(avg_parent_edu ~ group, data = data)
print(t_test_avg_parent_edu)

wilcox_avg_parent_edu <- wilcox.test(avg_parent_edu ~ group, data = data)
print(wilcox_avg_parent_edu)

cohen_d_avg_parent_edu <- cohen.d(avg_parent_edu ~ group, data = data)
print(cohen_d_avg_parent_edu)

r_avg_parent_edu <- wilcox_effsize(data, avg_parent_edu ~ group)
print(r_avg_parent_edu)

# --- GENDER Analysis ---------------------------------------------------------
gender_table <- with(droplevels(subset(data, gender != "trans man")), 
                     table(gender, group))

print(gender_table)

df <- as.data.frame(gender_table)
colnames(df) <- c("Gender", "Group", "Count")

ggplot(df, aes(x = Group, y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge", color = "white", alpha = 0.8) +
  scale_fill_manual(values = c("man" = "#1A85FF", "woman" = "#D41159")) +
  labs(title = "Counts of Gender by Group", x = "Group", y = "Count") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right")

ggplot(df, aes(x = Group, y = Count, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", color = "white", alpha = 0.8) +
  facet_wrap(~ Gender) +
  scale_fill_manual(values = c("control" = "#1B55D4", "psychosis" = "#D41B55")) +
  labs(title = "Counts of Gender by Group", x = "Group", y = "Count") +
  theme_minimal(base_size = 14) +
  theme(strip.text = element_text(face = "bold", size = 14),
        legend.position = "none")

chisq_result <- chisq.test(gender_table)
print(chisq_result)
