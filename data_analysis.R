knitr::opts_chunk$set(echo = TRUE)

# load libraries
library(dplyr)
library(psych)
library(rstatix)
library(stringr)
library(lubridate)
library(ggplot2)
library(pastecs)
library(effsize)
library(vcd)

# create output directory
if (!dir.exists("outputs")) dir.create("outputs")

# create saving functions
save_plot <- function(plot_obj, width = 8, height = 6) {
    name <- deparse(substitute(plot_obj)) 
  ggsave(filename = paste0("outputs/plot_", name, ".png"), plot = plot_obj, width = width, height = height, dpi = 300)
}

save_table <- function(table_obj) {
  name <- deparse(substitute(table_obj))  
  write.csv(table_obj, file = paste0("outputs/table_", name, ".csv"), row.names = FALSE)
}


save_test <- function(test_obj) {
  name <- deparse(substitute(test_obj))
  
  if (is.data.frame(test_obj) || is.matrix(test_obj)) {
    write.table(test_obj, 
                file = paste0("outputs/test_", name, ".txt"), 
                sep = "\t", 
                row.names = FALSE, 
                quote = FALSE)
  } else {
    capture.output(print(test_obj), 
                   file = paste0("outputs/test_", name, ".txt"))
  }
}


save_stats <- function(stat_obj, name = NULL) {
  if (is.null(name)) {
    name <- deparse(substitute(stat_obj))
  }
  
  if (is.data.frame(stat_obj)) {
    stats_df <- stat_obj
  } else {
    if (!is.list(stat_obj)) {
      stat_obj <- list(stat_obj)
      names(stat_obj) <- name
    }
    stats_df <- do.call(rbind, lapply(names(stat_obj), function(g) {
      df <- as.data.frame(t(stat_obj[[g]]))
      df$group <- g
      df
    }))
    stats_df <- stats_df[, c("group", setdiff(names(stats_df), "group"))]
  }
  
  write.csv(stats_df, paste0("outputs/table_", name, ".csv"), row.names = FALSE)
}


# read data
data_raw <- read.csv(file = "Praat-Blancademographics_DATA_2025-08-07_1952.csv", head = TRUE, sep = ";")

# rename columns
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

data_raw <- data_raw %>%
  mutate(
    duration = 2025 - as.numeric(substr(duration, 1, 4)),
    group = factor(group, 
                   levels = c(1, 2), 
                   labels = c("control", "psychosis")),
    gender = factor(gender, 
                    levels = c(1, 2, 4), 
                    labels = c("man", "woman", "trans man")),
    mother_education = ifelse(mother_education %in% c(0, 1), NA, mother_education),
    father_education = ifelse(father_education %in% c(0, 1), NA, father_education),
    mother_education = rowMeans(cbind(mother_education, father_education), na.rm = TRUE),
    mother_education = replace(mother_education, is.nan(mother_education), NA),
    primary_diagnosis = factor(primary_diagnosis, 
                               levels = c(1, 2), 
                               labels = c("mood", "psychosis")),
    disorder = factor(disorder, levels = c(1, 2, 3, 4, 5, 6, 7, 8),
                        labels = c(
                          "schizoaffective",
                          "schizophrenia",
                          "schizofreniform",
                          "psychosis NOS",
                          "psychosis NOS",
                          "psychosis NOS",
                          "psychosis NOS",
                          "psychosis NOS"
                        ))

  ) %>%
  rename(avg_parent_edu = mother_education) %>%
  select(-father_education)


# read list of IDs with transcripts from txt file
keep_ids <- scan("saved_all.txt", what = "", quiet = TRUE)

# keep only rows where participant ID is in keep_ids
data <- data_raw %>% 
  filter(participant %in% keep_ids)

# find missing IDs
missing_ids <- setdiff(keep_ids, data_raw$participant)
capture.output(print(missing_ids), file = "outputs/excluded_participants.txt")

# save filtered data for manual editing
write.csv(data, "data_to_edit.csv", row.names = FALSE, fileEncoding = "UTF-8")


# load edited data
data <- read.csv(file = "edited_data.csv", head = TRUE, sep = ",")

# separate groups
# actually unnecessary idk?
control_group <- subset(data, group == "control") 
psychosis_group <- subset(data, group == "psychosis") 

# descriptive stats for age, education, and parental education of both groups
general_stats <- data %>%
  group_by(group) %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE),
    mean_edu = mean(education, na.rm = TRUE),
    sd_edu = sd(education, na.rm = TRUE),
    mean_paredu = mean(avg_parent_edu, na.rm = TRUE),
    sd_paredu = sd(avg_parent_edu, na.rm = TRUE),
    mean_duration = mean(duration, na.rm = TRUE),
    sd_duration = sd(duration, na.rm = TRUE)
  )
print(general_stats)
save_stats(general_stats)


# gender distribution of both groups 
women_distribution <- data %>%
  group_by(group) %>%
  summarise(
    total = n(),
    n_women = sum(gender == "woman", na.rm = TRUE),
    percent_women = round(100 * n_women / total, 2)
  )
print(women_distribution)
save_stats(women_distribution)

gender_distribution <- data %>%
  group_by(group, gender) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(group) %>%
  mutate(percent = round(100 * n / sum(n), 2)) %>%
  arrange(group, desc(n))
print(gender_distribution)
save_stats(gender_distribution)


# distribution of diagnosis in psychosis group 
diagnosis_distribution <- data %>%
  filter(group == "psychosis") %>%
  count(disorder) %>%
  mutate(
    percent = round(100 * n / sum(n), 1)
  ) %>%
  arrange(desc(n))
print(diagnosis_distribution)
save_stats(diagnosis_distribution)


# descriptive stats of PANSS scores for psychosis group
panss_brief_stats <- data %>%
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
print(panss_brief_stats)
save_stats(panss_brief_stats)

panss_vars <- c("panss_total", "panss_positive", "panss_negative", "panss_general")

panss_statsdesc <- do.call(rbind, lapply(panss_vars, function(var) {
  stats <- stat.desc(data[[var]], basic = FALSE, norm = TRUE)
  df <- as.data.frame(t(stats))
  df$variable <- var
  df
}))

panss_statsdesc <- panss_statsdesc[, c("variable", setdiff(names(panss_statsdesc), "variable"))]
print(panss_statsdesc)
save_stats(panss_statsdesc)


# descriptive stats of duration of symptoms for psychosis group
duration_statsdesc <- data %>%
  filter(group == "psychosis") %>%
  pull(duration) %>%
  stat.desc(basic = FALSE, norm = TRUE)
print(duration_statsdesc)
save_stats(duration_statsdesc)

age_group_histogram <- ggplot(data, aes(x = age, fill = group)) +
  geom_histogram(binwidth = 1, color = "white", alpha = 0.8) +
  facet_wrap(~ group, ncol = 1) +
  scale_fill_manual(values = c("control" = "#1A85FF", "psychosis" = "#D41B55")) +
  scale_x_continuous(
    breaks = seq(20, 80, by = 5)  # breaks every 5 units from 20 to 80
  ) +
  labs(
    title = "Age distribution by group",
    x = "Age (years)",
    y = "Count"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold", size = 14),
    legend.position = "none"
  )
print(age_group_histogram)
save_plot(age_group_histogram)

# overlapping plots
age_group_histogram_overlap <- ggplot(data, aes(x = age, fill = group)) +
  geom_histogram(alpha = 0.5, position = "identity") +
  scale_fill_manual(values = c("control" = "#1B55D4", "psychosis" = "#D41B55")) +
  labs(
    title = "Age Distribution by Group",
    x = "Age",
    y = "Count",
    fill = "Group"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold", size = 14)
  )
# i thought overlapping plots would be cute but this is illegible 


# normality
age_group_statdesc <- by(data$age, data$group, stat.desc, basic = FALSE, norm = TRUE)
print(age_group_statdesc)
save_stats(age_group_statdesc)

# t-test for age 
age_group_ttest <- t.test(age ~ group, data = data)
print(age_group_ttest)
save_test(age_group_ttest)

# wilcoxon for age (because it is very not normal)
age_group_wilcox <- wilcox.test(age ~ group, data = data)
print(age_group_wilcox)
save_test(age_group_wilcox)

# size effect for t-test using cohen's d
age_group_cohend <- cohen.d(age ~ group, data = data)
print(age_group_cohend)
save_test(age_group_cohend)

# size effect for wilcoxon using r
age_group_reffect <- wilcox_effsize(data, age ~ group)
print(age_group_reffect)
save_test(age_group_reffect)

edu_group_histogram <- ggplot(data %>% filter(!is.na(education)), aes(x = education, fill = group)) +
  geom_histogram(binwidth = 1, color = "white", alpha = 0.8) +
  facet_wrap(~ group, ncol = 1) +
  scale_fill_manual(values = c("control" = "#1A85FF", "psychosis" = "#D41B55")) +
  labs(
    title = "Education distribution by group",
    x = "Education (years)",
    y = "Count"
  ) +
  scale_x_continuous(breaks = min(data$education, na.rm = TRUE):max(data$education, na.rm = TRUE)) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold", size = 14),
    legend.position = "none"
  )
save_plot(edu_group_histogram)
print(edu_group_histogram)

# overlapping plot is again illegible
edu_group_histogram_overlap <- ggplot(data, aes(x = education, fill = group)) +
  geom_histogram(alpha = 0.5, position = "identity", binwidth = 1) +
  scale_fill_manual(values = c("control" = "#1B55D4", "psychosis" = "#D41B55")) +
  labs(
    title = "Education distribution by group",
    x = "Education (years)",
    y = "Count",
    fill = "Group"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold", size = 14)
  )


# normality for education
edu_group_statdesc <- by(data$education, data$group, stat.desc, basic = FALSE, norm = TRUE)
print(edu_group_statdesc)
save_stats(edu_group_statdesc)

# t-test for education
edu_group_ttest <- t.test(education ~ group, data = data)
print(edu_group_ttest)
save_test(edu_group_ttest)

# wilcoxon test for education
edu_group_wilcox <- wilcox.test(education ~ group, data = data)
print(edu_group_wilcox)
save_test(edu_group_wilcox)

# effect size for t-test (Cohen's d)
edu_group_cohend <- cohen.d(education ~ group, data = data)
print(edu_group_cohend)
save_test(edu_group_cohend)

# effect size for wilcoxon test (r)
edu_group_reffect <- wilcox_effsize(data, education ~ group)
print(edu_group_reffect)
save_test(edu_group_reffect)

paredu_group_histogram <- ggplot(data %>% filter(!is.na(avg_parent_edu)), aes(x = avg_parent_edu, fill = group)) +
  geom_histogram(binwidth = 1, color = "white", alpha = 0.8) +
  facet_wrap(~ group, ncol = 1) +
  scale_fill_manual(values = c("control" = "#1A85FF", "psychosis" = "#D41B55")) +
  scale_x_continuous(
    breaks = seq(floor(min(data$avg_parent_edu, na.rm = TRUE)), 
                 ceiling(max(data$avg_parent_edu, na.rm = TRUE)), by = 1)
  ) +
  labs(
    title = "Average parental education distribution by group",
    x = "Parental education (years)",
    y = "Count"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold", size = 14),
    legend.position = "none"
  )
save_plot(paredu_group_histogram)
print(paredu_group_histogram)


# illegible overlapping plots
paredu_group_histogram_overlap <- ggplot(data, aes(x = avg_parent_edu, fill = group)) +
  geom_histogram(alpha = 0.5, position = "identity", binwidth = 1) +
  scale_fill_manual(values = c("control" = "#1B55D4", "psychosis" = "#D41B55")) +
  labs(
    title = "Average parental education distribution by group",
    x = "Parental education (years)",
    y = "Count",
    fill = "Group"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold", size = 14)
  )


# normality for avg_parent_edu
paredu_group_statdesc <- by(data$avg_parent_edu, data$group, stat.desc, basic = FALSE, norm = TRUE)
print(paredu_group_statdesc)
save_test(paredu_group_statdesc)

# t-test for avg_parent_edu
paredu_group_ttest <- t.test(avg_parent_edu ~ group, data = data)
print(paredu_group_ttest)
save_test(paredu_group_ttest)

# wilcoxon test for avg_parent_edu
paredu_group_wilcox <- wilcox.test(avg_parent_edu ~ group, data = data)
print(paredu_group_wilcox)
save_test(paredu_group_wilcox)

# effect size for t-test (Cohen's d)
paredu_group_cohend <- cohen.d(avg_parent_edu ~ group, data = data)
print(paredu_group_cohend)
save_test(paredu_group_cohend)

# effect size for wilcoxon test (r)
paredu_group_reffect <- wilcox_effsize(data, avg_parent_edu ~ group)
print(paredu_group_reffect)
save_test(paredu_group_reffect)


# contingency table
gender_table <- with(
  droplevels(subset(data, gender != "trans man")), 
  table(gender, group)
)

print(gender_table)
save_stats(gender_table)


df <- as.data.frame(gender_table)
colnames(df) <- c("Gender", "Group", "Count")

gender_group_barplot <- ggplot(df, aes(x = Gender, y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge", color = "white", alpha = 0.8) +
  facet_wrap(~ Group) +
  # scale_fill_manual(values = c("man" = "#2CA25F", "woman" = "#F16913")) +
  scale_fill_manual(values = c("man" = "#D41B55", "woman" = "#1A85FF")) +
  labs(
    title = "Counts of Gender by Group",
    x = "Gender",
    y = "Count"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold", size = 14),
    legend.position = "none"
  )
save_plot(gender_group_barplot)
print(gender_group_barplot)

ggplot(df, aes(x = Gender, y = Count, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", color = "white", alpha = 0.8) +
  facet_wrap(~ Group) +
  scale_fill_manual(values = c("control" = "#1A85FF", "psychosis" = "#D41B55")) +
  labs(
    title = "Counts of Gender by Group",
    x = "Gender",
    y = "Count"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold", size = 14),
    legend.position = "none"
  )

ggplot(df, aes(x = Group, y = Count, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", color = "white", alpha = 0.8) +
  facet_wrap(~ Gender) +
  scale_fill_manual(values = c("control" = "#1A85FF", "psychosis" = "#D41B55")) +
  labs(
    title = "Counts of Gender by Group",
    x = "Group",
    y = "Count"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold", size = 14),
    legend.position = "none"
  )



# chi squared for gender
gender_group_chisq <- chisq.test(gender_table)
print(gender_group_chisq)
save_test(gender_group_chisq)

# check expected counts
gender_expectedcounts <- gender_group_chisq$expected
print(gender_expectedcounts)
save_stats(gender_expectedcounts)

# fisher test just to check
gender_fisher <- fisher.test(gender_table)
save_test(gender_fisher)

# effect size (including phi)
gender_effsize <- assocstats(gender_table)
print(gender_effsize)
save_test(gender_effsize)

