knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(ggplot2)

# define models, groups, sizes, metrics, and panss measures
models <- c("gpt2", "qwen3", "falcon", "geitje")
groups <- c("control", "psychosis")
sizes <- c("10","20","30","40","50")
metrics <- c("mean_perp", "sd_perp", "min_perp", "max_perp")
metric_labels <- c(
  mean_perp = "mean perp.",
  sd_perp   = "SD perp.",
  min_perp  = "min perp.",
  max_perp  = "max perp."
)
#panss_cols <- c("panss_total", "panss_positive", "panss_negative", "panss_general")
panss_cols <- c("panss_positive", "panss_negative")
panss_labels <- c(
  panss_positive = "PANSS positive", 
  panss_negative = "PANSS negative"
)

# define directories
data_dir <- "perplexity_results_clean/"
output_dir <- "panss_outputs/"
if (!dir.exists("panss_outputs")) dir.create("panss_outputs")

# load clean data into a single dataframe per model for psychosis group
for (m in models) {
  local({
    combined_df <- map_dfr(sizes, function(s) {
      filename <- paste(m, "psychosis", s, "clean.csv", sep = "_")
      filepath <- paste0(data_dir, filename)
      df <- read_csv(filepath, show_col_types = FALSE)
      df$group <- "psychosis"   # constant now
      df$size <- as.factor(s)
      df
    })
    
    # Assign the combined dataframe to a variable per model
    assign(paste(m, "psychosis", sep = "_"), combined_df, envir = .GlobalEnv)
  })
}



# load edited metadata file
patient_data <- read.csv(file = "demographics/data_metadata_edited.csv", head = TRUE, sep = ",") %>%
  rename(id = participant) %>%
  mutate(across(any_of(panss_cols), as.numeric))


# loop over dataframes to calculate correlation for each perp and panss metric
for (m in models) {
  local({
    model_results <- NULL
    
    # get the combined dataframe for this model (all metrics are columns now)
    df_name <- paste(m, "psychosis", sep = "_")
    df <- get(df_name)
    
    merged_df <- df %>%
      inner_join(patient_data %>% select(id, all_of(panss_cols)), by = "id") %>%
      mutate(across(all_of(panss_cols), as.numeric))
    
    # loop over metrics as columns inside merged_df
    for (metric in metrics) {
      cor_results <- merged_df %>%
        group_by(size) %>%
        group_modify(~ {
          map_dfr(panss_cols, function(col) {
            res <- cor.test(.x[[metric]], .x[[col]], method = "pearson")
            broom::tidy(res) %>%
              rename(t_value = statistic, df = parameter) %>%
              mutate(PANSS = col)
          })
        }) %>%
        ungroup() %>%
        mutate(metric = metric) %>%
        select(metric, size, PANSS, estimate, conf.low, conf.high, p.value, t_value, df) %>%
        # apply FDR correction
        mutate(p.value_fdr = p.adjust(p.value, method = "fdr"))
      
      model_results <- bind_rows(model_results, cor_results)
    }
    
    obj_name <- paste("PANSS_cor_results", m, sep = "_")
    assign(obj_name, model_results, envir = .GlobalEnv)
    write_csv(model_results, file = paste0(output_dir, obj_name, ".csv"))
  })
}



# plot lineplots
for (m in models) {
  local({
    cor_df <- get(paste("PANSS_cor_results", m, sep = "_"))
    
    lineplot <- ggplot(cor_df, aes(x = size, y = estimate, color = PANSS, group = PANSS)) +
      geom_point() +
      geom_line() +
      geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
      geom_hline(yintercept = 0, linetype = "dotted", linewidth = 1) +
      facet_wrap(
        ~ metric,
        scales = "free_y",
        labeller = as_labeller(metric_labels)
      ) +
      scale_color_manual(
        values = c("panss_negative" = "#1A85FF", "panss_positive" = "#D41B55"),
        labels = panss_labels
      ) +
      theme_minimal() +
      labs(title = paste0("Correlation between perplexity metrics and PANSS scores (", toupper(m), ")"),
           x = "Window size",
           y = "Pearson correlation",
           color = "PANSS")
    
    print(lineplot)
    
    ggsave(filename = paste0(output_dir, "PANSS_lineplot", m, ".png"), 
           plot = lineplot, width = 10, height = 6)
  })
}


# plot heatmaps
all_results <- map_dfr(models, ~ get(paste("PANSS_cor_results", .x, sep = "_")))
global_min <- min(all_results$estimate, na.rm = TRUE)
global_max <- max(all_results$estimate, na.rm = TRUE)
limit <- max(abs(global_min), abs(global_max))

for (m in models) {
  local({
    cor_df <- get(paste("PANSS_cor_results", m, sep = "_")) %>%
      mutate(PANSS = factor(PANSS, levels = names(panss_labels)))
    
    heatmap <- ggplot(cor_df, aes(x = size, y = PANSS, fill = estimate)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "#1A85FF", 
                           mid = "#FFFFFF", 
                           high = "#D41B55", 
                           midpoint = 0,
                           limits = c(-limit, limit)) +
      facet_wrap(~ metric,
                 scales = "free",
                 labeller = as_labeller(metric_labels)) +
      scale_y_discrete(labels = panss_labels) +
      theme_minimal() +
      labs(title = paste0("Correlation between perplexity metrics and PANSS scores (", toupper(m), ")"),
           x = "Window Size",
           y = "PANSS Score",
           fill = "Correlation")
    
    print(heatmap)
    ggsave(filename = paste0(output_dir, "PANSS_heatmap_", m, ".png"), 
           plot = heatmap, width = 10, height = 6)
  })
}

