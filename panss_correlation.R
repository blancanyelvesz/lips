knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(ggplot2)

# define models, groups, sizes, metrics, and panss measures
models <- c("gpt2", "qwen3", "falcon", "geitje")
groups <- c("control", "psychosis")
sizes <- c("10","20","30","40","50")
metrics <- c("word_mean", "word_std", "word_min", "word_max")
panss_cols <- c("panss_total", "panss_positive", "panss_negative", "panss_general")

# define directories
data_dir <- "perplexity_results_clean/"
output_dir <- "panss_outputs/"
if (!dir.exists("panss_outputs")) dir.create("panss_outputs")

# load clean data into a single dataframe per model and metric for psychosis group
for (metric in metrics) {
  for (m in models) {
    local({
      combined_df <- map_dfr(sizes, function(s) {
        filename <- paste(m, "psychosis", s, metric, "clean.csv", sep = "_")
        filepath <- paste0(data_dir, filename)
        df <- read_csv(filepath, show_col_types = FALSE)
        df$group <- "psychosis"   # constant now
        df$size <- as.factor(s)
        df
      })
      
      assign(paste(m, metric, sep = "_"), combined_df, envir = .GlobalEnv)
    })
  }
}


# load edited metadata file
patient_data <- read.csv(file = "demographics/data_metadata_edited.csv", head = TRUE, sep = ",") %>%
  rename(id = participant) %>%
  mutate(across(panss_cols, as.numeric))


# loop over dataframes to calculate correlation for each perp and panss metric
for (m in models) {
  all_results <- NULL
  
  for (metric in metrics) {
    df_name <- paste(m, metric, sep = "_")
    df <- get(df_name)
    
    merged_df <- df %>%
      inner_join(patient_data %>% select(id, all_of(panss_cols)), by = "id") %>%
      mutate(across(all_of(panss_cols), as.numeric))
    
    #cat("\n=== Model:", m, "| Metric:", metric, "| Rows:", nrow(merged_df), "===\n")
    
    cor_results <- merged_df %>%
      group_by(size) %>%
      group_modify(~ {
        map_dfr(panss_cols, function(col) {
          res <- cor.test(.x[[metric]], .x[[col]], method = "pearson")
          tidy(res) %>%
            rename(t_value = statistic, df = parameter) %>%
            mutate(PANSS = col)
        })
      }) %>%
      mutate(metric = metric) %>%
      select(metric, size, PANSS, estimate, conf.low, conf.high, p.value, t_value, df)
    
    all_results <- bind_rows(all_results, cor_results)
  }
  
  filename <- paste(m, "cor_results", sep = "_")
  assign(filename, all_results, envir = .GlobalEnv)
  write_csv(all_results, file = paste0(output_dir, filename, ".csv"))
}


# plot lineplots with confidence intervals
for (m in models) {
  local({
    df <- get(paste0(m, "_cor_results"))
    
    plot <- ggplot(df, aes(x = size, y = estimate, color = PANSS, group = PANSS)) +
      geom_point() +
      geom_line() +
      geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
      facet_wrap(~ metric, scales = "free_y") +
      theme_minimal() +
      labs(
        title = paste("Correlation between perplexity and PANSS scores:", toupper(m)),
        x = "Window Size",
        y = "Pearson correlation",
        color = "PANSS"
      )
    
    print(plot)
    ggsave(paste0(output_dir, m, "_cor_lineplot.png"), plot, width = 10, height = 6)
  })
}



# plot heatmaps 
for (m in models) {
  local({
    df <- get(paste0(m, "_cor_results"))
    
    p <- ggplot(df, aes(x = size, y = PANSS, fill = estimate)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "#1A85FF", mid = "white", high = "#D41B55", midpoint = 0) +
      facet_wrap(~ metric, scales = "free") +
      theme_minimal() +
      labs(title = paste("Correlation between perplexity and PANSS scores:", toupper(m)),
           x = "Window Size",
           y = "PANSS score",
           fill = "Correlation")
    
    print(p)
    ggsave(paste0(output_dir, m, "_cor_heatmap.png"), p, width = 10, height = 6)
  })
}

