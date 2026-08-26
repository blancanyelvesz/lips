knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(report)
library(readxl)
library(grid)

# define models, groups, sizes, metrics, and panss measures
models <- c("gpt2", "falcon", "qwen3", "geitje")
groups <- c("control", "psychosis")
sizes <- c("10","20","30","40","50")
metrics <- c("mean_perp", "sd_perp", "min_perp", "max_perp")
metric_labels <- c(
  mean_perp = "mean perp.",
  sd_perp   = "SD perp.",
  min_perp  = "minimum perp.",
  max_perp  = "maximum perp."
)
#panss_cols <- c("panss_total", "panss_positive", "panss_negative", "panss_general")
panss_cols <- c("panss_positive", "panss_negative")
panss_labels <- c(
  panss_positive = "PANSS pos.", 
  panss_negative = "PANSS neg."
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


# load new metadata xlsx and keep subject + individual PANSS items


panss_item_cols <- c(
  paste0("panss_p", 1:7),
  paste0("panss_n", 1:7)
)

patient_PANSS_items <- readxl::read_xlsx("demographics/metadata_detailed.xlsx") %>%
  rename(id = subject) %>%
  select(id, all_of(panss_item_cols)) %>%
  mutate(across(all_of(panss_item_cols), as.numeric))

# restricted analysis for individual PANSS items
models_item <- c("falcon", "geitje")
metrics_item <- c("mean_perp", "max_perp", "sd_perp")
metric_labels_item <- c(
  mean_perp = "mean perp.",
  max_perp  = "maximum perp.",
  sd_perp = "SD perp."
)
panss_item_labels <- c(
  panss_p1 = "P1",
  panss_p2 = "P2",
  panss_p3 = "P3",
  panss_p4 = "P4",
  panss_p5 = "P5",
  panss_p6 = "P6",
  panss_p7 = "P7",
  panss_n1 = "N1",
  panss_n2 = "N2",
  panss_n3 = "N3",
  panss_n4 = "N4",
  panss_n5 = "N5",
  panss_n6 = "N6",
  panss_n7 = "N7"
)

# panss_item_labels <- c(
#   panss_p1 = "Delusions",
#   panss_p2 = "Disorganised thought",
#   panss_p3 = "Hallucinations",
#   panss_p4 = "Excitement",
#   panss_p5 = "Grandiosity",
#   panss_p6 = "Suspiciousness",
#   panss_p7 = "Hostility",
#   panss_n1 = "Blunted affect",
#   panss_n2 = "Emotional withdrawal",
#   panss_n3 = "Poor rapport",
#   panss_n4 = "Social withdrawal",
#   panss_n5 = "Abstract thought",
#   panss_n6 = "Lack of spontaneity",
#   panss_n7 = "Stereotyped thought"
# )

# loop over dataframes to calculate correlation for each perp and individual panss item
for (m in models_item) {
  local({
    model_results <- NULL
    
    df_name <- paste(m, "psychosis", sep = "_")
    df <- get(df_name)
    
    merged_df <- df %>%
      inner_join(patient_PANSS_items %>% select(id, all_of(panss_item_cols)), by = "id") %>%
      mutate(across(all_of(panss_item_cols), as.numeric))
    
    for (metric in metrics_item) {
      cor_results <- merged_df %>%
        group_by(size) %>%
        group_modify(~ {
          map_dfr(panss_item_cols, function(col) {
            res <- cor.test(.x[[metric]], .x[[col]], method = "pearson")
            
            broom::tidy(res) %>%
              rename(t_value = statistic, df = parameter) %>%
              mutate(PANSS = col)
          })
        }) %>%
        ungroup() %>%
        mutate(metric = metric) %>%
        select(metric, size, PANSS, estimate, conf.low, conf.high, p.value, t_value, df) %>%
        mutate(p.value_fdr = p.adjust(p.value, method = "fdr"))
      
      model_results <- bind_rows(model_results, cor_results)
    }
    
    obj_name <- paste("PANSS_item_cor_results", m, sep = "_")
    assign(obj_name, model_results, envir = .GlobalEnv)
    write_csv(model_results, file = paste0(output_dir, obj_name, ".csv"))
  })
}

# Combine all models’ individual-item correlation results
all_item_results <- map_dfr(models_item, ~ {
  get(paste("PANSS_item_cor_results", .x, sep = "_")) %>%
    mutate(model = toupper(.x))
})

# Get global limits for consistent color scaling
global_min <- min(all_item_results$estimate, na.rm = TRUE)
global_max <- max(all_item_results$estimate, na.rm = TRUE)
limit <- max(abs(global_min), abs(global_max))

# Desired facet order
model_order <- c("FALCON", "GEITJE")
# Desired PANSS display order (top to bottom in the heatmap)
panss_item_order <- c(
  "panss_p1", "panss_p2", "panss_p3", "panss_p4", "panss_p5", "panss_p6", "panss_p7",
  "panss_n1", "panss_n2", "panss_n3", "panss_n4", "panss_n5", "panss_n6", "panss_n7"
)

# Loop over metrics
for (metric_name in unique(all_item_results$metric)) {
  local({
    metric_df <- all_item_results %>%
      filter(metric == metric_name) %>%
      mutate(
        PANSS = factor(PANSS, levels = panss_item_cols),
        model = factor(model, levels = model_order),
        stars = case_when(
          p.value < 0.001 ~ "***",
          p.value < 0.01  ~ "**",
          p.value < 0.05  ~ "*",
          TRUE ~ ""
        ),
        label = paste0(round(estimate, 2), stars)
      )
    
    heatmapscores <- ggplot(metric_df, 
                            aes(x = size, y = forcats::fct_rev(PANSS), fill = estimate)) +
      geom_tile(color = "white") +
      geom_text(aes(label = label), color = "black", size = 3) +
      scale_fill_gradient2(
        low = "#1A85FF",
        mid = "#FFFFFF",
        high = "#D41B55",
        midpoint = 0,
        limits = c(-limit, limit)
      ) +
      facet_wrap(
        ~ model,
        scales = "free",
        labeller = label_value
      ) +
      scale_y_discrete(labels = panss_item_labels) +
      theme_minimal(base_size = 14) +
      theme(
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 11),
        legend.key.size = unit(1, "lines"),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        strip.text = element_text(size = 14)
      ) +
      labs(
        title = paste0(
          "Correlation between ", metric_labels_item[[metric_name]],
          " and individual PANSS items across models"
        ),
        x = "Window Size",
        y = "PANSS Item",
        fill = "Correlation"
      )
    
    print(heatmapscores)
    ggsave(
      filename = paste0(output_dir, "PANSS_item_heatmapscores_", metric_name, ".png"),
      plot = heatmapscores,
      width = 10,
      height = 6,
      dpi = 300
    )
  })
}
