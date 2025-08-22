knitr::opts_chunk$set(echo = TRUE)
library(lme4)
library(lmerTest)
library(dplyr)
library(readr)
library(emmeans)
emm_options(pbkrtest.limit = 4000, lmerTest.limit = 4000)

library(ggplot2)

# define models, groups, sizes, metrics
models <- c("gpt2", "qwen3", "falcon", "geitje")
groups <- c("control", "psychosis")
sizes <- c("10","20","30","40","50")
metrics <- c("word_mean", "word_std", "word_min", "word_max")

# define directories
data_directory <- "clean_perp_data/"
output_directory <- "models_outputs/"
if (!dir.exists("models_outputs")) dir.create("models_outputs")

# load all clean data into a single dataframe per LLM and metric
for (metric in metrics) {
  for (m in models) {
  local({
    combined_df <- map_dfr(groups, function(g) {
      map_dfr(sizes, function(s) {
        filename <- paste(m, g, s, metric, "clean.csv", sep = "_")
        filepath <- paste0(data_directory, filename)
        df <- read_csv(filepath, show_col_types = FALSE)
        df$group <- as.factor(g)
        df$size <- as.factor(s)
        df
      })
    })
    
    assign(paste(m, metric, "combined", sep = "_"), combined_df, envir = .GlobalEnv)
  })
  }
}

# str(gpt2_word_mean_combined)

# fit a mixed-effects model per LLM and metric
for (metric in metrics) {
  for (m in models) {
    local({
      df_name <- paste(m, metric, "combined", sep = "_")
      df <- get(df_name)
      
      # random intercept per subject (id)
      formula_str <- paste(metric, "~ group * size + (1|id)")
      formula_obj <- as.formula(formula_str)
      
      model <- lmer(formula_obj, data = df)
      
      model_name <- paste("model", m, metric, sep = "_")
      assign(model_name, model, envir = .GlobalEnv)
      
      # save to text file
      filepath <- paste0(output_directory, model_name, "_summary.txt")
      sink(filepath, split = TRUE)
      cat("Model name:", model_name, "\n")
      cat("Formula:", formula_str, "\n\n")
      print(summary(model))
      cat("\n\n")
      sink()
      rm(model)
    })
  }
}


# plot predictions
# loop over metrics and models
for(metric in metrics) {
  for(m in models) {
    model_name <- paste0("model_", m, "_", metric)
    model <- get(model_name)
    
    # estimated marginal means
    emm <- emmeans(model, ~ group * size)
    emm_df <- as.data.frame(emm)
    
    # compute confidence intervals if not present
    if(!all(c("lower.CL", "upper.CL") %in% colnames(emm_df))) {
      emm_df <- emm_df %>%
        mutate(lower.CL = emmean - SE,
               upper.CL = emmean + SE)
    }
    
    # build the plot
    plot <- ggplot(emm_df, aes(x = size, y = emmean, color = group, group = group)) +
      geom_line(size = 1.5) +
      geom_point(size = 3, shape = 21, fill = "white") +
      geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.15, size = 0.8) +
      scale_color_manual(values = c("control" = "#1f77b4", "psychosis" = "#D41B55")) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "top",
            panel.grid.major = element_line(color = "grey85"),
            panel.grid.minor = element_blank()) +
      labs(title = paste0("Predicted ", metric, " by group and window size (", m, ")"),
           x = "Window Size",
           y = paste0("Predicted ", metric),
           color = "Group")
    
    # save the plot
    ggsave(filename = paste0(output_directory, m, "_", metric, "_predicted.png"),
           plot = plot, width = 10, height = 10)
    
    # print plot to console
    print(plot)
  }
}
