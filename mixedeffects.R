knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
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
metrics <- c("mean_perp", "sd_perp", "min_perp", "max_perp")
metric_labels <- c(
  mean_perp = "mean perp.",
  sd_perp   = "SD perp.",
  min_perp  = "min perp.",
  max_perp  = "max perp."
)

# define directories
data_dir <- "perplexity_results_clean/"
output_dir <- "models_outputs/"
if (!dir.exists("models_outputs")) dir.create("models_outputs")

# load all clean data into a single dataframe per LLM 
for (m in models) {
  local({
    combined_df <- map_dfr(groups, function(g) {
      map_dfr(sizes, function(s) {
        filename <- paste(m, g, s, "clean.csv", sep = "_")  # one CSV per model-group-size
        filepath <- paste0(data_dir, filename)
        df <- read_csv(filepath, show_col_types = FALSE)
        df$group <- as.factor(g)
        df$size <- as.factor(s)
        df
      })
    })
    
    # assign combined dataframe (all metrics included) to global env
    assign(paste(m, "combined", sep = "_"), combined_df, envir = .GlobalEnv)
  })
}


# fit a mixed-effects model per LLM and metric
for (metric in metrics) {
  for (m in models) {
    local({
      df_name <- paste(m, "combined", sep = "_")
      df <- get(df_name)
      
      # random intercept per subject (id)
      formula_str <- paste(metric, "~ group * size + (1|id)")
      formula_obj <- as.formula(formula_str)
      
      model <- lmer(formula_obj, data = df)
      
      model_name <- paste("model", m, metric, sep = "_")
      assign(model_name, model, envir = .GlobalEnv)
      
      filepath <- paste0(output_dir, model_name, "_summary.txt")
      sink(filepath)
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
for (metric in metrics) {
  for (m in models) {
    local({
      model_name <- paste0("model_", m, "_", metric)
      model <- get(model_name)
      
      # estimated marginal means
      emm <- emmeans(model, ~ group * size)
      emm_df <- as.data.frame(emm)
      
      # compute confidence intervals if not present
      if (!all(c("lower.CL", "upper.CL") %in% colnames(emm_df))) {
        emm_df <- emm_df %>%
          mutate(lower.CL = emmean - SE,
                 upper.CL = emmean + SE)
      }
      
      metric_label <- metric_labels[metric]
      
      # build the plot
      predicted_plot <- ggplot(emm_df, aes(x = size, y = emmean, color = group, group = group)) +
        geom_line(size = 1.5) +
        geom_point(size = 3, shape = 21, fill = "white") +
        geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.15, size = 0.8) +
        scale_color_manual(values = c("control" = "#1A85FF", "psychosis" = "#D41B55")) +
        theme_minimal(base_size = 14) +
        theme(legend.position = "top",
              panel.grid.major = element_line(color = "grey85"),
              panel.grid.minor = element_blank()) +
        labs(title = paste0("Predicted ", metric_label, " by group and window size (", m, ")"),
             x = "Window Size",
             y = paste0("Predicted ", metric_label),
             color = "Group")
      
      print(predicted_plot)
      ggsave(filename = paste0(output_dir, "predicted_", m, "_", metric, ".png"),
             plot = predicted_plot, width = 10, height = 10)
    })
  }
}


# plot psychosis-control differences per size + significance
sink(paste0(output_dir, "size_differences_results.txt"))
cat("Size-wise Group Differences Results\n")
cat("===================================\n\n")

for (metric in metrics) {
  for (m in models) {
    local ({
      model_name <- paste("model", m, metric, sep = "_")
      model <- get(model_name)
      
      metric_label <- metric_labels[metric]
      cat("Metric:", metric_label, "| Model:", m, "\n")
      
      # estimated marginal means
      emm <- emmeans(model, ~ group * size)
      emm_df <- as.data.frame(emm) %>%
        mutate(size = as.numeric(as.character(size)))
      
      # pairwise contrasts psychosis - control per size
      contrasts <- contrast(emm, method = "revpairwise", by = "size", adjust = "none")
      contrasts_df <- as.data.frame(contrasts)
      
      # prepare plotting data
      diffsize_df <- contrasts_df %>%
        filter(contrast == "psychosis - control") %>%
        select(size, estimate, p.value) %>%
        rename(diff = estimate, pvalue = p.value) %>%
        mutate(stars = case_when(
          pvalue < 0.001 ~ "***",
          pvalue < 0.01 ~ "**",
          pvalue < 0.05 ~ "*",
          TRUE ~ ""
        ))
      
      # print results
      for(i in 1:nrow(diffsize_df)) {
        cat("Size", as.character(diffsize_df$size[i]), 
            ": Diff =", round(diffsize_df$diff[i], 3), 
            ", p =", round(diffsize_df$pvalue[i], 4), diffsize_df$stars[i], 
            "\n")
      }
      
      cat(paste(rep("-", 50), collapse = ""), "\n")
      
      # plot with significance stars
      diffsize_plot <- ggplot(diffsize_df, aes(x = size, y = diff)) +
        geom_col(fill = "#D41B55", alpha = 0.8) +
        geom_text(aes(label = paste0(round(diff, 2), stars)), 
                  vjust = -0.2) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
        theme_minimal(base_size = 14) +
        labs(title = paste("Group difference in", metric_label, "per window size (", m, ")"),
             x = "Window size",
             y = "Difference (psychosis - control)")
      
      print(diffsize_plot)
      
      # save plot
      filename <- paste0(output_dir, "diffsize_", metric, "_", m, ".png")
      ggsave(filename, plot = diffsize_plot, width = 10, height = 6, dpi = 300)
    })
  }
}

sink()

# plot psychosis-control differences per step (change between sizes) + significance
sink(paste0(output_dir, "step_differences_results.txt"))
cat("Step-wise Group Differences Results\n")
cat("===================================\n\n")

for (metric in metrics) {
  for (m in models) {
    local ({
      model_name <- paste("model", m, metric, sep = "_")
      model <- get(model_name)
      
      metric_label <- metric_labels[metric]
      cat("Metric:", metric_label, "| Model:", m, "\n")
      
      # estimated marginal means
      emm <- emmeans(model, ~ group * size)
      emm_df <- as.data.frame(emm) %>%
        mutate(size = as.numeric(as.character(size)))
      
      # calculate delta = size_i - size_(i-1)
      steps_df <- emm_df %>%
        group_by(group) %>%
        mutate(prev_emmean = lag(emmean),
               prev_size = lag(size)) %>%
        filter(!is.na(prev_emmean)) %>%
        mutate(delta = emmean - prev_emmean,
               step = paste0(prev_size, " → ", size)) %>%
        ungroup() %>%
        mutate(step = factor(step, levels = unique(step)))
      
      # get difference of delta between groups
      diffsteps_df <- steps_df %>%
        select(group, step, delta) %>%
        pivot_wider(names_from = group, values_from = delta) %>%
        mutate(diff = NA,
               pvalue = NA, 
               stars = "")
      
      # test significance for each step
      for(i in 1:(length(sizes)-1)) {
        prev_size <- sizes[i]
        curr_size <- sizes[i+1]
        
        # find the row indices for each group/size combination
        curr_psychosis <- which(emm_df$group == "psychosis" & emm_df$size == curr_size)
        prev_psychosis <- which(emm_df$group == "psychosis" & emm_df$size == prev_size)
        curr_control <- which(emm_df$group == "control" & emm_df$size == curr_size)
        prev_control <- which(emm_df$group == "control" & emm_df$size == prev_size)
        
        # create contrast vector: psychosis_delta - control_delta = 
        # = psychosis_i - psychosis_i-1 - (control_i - control_i-1)
        contrast_vec <- rep(0, nrow(emm_df))
        contrast_vec[curr_psychosis] <- 1   
        contrast_vec[prev_psychosis] <- -1   
        contrast_vec[curr_control] <- -1    
        contrast_vec[prev_control] <- 1     
        
        # get values from the contrast
        contrast_summary <- summary(contrast(emm, method = list(contrast_vec)))
        diffsteps_df$diff[i] <- contrast_summary$estimate
        diffsteps_df$pvalue[i] <- contrast_summary$p.value
        diffsteps_df$stars[i] <- case_when(
          diffsteps_df$pvalue[i] < 0.001 ~ "***",
          diffsteps_df$pvalue[i] < 0.01 ~ "**",
          diffsteps_df$pvalue[i] < 0.05 ~ "*",
          TRUE ~ ""
        )
        
        cat("Step", as.character(diffsteps_df$step[i]), 
            ": Difference =", round(diffsteps_df$diff[i], 3), 
            ", p =", round(diffsteps_df$pvalue[i], 4), diffsteps_df$stars[i], 
            "\n")
      }
      
      cat(paste(rep("-", 50), collapse = ""), "\n")
      
      # plot with significance stars
      diffsteps_plot <- ggplot(diffsteps_df, aes(x = step, y = diff)) +
        geom_col(fill = "#D41B55", alpha = 0.8) +
        geom_text(aes(label = paste0(round(diff, 2), stars)), 
                  vjust = -0.2) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
        theme_minimal(base_size = 14) +
        labs(title = paste("Group difference in stepwise change for", metric_label, "(", m, ")"),
             x = "Step (window size)",
             y = "Difference (psychosis - control)")
      
      print(diffsteps_plot)
      
      # save plot
      filename <- paste0(output_dir, "diffsteps_", metric, "_", m, ".png")
      ggsave(filename, plot = diffsteps_plot, width = 10, height = 6, dpi = 300)
    })
  }
}

sink()


# plot total change per group + significance
sink(paste0(output_dir, "total_change_results.txt"))
cat("Total Difference Results\n")
cat("===================================\n\n")

# plot total change from size 10 to size 50 per group with significance
for (metric in metrics) {
  for (m in models) {
    local({
    model_name <- paste("model", m, metric, sep = "_")
    model <- get(model_name)
    
    # estimated marginal means
    emm <- emmeans(model, ~ group * size)
    
    # pairwise contrasts within each group: size 50 vs size 10
    size_contrasts <- contrast(emm, method = "revpairwise", by = "group", adjust = "none")
    size_contrasts_df <- as.data.frame(size_contrasts)
    
    # filter for the specific contrast we want: size 50 vs size 10
    total_change_df <- size_contrasts_df %>%
      filter(contrast == "size50 - size10") %>%
      select(group, estimate, p.value) %>%
      rename(total_change = estimate, pvalue = p.value) %>%
      mutate(stars = case_when(
        pvalue < 0.001 ~ "***",
        pvalue < 0.01 ~ "**",
        pvalue < 0.05 ~ "*",
        TRUE ~ ""
      ))
    
    metric_label <- metric_labels[metric]
    
    cat("Metric:", metric_label, "| Model:", m, "\n")
    cat(paste("Group", total_change_df$group, 
              ": Change =", round(total_change_df$total_change, 3), 
              ", p =", format(total_change_df$pvalue, scientific = TRUE, digits = 3), 
              total_change_df$stars, 
              collapse = "\n"), "\n")
    cat("=", rep("=", 50), "\n", sep = "")
    
    # plot with significance stars
    total_change_plot <- ggplot(total_change_df, aes(x = group, y = total_change, fill = group)) +
      geom_col(alpha = 0.8) +
      geom_text(aes(label = paste0(round(total_change, 2), stars)), 
                vjust = -0.2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") +
      theme_minimal(base_size = 14) +
      scale_fill_manual(values = c("control" = "#1A85FF", "psychosis" = "#D41B55")) +
      labs(title = paste("Total change (size 10 → size 50) in", metric_label, "(", m, ")"),
           x = "Group",
           y = paste0("Total difference in", metric_label)) +
      theme(legend.position = "none")
    
    print(total_change_plot)
    
    # save
    filename <- paste0(output_dir, "totalchange_", metric, "_", m, ".png")
    ggsave(filename, plot = total_change_plot, width = 8, height = 6, dpi = 300)
    })
  }
}

sink()
while (sink.number() > 0) sink()

