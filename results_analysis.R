knitr::opts_chunk$set(echo = TRUE)

# load libraries
library(tidyverse)
library(ggplot2)
library(pastecs)

# define models, groups, sizes, metrics
models <- c("gpt2", "qwen3", "falcon", "geitje")
groups <- c("control", "psychosis")
sizes  <- c("10", "20", "30", "40", "50")
metrics <- c("mean_perp", "sd_perp", "min_perp", "max_perp")
metric_labels <- c(
  mean_perp = "mean perp.",
  sd_perp   = "SD perp.",
  min_perp  = "min perp.",
  max_perp  = "max perp."
)

# define directories
data_dir <- "perplexity_results/"
if (!dir.exists("exploration_outputs")) dir.create("exploration_outputs")
output_dir <- "exploration_outputs/"
if (!dir.exists("perplexity_results_clean")) dir.create("perplexity_results_clean")
clean_dir <- "perplexity_results_clean/"

# load a dataframe per perplexity output csv
for (m in models) {
  for (g in groups) {
    for (s in sizes) {
      local({
        objname <- paste(m, g, s, sep = "_")
        filepath <- paste0(data_dir, "output_perp_", objname, ".csv")
        
        df <- read.csv(filepath)
        df$id <- sub("\\.txt$", "", df$filename)
        df$filename <- NULL
        df <- df[, c("id", setdiff(names(df), "id"))]
        
        df <- df %>% rename(
          mean_perp = word_mean,
          sd_perp   = word_std,
          min_perp  = word_min,
          max_perp  = word_max, 
          q10_perp = word_10th, 
          q90_perp = word_90th
        )
        
        assign(objname, df, envir = .GlobalEnv)
      })
    }
  }
}


# remove excluded participants
excluded_ids <- readLines("demographics_outputs/excluded_participants.txt")

for (m in models) {
  for (g in groups) {
    for (s in sizes) {
      local({
        objname <- paste(m, g, s, sep = "_")
        df <- get(objname)
        df <- df[!df$id %in% excluded_ids, ]
        assign(objname, df, envir = .GlobalEnv)
      })
    }
  }
}


# initialize one single high skew list
high_skew_list_all <- data.frame(
  metric   = character(),
  model    = character(),
  group    = character(),
  size     = numeric(),
  skewness = numeric(),
  stringsAsFactors = FALSE
)

# loop through metrics for stat.desc and high skew detection
for (metric in metrics) {
  local({
    combined_file <- file(paste0(output_dir, "statdesc_", metric, "_all.txt"), open = "wt") 
    
    for (m in models) {
      for (g in groups) {
        
        sink(paste0(output_dir, "statdesc_", metric, "_", m, "_", g, ".txt"))
        for (s in sizes) {
          objname <- paste(m, g, s, sep = "_")
          df <- get(objname)

          header <- paste0("====================\n",
                           "Metric: ", metric,
                           " | Model: ", m,
                           " | Group: ", g,
                           " | Size: ", s,
                           "\n")
          cat(header)

          desc <- stat.desc(df[[metric]], norm = TRUE)
          print(desc)
          cat("\n\n")
          writeLines(c(header, capture.output(desc), "\n"), combined_file)
          
          skew_val <- as.numeric(desc["skewness"])
          if (!is.na(skew_val) && abs(skew_val) > 1) {
            high_skew_list_all <<- rbind(
              high_skew_list_all,
              data.frame(
                metric   = metric,
                model    = m,
                group    = g,
                size     = s,
                skewness = skew_val,
                stringsAsFactors = FALSE
              )
            )
          }
        }
        sink()
      }
    }
    
    close(combined_file)
  })
}

write.csv(high_skew_list_all,
          paste0(output_dir, "high_skew_list_all.csv"),
          row.names = FALSE)



# outlier removal per metric (removing whole rows)
# outputs 160 dataframes!
# commented because its obsolete

# for (metric in metrics) {
#   
#   sink(paste0(output_dir, "outlier_removal_", metric, "_all.txt"), split = TRUE)
#   
#   for (m in models) {
#     for (g in groups) {
#       for (s in sizes) {
#         objname <- paste(m, g, s, sep = "_")
#         df <- get(objname)
#         
#         mean_val <- mean(df[[metric]], na.rm = TRUE)
#         sd_val   <- sd(df[[metric]], na.rm = TRUE)
#         
#         df_clean <- df[df[[metric]] >= (mean_val - 3*sd_val) &
#                        df[[metric]] <= (mean_val + 3*sd_val), ]
#         
#         skew_clean <- psych::describe(df_clean[[metric]])$skew
#         
#         cat("Metric:", metric, " | Model:", m, " | Group:", g, " | Size:", s,
#             "- Original N:", nrow(df),
#             "Clean N:", nrow(df_clean),
#             "Skewness:", round(skew_clean, 3), "\n")
#         
#         clean_name <- paste(objname, metric, "clean", sep = "_")
#         assign(clean_name, df_clean)
#         
#         write.csv(df_clean, file = paste0(clean_dir, clean_name, ".csv"), row.names = FALSE)
#       }
#     }
#   }
#   
#   sink()
# }


# outlier removal (swiss cheese aproach)
# outputs as many dataframes as inputted (40)
sink(paste0(output_dir, "outlier_removal_log.txt"), split = TRUE)

for (m in models) {
  for (g in groups) {
    for (s in sizes) {
      local({
        objname <- paste(m, g, s, sep = "_")
        df <- get(objname)
        
        for (metric in metrics) {
          mean_val <- mean(df[[metric]], na.rm = TRUE)
          sd_val   <- sd(df[[metric]], na.rm = TRUE)
          
          outliers <- !is.na(df[[metric]]) & (
                        df[[metric]] < (mean_val - 3*sd_val) |
                        df[[metric]] > (mean_val + 3*sd_val)
                      )
          
          df[[metric]][outliers] <- NA
          
          skew_clean <- psych::describe(df[[metric]])$skew
          
          cat("Metric:", metric, " | Model:", m, " | Group:", g, " | Size:", s,
              "- Original N:", nrow(df),
              "Clean N:", sum(!is.na(df[[metric]])),
              "Skewness:", round(skew_clean, 3), "\n")
        }
        
        clean_name <- paste(objname, "clean", sep = "_")
        assign(clean_name, df, envir = .GlobalEnv)
        write.csv(df, file = paste0(clean_dir, clean_name, ".csv"), row.names = FALSE)
      })
    }
  }
}

sink()



# loop through metrics for stat.desc and high skew detection again
# initialize one single high skew list
high_skew_list_all_clean <- data.frame(
  metric   = character(),
  model    = character(),
  group    = character(),
  size     = numeric(),
  skewness = numeric(),
  stringsAsFactors = FALSE
)

for (metric in metrics) {
  local({
    combined_file <- file(paste0(output_dir, "clean_statdesc_", metric, "_all.txt"), open = "wt") 
    
    for (m in models) {
      for (g in groups) {
        
        sink(paste0(output_dir, "clean_statdesc_", metric, "_", m, "_", g, ".txt"))
        for (s in sizes) {
          objname <- paste(m, g, s, "clean", sep = "_")
          df <- get(objname)

          header <- paste0("====================\n",
                           "Metric: ", metric,
                           " | Model: ", m,
                           " | Group: ", g,
                           " | Size: ", s,
                           "\n")
          cat(header)

          desc <- stat.desc(df[[metric]][!is.na(df[[metric]])], norm = TRUE)
          print(desc)
          cat("\n\n")
          writeLines(c(header, capture.output(desc), "\n"), combined_file)
          
          skew_val <- as.numeric(desc["skewness"])
          if (!is.na(skew_val) && abs(skew_val) > 1) {
            high_skew_list_all_clean <<- rbind(
              high_skew_list_all_clean,
              data.frame(
                metric   = metric,
                model    = m,
                group    = g,
                size     = s,
                skewness = skew_val,
                stringsAsFactors = FALSE
              )
            )
          }
        }
        sink()
      }
    }
    
    close(combined_file)
  })
}

write.csv(high_skew_list_all_clean,
          paste0(output_dir, "high_skew_list_all_clean.csv"),
          row.names = FALSE)


# plot the data
# define custom colors for groups
group_colors <- c("control" = "#1A85FF", "psychosis" = "#D41B55")

for (metric in metrics) {
  for (m in models) {
    
    local({
      df_combined <- map_dfr(groups, function(g) {
        map_dfr(sizes, function(s) {
          objname <- paste(m, g, s, sep = "_")
          df <- get(paste(objname, "clean", sep = "_"))
          df %>%
            mutate(size = factor(s, levels = sizes),
                   group = g)
        })
      })
      
      metric_label <- metric_labels[metric]
      
      # histogram with group colors for both fill and border
      hist_plot <- ggplot(df_combined, aes(x = .data[[metric]], fill = group, color = group)) +
        geom_histogram(aes(y = ..density..), binwidth = 1, alpha = 0.8, position = "identity") +
        scale_fill_manual(values = group_colors) +
        scale_color_manual(values = group_colors) +
        facet_grid(rows = vars(size), cols = vars(group), scales = "free_y") +
        theme_minimal() +
        labs(title = paste("Histogram | Metric:", metric_label, "| Model:", toupper(m)),
             x = metric_label, y = "Density")
      
      print(hist_plot)
      ggsave(filename = paste0(output_dir, "histogram_", metric, "_", m, ".png"),
             plot = hist_plot, width = 10, height = 8, dpi = 300)
      
      # Violin plot with custom group colors
      violin_plot <- ggplot(df_combined, aes(x = size, y = .data[[metric]], fill = group)) +
        geom_violin(trim = FALSE, alpha = 0.8, color = NA) +
        scale_fill_manual(values = group_colors) +
        facet_wrap(~group) +
        theme_minimal() +
        labs(title = paste("Violin Plot | Metric:", metric_label, "| Model:", toupper(m)),
             x = "Size", y = metric_label)
      
      print(violin_plot)
      ggsave(filename = paste0(output_dir, "violin_", metric, "_", m, ".png"),
             plot = violin_plot, width = 10, height = 8, dpi = 300)
    })
  }
}



# violin plots with extra info to make them easier to read
# define custom colors for groups
group_colors <- c("control" = "#1A85FF", "psychosis" = "#D41B55")

for (metric in metrics) {
  for (m in models) {
    
    local({
      df_combined <- map_dfr(groups, function(g) {
        map_dfr(sizes, function(s) {
          objname <- paste(m, g, s, sep = "_")
          df <- get(paste(objname, "clean", sep = "_"))
          df %>%
            mutate(size = factor(s, levels = sizes),
                   group = g)
        })
      })
      
      metric_label <- metric_labels[metric]
    
      
      # compute mean and SD per group and size
      summary_stats <- df_combined %>%
        group_by(group, size) %>%
        summarise(
          mean_value = mean(.data[[metric]], na.rm = TRUE),
          sd_value   = sd(.data[[metric]], na.rm = TRUE),
          .groups = "drop"
        )

      violin_plot2 <- ggplot(df_combined, aes(x = size, y = .data[[metric]], fill = group)) +
        geom_violin(trim = FALSE, alpha = 0.8, color = NA) +
      
        # Add mean point
        geom_point(
          data = summary_stats,
          aes(x = size, y = mean_value, fill = group),
          color = "black",
          alpha = 0.5,
          size = 2,
          inherit.aes = FALSE
        ) +
      
        # Add vertical error bar for SD
        geom_errorbar(
          data = summary_stats,
          aes(x = size, ymin = mean_value - sd_value, ymax = mean_value + sd_value, group = group),
          width = 0.15,
          color = "black",
          alpha = 0.5,
          inherit.aes = FALSE
        ) +
      
        scale_fill_manual(values = group_colors) +
        facet_wrap(~group) +
        theme_minimal() +
        labs(
          title = paste("Violin Plot | Metric:", metric_label, "| Model:", toupper(m)),
          x = "Size",
          y = metric_label
        )

      print(violin_plot2)
      ggsave(filename = paste0(output_dir, "violin2_", metric, "_", m, ".png"),
             plot = violin_plot2, width = 10, height = 8, dpi = 300)
      
    })
  }
}

