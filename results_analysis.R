knitr::opts_chunk$set(echo = TRUE)

# load libraries
library(tidyverse)
library(ggplot2)
library(pastecs)

# define models, groups, sizes, metrics
models <- c("gpt2", "qwen3", "falcon", "geitje")
groups <- c("control", "psychosis")
sizes  <- c("10", "20", "30", "40", "50")
metrics <- c("word_mean", "word_std", "word_min", "word_max")

# define directories
data_dir <- "perp_data/"
if (!dir.exists("outputs")) dir.create("outputs")
output_dir <- "outputs/"
if (!dir.exists("clean_perp_data")) dir.create("clean_perp_data")
clean_dir <- "clean_perp_data/"

# load a dataframe per perplexity output csv
for (m in models) {
  for (g in groups) {
    for (s in sizes) {
      objname <- paste(m, g, s, sep = "_")
      filepath <- paste0(data_dir, "output_perp_", objname, ".csv")
      
      df <- read.csv(filepath)
      df$id <- sub("\\.txt$", "", df$filename)
      df$filename <- NULL
      df <- df[, c("id", setdiff(names(df), "id"))]
      
      assign(objname, df)
    }
  }
}


# remove excluded participants
excluded_ids <- readLines("excluded_participants.txt")

for (m in models) {
  for (g in groups) {
    for (s in sizes) {
      objname <- paste(m, g, s, sep = "_")
      df <- get(objname)
      df <- df[!df$id %in% excluded_ids, ]
      assign(objname, df)
    }
  }
}


# initialise high skewness dataframe per metric
high_skew_list <- list()
for (metric in metrics) {
  high_skew_list[[metric]] <- data.frame(
    model = character(),
    group = character(),
    size = numeric(),
    skewness = numeric(),
    stringsAsFactors = FALSE
  )
}


# loop through metrics for stat.desc and high skew detection
for (metric in metrics) {
  
  combined_file <- paste0(output_dir, "statdesc_", metric, "_all.txt")
  sink(combined_file, split = TRUE)
  
  for (m in models) {
    for (g in groups) {
      
      individual_file <- paste0(output_dir, "statdesc_", metric, "_", m, "_", g, ".txt")
      sink(individual_file, split = TRUE)
      
      for (s in sizes) {
        objname <- paste(m, g, s, sep = "_")
        df <- get(objname)
        
        cat("\n====================\n")
        cat("Metric:", metric, " | Model:", m, " | Group:", g, " | Size:", s, "\n")
        
        # compute stat.desc
        desc <- stat.desc(df[[metric]], norm = TRUE)
        print(desc)
        
        # check skewness
        skew_val <- as.numeric(desc["skewness"])
        if (!is.na(skew_val) && abs(skew_val) > 1) {
          high_skew_list[[metric]] <- rbind(
            high_skew_list[[metric]],
            data.frame(model = m, group = g, size = s, skewness = skew_val)
          )
        }
      }
      
      sink()  # close individual file
    }
  }
  
  sink()  # close combined file
  # save high skew list per metric
  write.csv(high_skew_list[[metric]], 
            paste0(output_dir, "high_skew_list_", metric, ".csv"), 
            row.names = FALSE)
}


# outlier removal per metric
for (metric in metrics) {
  
  sink(paste0(output_dir, "outlier_removal_", metric, "_all.txt"), split = TRUE)
  
  for (m in models) {
    for (g in groups) {
      for (s in sizes) {
        objname <- paste(m, g, s, sep = "_")
        df <- get(objname)
        
        mean_val <- mean(df[[metric]], na.rm = TRUE)
        sd_val   <- sd(df[[metric]], na.rm = TRUE)
        
        df_clean <- df[df[[metric]] >= (mean_val - 3*sd_val) &
                       df[[metric]] <= (mean_val + 3*sd_val), ]
        
        skew_clean <- psych::describe(df_clean[[metric]])$skew
        
        cat("Metric:", metric, " | Model:", m, " | Group:", g, " | Size:", s,
            "- Original N:", nrow(df),
            "Clean N:", nrow(df_clean),
            "Skewness:", round(skew_clean, 3), "\n")
        
        clean_name <- paste(objname, metric, "clean", sep = "_")
        assign(clean_name, df_clean)
        
        write.csv(df_clean, file = paste0(clean_dir, clean_name, ".csv"), row.names = FALSE)
      }
    }
  }
  
  sink()
}


# now repeat skewness and stat desc check after outlier removal
# initialise high skewness dataframe per metric
high_skew_list_clean <- list()
for (metric in metrics) {
  high_skew_list_clean[[metric]] <- data.frame(
    model = character(),
    group = character(),
    size = numeric(),
    skewness = numeric(),
    stringsAsFactors = FALSE
  )
}


# loop through metrics for stat.desc and high skew detection again
for (metric in metrics) {
  
  combined_file <- paste0(output_dir, "clean_statdesc_", metric, "_all.txt")
  sink(combined_file, split = TRUE)
  
  for (m in models) {
    for (g in groups) {
      
      individual_file <- paste0(output_dir, "clean_statdesc_", metric, "_", m, "_", g, ".txt")
      sink(individual_file, split = TRUE)
      
      for (s in sizes) {
        objname <- paste(m, g, s, metric, "clean", sep = "_")
        df <- get(objname)
        
        cat("\n====================\n")
        cat("Metric:", metric, " | Model:", m, " | Group:", g, " | Size:", s, "\n")
        
        # compute stat.desc
        desc <- stat.desc(df[[metric]], norm = TRUE)
        print(desc)
        
        # check skewness
        skew_val <- as.numeric(desc["skewness"])
        if (!is.na(skew_val) && abs(skew_val) > 1) {
          high_skew_list_clean[[metric]] <- rbind(
            high_skew_list_clean[[metric]],
            data.frame(model = m, group = g, size = s, skewness = skew_val)
          )
        }
      }
      
      sink()  # close individual file
    }
  }
  
  sink()  # close combined file
  
  # save high skew list per metric
  write.csv(high_skew_list_clean[[metric]], 
            paste0(output_dir, "high_skew_list_", metric, "_clean.csv"), 
            row.names = FALSE)
}


# plot histograms per metric, model, across groups & sizes (density)
for (metric in metrics) {
  for (m in models) {
    
    df_combined <- map_dfr(groups, function(g) {
      map_dfr(sizes, function(s) {
        objname <- paste(m, g, s, sep = "_")
        df <- get(paste(objname, metric, "clean", sep = "_"))
        df %>%
          mutate(size = factor(s, levels = sizes),
                 group = g)
      })
    })
    
    plot <- ggplot(df_combined, aes(x = .data[[metric]])) +
      geom_histogram(aes(y = ..density..), binwidth = 1, fill = "#D41B55", color = "#D41B55") +
      facet_grid(rows = vars(size), cols = vars(group), scales = "free_y") +
      theme_minimal() +
      labs(title = paste("Metric:", metric, "| Model:", m),
           x = metric, y = "Density")
    
    print(plot)
  }
}

