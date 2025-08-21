knitr::opts_chunk$set(echo = TRUE)

# load libraries
library(tidyverse)
library(ggplot2)
library(pastecs)

# list variables
models <- c("gpt2", "qwen3", "falcon", "geitje")
groups <- c("control", "psychosis")
sizes  <- c("10", "20", "30", "40", "50")

# load a dataframe per perplexity output csv
for (m in models) {
  for (g in groups) {
    for (s in sizes) {
      fname <- paste0("output_perp_", m, "_", g, "_", s, ".csv")
      objname <- paste(m, g, s, sep = "_")
      
      df <- read.csv(fname)
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


# data frame to store results with high skewness
high_skew <- data.frame(
  model = character(),
  group = character(),
  size = numeric(),
  skewness = numeric(),
  stringsAsFactors = FALSE
)

# loop through models, groups, sizes for stat desc
for (m in models) {
  for (g in groups) {
    filename <- paste0("statdesc_", m, "_", g, ".txt")
    
    # split sink to print AND save
    sink(filename, split = TRUE)
    
    for (s in sizes) {
      objname <- paste(m, g, s, sep = "_")
      df <- get(objname)
      
      cat("\nModel:", m, "Group:", g, "Size:", s, "\n")
      desc <- stat.desc(df$word_mean, norm = TRUE)
      print(desc)
      
      # check skewness
      skew_val <- as.numeric(desc["skewness"])
      if (!is.na(skew_val) && abs(skew_val) > 1) {
        high_skew <- rbind(
          high_skew,
          data.frame(model = m, group = g, size = s, skewness = skew_val)
        )
      }
    }
    
    sink()
  }
}

# show the list of high skewness combinations
print(high_skew)
write.csv(high_skew, "high_skew_list.csv", row.names = FALSE)


# remove outliers
sink("outlier_removal_all.txt", split = TRUE)

# loop through models, groups, sizes
for (m in models) {
  for (g in groups) {
    for (s in sizes) {
      
      objname <- paste(m, g, s, sep = "_")
      df <- get(objname)
      
      # compute mean and SD
      mean_val <- mean(df$word_mean, na.rm = TRUE)
      sd_val <- sd(df$word_mean, na.rm = TRUE)
      
      # keep only rows within ±3 SD
      df_clean <- df[df$word_mean >= (mean_val - 3*sd_val) & 
                     df$word_mean <= (mean_val + 3*sd_val), ]
      
      # calculate skewness for cleaned data
      desc_clean <- psych::describe(df_clean$word_mean)
      skew_clean <- desc_clean$skew
      
      cat("Model:", m, "Group:", g, "Size:", s, 
          "- Original N:", nrow(df), 
          "Clean N:", nrow(df_clean),
          "Skewness:", round(skew_clean, 3), "\n")
      
      # assign cleaned dataframe to environment
      assign(paste0(objname, "_clean"), df_clean)
    }
  }
}

sink() 


# plot histograms for each model-group combination
for (m in models) {
  for (g in groups) {
    
    # combine the five window sizes dataframes
    df_combined <- map_dfr(sizes, function(s) {
      objname <- paste(m, g, s, sep = "_")
      df <- get(objname)
      df$size <- s
      df
    })
    
    df_combined <- df_combined %>%
      mutate(size = factor(size, levels = sizes))
    
    p <- ggplot(df_combined, aes(x = word_mean)) +
      geom_histogram(binwidth = 1, fill = "#D41B55", color = "#D41B55") +
      facet_wrap(~ size, scales = "free_y") +
      theme_minimal() +
      labs(title = paste("Model:", m, "| Group:", g),
           x = "mean perplexity", y = "Count")
    
    print(p)
  }
}

# plot histograms for each model
for (m in models) {
  
  # combine all groups and sizes for model
  df_combined <- map_dfr(groups, function(g) {
    map_dfr(sizes, function(s) {
      objname <- paste(m, g, s, "clean", sep = "_")
      df <- get(objname)
      df %>%
        mutate(size = factor(s, levels = sizes),
               group = g)
    })
  })
  
  # facet_grid: rows = size, cols = group
  # density instead of count because theres twice the mount of psychosis subjects
  p <- ggplot(df_combined, aes(x = word_mean)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "#D41B55", color = "#D41B55") +
  facet_grid(rows = vars(size), cols = vars(group), scales = "free_y") +
  theme_minimal() +
  labs(title = paste("Model:", m),
       x = "mean perplexity",
       y = "Density")

  
  print(p)
}

