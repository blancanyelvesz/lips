knitr::opts_chunk$set(echo = TRUE)

# load libraries
library(tidyverse)
library(ggplot2)
library(pastecs)

# list variables
models <- c("gpt2", "qwen3", "falcon", "geitje")
groups <- c("control", "psychosis")
sizes  <- c("10", "20", "30", "40", "50")

# load all data
file_grid <- expand_grid(model = models,
                         group = groups,
                         size  = sizes) %>%
  mutate(file_path = paste0("output_perp_", model, "_", group, "_", size, ".csv"))

all_data <- file_grid %>%
  mutate(data = map(file_path, read_csv)) %>%
  unnest(data)

head(all_data)
# actually this is a lot maybe i just use smaller more readable dataframes


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


# plot histograms for each model-group combination
# NOT YET lets do after removing outliers
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
# NOT YET lets do after removing outliers
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


# REMOVE LATER
# log-transformation of high skew data
# open sink to save AND print all high-skewness log stat.desc outputs
sink("statdesc_high_skew_log.txt", split = TRUE)

# loop through each entry in high_skew list
for (i in seq_len(nrow(high_skew))) {
  m <- high_skew$model[i]
  g <- high_skew$group[i]
  s <- high_skew$size[i]
  
  # build the original dataframe name
  objname <- paste(m, g, s, sep = "_")
  
  # bet the dataframe
  df <- get(objname)
  
  # breate log-transformed dataframe
  df_log <- data.frame(
    id = df$id,
    word_mean_log = log(df$word_mean)
  )
  
  # build new dataframe name with _log
  new_objname <- paste(objname, "log", sep = "_")
  
  # assign to environment
  assign(new_objname, df_log)
  
  # print model info
  cat("\nModel:", m, "Group:", g, "Size:", s, "(log transformed)\n")
  
  # run stat.desc on log-transformed data
  desc <- stat.desc(df_log$word_mean_log, norm = TRUE)
  print(desc)
}

# close sink
sink()



# REMOVE LATER
# plot histograms for each model after log transformation
for (m in models) {
  
  # combine all groups and sizes for model
  df_combined <- map_dfr(groups, function(g) {
    map_dfr(sizes, function(s) {
      objname <- paste(m, g, s, sep = "_")
      
      # check if this model/group/size is in high_skew list
      if (any(high_skew$model == m & high_skew$group == g & high_skew$size == s)) {
        objname <- paste(objname, "log", sep = "_") # Use log dataframe
        df <- get(objname) %>%
          rename(word_mean = word_mean_log) # rename for plotting
      } else {
        df <- get(objname)
      }
      
      df %>%
        mutate(size = factor(s, levels = sizes),
               group = g)
    })
  })
  
  # plot: density instead of count
  p <- ggplot(df_combined, aes(x = word_mean)) +
    geom_histogram(aes(y = ..density..), binwidth = 1, fill = "#D41B55", color = "#D41B55") +
    facet_grid(rows = vars(size), cols = vars(group), scales = "free_y") +
    theme_minimal() +
    labs(
      title = paste("Model:", m),
      x = "mean perplexity",
      y = "Density"
    )
  
  print(p)
}



# REMOVE LATER
# plot histograms for log transformed data
# loop over each row of high_skew
for (i in 1:nrow(high_skew)) {
  m <- high_skew$model[i]
  g <- high_skew$group[i]
  s <- high_skew$size[i]
  
  df_name <- paste(m, g, s, sep = "_")
  log_df_name <- paste0(df_name, "_log")
  
  # only proceed if the log-transformed dataframe exists
  if (exists(log_df_name)) {
    df <- get(log_df_name)
    
    # plot histogram
    p <- ggplot(df, aes(x = word_mean_log)) +
      geom_histogram(aes(y = ..density..), binwidth = 0.1, fill = "#D41B55", color = "#D41B55") +
      theme_minimal() +
      labs(
        title = paste("Histogram (log-transformed) for", df_name),
        x = "log(mean perplexity)",
        y = "Density"
      )
    
    print(p)
  } else {
    warning(paste("Log-transformed dataframe does not exist for", df_name))
  }
}


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

