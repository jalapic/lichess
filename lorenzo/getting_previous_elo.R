### Load in Data

ldf <- readRDS("lorenzo/streaks.RData")
head(ldf)

library(tidyverse)

df <- ldf %>%
  select(Username, UserELO, EndDate) %>%
  group_by(Username) %>%
  mutate(relday =  as.numeric(difftime(as.Date(EndDate), min(as.Date(EndDate)), units = "days")),
         lagval = relday-lag(relday))




# Assuming your DataFrame is named 'df'

# Step 1: Identify rows where 'lagval' is 30 or greater
index_to_keep <- which(df$lagval >= 30)

# Step 2: Extract the preceding 20 rows with the same IDs
results_list <- lapply(index_to_keep, function(idx) {
  current_id <- df$id[idx]
  start_idx <- max(idx - 20, 1)  # Make sure the starting index is at least 1
  preceding_rows <- df[start_idx:idx - 1, ]
  
  # Keep the preceding rows as long as the IDs are the same
  while (all(preceding_rows$id == current_id) && start_idx > 1) {
    start_idx <- max(start_idx - 20, 1)
    preceding_rows <- df[start_idx:idx - 1, ]
  }
  
  # Return the final subset of rows
  return(df[start_idx:idx, ])
})

# Step 3: 'results_list' will contain a list of data frames with the required rows

results_list
