library(nbastatR)
library(ballr)
library(tidyverse)

# Function to retrieve player statistics for each year using ballr with delay and retry
get_player_stats_by_year <- function(year) {
  file_name <- paste0("player_stats_", year, ".csv")
  if (file.exists(file_name)) {
    # Read the data from the file if it exists
    print(paste("Reading data from file for season:", year))
    season_stats <- read_csv(file_name)
  } else {
    retry_count <- 0
    season_stats <- data.frame()
    while (retry_count < 5) {
      print(paste("Attempting to retrieve data for season:", year, "Retry count:", retry_count))
      try({
        season_stats <- NBAPerGameStatistics(year)
        season_stats$season <- year
        # Save the data to a file
        write_csv(season_stats, file_name)
        print(paste("Successfully retrieved and saved data for season:", year))
        Sys.sleep(4)  # Slightly longer delay to avoid rate limiting
        break
      }, silent = TRUE)
      retry_count <- retry_count + 1
      if (retry_count < 5) {
        print(paste("Failed to retrieve data for season:", year, "Retrying after 15 seconds"))
        Sys.sleep(15)  # Longer delay before retrying
      } else {
        print(paste("Failed to retrieve data for season:", year, "after 5 attempts. Waiting for 1 minute before final retry."))
        Sys.sleep(60)  # Wait 1 minute before the final retry
      }
    }
    if (nrow(season_stats) == 0) {
      print(paste("Failed to retrieve data for season:", year))
    }
  }
  return(season_stats)
}

# List of years
years <- 1966:2024

# Function to handle rate limiting with timer
retrieve_all_data <- function(years) {
  player_stats_list <- list()
  start_time <- Sys.time()
  for (year in years) {
    # Calculate elapsed time
    elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    if (elapsed_time >= 60) {
      print("Resetting the timer.")
      start_time <- Sys.time()
    } else {
      print(paste("Elapsed time:", elapsed_time, "seconds. Waiting for the next request."))
      Sys.sleep(60 - elapsed_time)
    }
    season_stats <- get_player_stats_by_year(year)
    player_stats_list <- append(player_stats_list, list(season_stats))
  }
  return(bind_rows(player_stats_list))
}


draft_data <- drafts(draft_years = 1966:2024)

player_stats_list <- retrieve_all_data(years)

write_rds(draft_data, "draft_data.rds")
write_rds(player_stats_list, "player_stats_list.rds")


