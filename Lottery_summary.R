# Function to generate initial lottery data for two years and save it as a CSV file
setup_lottery_data <- function(filename) {
  start_date <- as.Date("2023-02-04")
  total_draws <- 104 * 2  # Two draws per week for 2 years
  
  # Create alternating draw days (Saturday and Wednesday)
  draw_days <- rep(c("Saturday", "Wednesday"), length.out = total_draws)
  draw_dates <- seq(start_date, by = "3 days", length.out = total_draws)
  
  # Generate random lottery numbers for each draw
  lottery_data <- data.frame(
    Date = as.character(draw_dates),
    Draw_Day = draw_days,
    t(sapply(1:total_draws, function(x) {
      main_numbers <- sample(1:39, 6)  # Pick 6 unique numbers
      bonus_number <- sample(setdiff(1:39, main_numbers), 1)  # Pick a unique bonus number
      c(main_numbers, bonus_number)
    }))
  )
  
  colnames(lottery_data)[3:9] <- c("Main1", "Main2", "Main3", "Main4", "Main5", "Main6", "Bonus")
  
  # Save the data to a CSV file
  write.csv(lottery_data, filename, row.names = FALSE)
  cat("Lottery data saved to", filename, "\n")
}

# Function to update lottery data with new weekly numbers
update_lottery_data <- function(filename) {
  if (!file.exists(filename)) {
    stop("Error: File not found. Run setup_lottery_data() first.")
  }
  
  lottery_data <- read.csv(filename, stringsAsFactors = FALSE)
  
  # Function to get user input for new draw numbers
  get_numbers <- function(draw_day) {
    repeat {
      cat("Enter 6 unique numbers (1-39) and a bonus number for", draw_day, "draw (separated by spaces): ")
      input <- scan(what = integer(), quiet = TRUE)
      if (length(input) == 7 && all(input >= 1 & input <= 39) && length(unique(input)) == 7) {
        return(c(Sys.Date(), draw_day, input))
      }
      cat("Invalid input. Please enter 6 unique numbers and 1 bonus number between 1 and 39.\n")
    }
  }
  
  # Get new entries from the user for both draws and convert to data frame
  new_entries <- as.data.frame(rbind(get_numbers("Wednesday"), get_numbers("Saturday")), stringsAsFactors = FALSE)
  colnames(new_entries) <- colnames(lottery_data)
  
  # Keep only the last 104 weeks of data (removing the oldest entries)
  lottery_data <- tail(lottery_data, 104 * 2)
  
  # Append new entries
  lottery_data <- rbind(lottery_data, new_entries)
  
  # Save updated data to the file
  write.csv(lottery_data, filename, row.names = FALSE)
  cat("Lottery data updated and saved to", filename, "\n")
}

# Function to analyze and summarize lottery data
lottery_summary <- function(filename) {
  if (!file.exists(filename)) {
    stop("Error: File not found. Run setup_lottery_data() first.")
  }
  
  lottery_data <- read.csv(filename, stringsAsFactors = FALSE)
  main_numbers <- unlist(lottery_data[, 3:8])  # Extract only main numbers from dataset
  
  # Count frequency of each number
  freq_table <- table(main_numbers)
  
  # Determine most and least frequently drawn numbers
  most_frequent <- as.numeric(names(freq_table[freq_table == max(freq_table)]))
  least_frequent <- as.numeric(names(freq_table[freq_table == min(freq_table)]))
  
  # Identify coldest numbers (longest since last drawn)
  latest_occurrence <- sapply(1:39, function(n) {
    indices <- which(lottery_data[, 3:8] == n, arr.ind = TRUE)
    if (length(indices) > 0) max(indices[, 1]) else NA
  })
  
  coldest_numbers <- order(latest_occurrence, decreasing = FALSE, na.last = TRUE)[1:3]
  
  # Display summary report
  cat("Lottery Summary:\n")
  cat("Most Frequent Number(s):", paste(most_frequent, collapse = ", "), "(Appeared", max(freq_table), "times)\n")
  cat("Least Frequent Number(s):", paste(least_frequent, collapse = ", "), "(Appeared", min(freq_table), "times)\n")
  cat("Coldest Numbers (Longest Not Seen):", paste(coldest_numbers, collapse = ", "), "\n")
}


lottery_summary("historic.csv")


