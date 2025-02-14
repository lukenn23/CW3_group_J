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
  
  # Get new entries from the user for both draws
  new_entries <- rbind(get_numbers("Wednesday"), get_numbers("Saturday"))
  colnames(new_entries) <- colnames(lottery_data)
  
  # Keep only the last 104 weeks of data (removing the oldest entries)
  lottery_data <- tail(lottery_data, 104 * 2)
  
  # Append new entries
  lottery_data <- rbind(lottery_data, new_entries)
  
  # Save updated data to the file
  write.csv(lottery_data, filename, row.names = FALSE)
  cat("Lottery data updated and saved to", filename, "\n")
}

# Generate initial dataset
setup_lottery_data("historic.csv")
