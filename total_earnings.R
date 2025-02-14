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

# Function to calculate winnings for a user-selected set of numbers
calculate_winnings <- function(filename) {
  if (!file.exists(filename)) {
    stop("Error: File not found. Run setup_lottery_data() first.")
  }
  
  lottery_data <- read.csv(filename, stringsAsFactors = FALSE)
  
  repeat {
    cat("Enter your 6 lottery numbers (separated by spaces): ")
    user_numbers <- scan(what = integer(), quiet = TRUE, nmax = 6)

    # Validate input
    if (length(user_numbers) == 6 && all(user_numbers >= 1 & user_numbers <= 39) && length(unique(user_numbers)) == 6) {
      break
    }
    
    cat("❌ Invalid input. Please enter exactly 6 unique numbers between 1 and 39.
")
  }
  
  winnings <- 0
  
  for (i in 1:nrow(lottery_data)) {
    draw_numbers <- as.numeric(lottery_data[i, 3:8])
    bonus_number <- as.numeric(lottery_data[i, 9])
    matched <- sum(user_numbers %in% draw_numbers)
    
    if (matched == 6) {
      winnings <- winnings + 2000000
    } else if (matched == 5 && bonus_number %in% user_numbers) {
      winnings <- winnings + 500000
    } else if (matched == 5) {
      winnings <- winnings + 1750
    } else if (matched == 4) {
      winnings <- winnings + 140
    } else if (matched == 3) {
      winnings <- winnings + 30
    }
  }
  
  cat("🎉 Total winnings based on past draws:", winnings, "
")
}
  
  lottery_data <- read.csv(filename, stringsAsFactors = FALSE)
  
  cat("Enter your 6 lottery numbers (separated by spaces): ")
  user_numbers <- scan(what = integer(), quiet = TRUE, nmax = 6)
  if (length(user_numbers) != 6 || any(user_numbers < 1 | user_numbers > 39) || length(unique(user_numbers)) != 6) {
    stop("Invalid input. Please enter 6 unique numbers between 1 and 39.")
  }
  
  winnings <- 0
  
  for (i in 1:nrow(lottery_data)) {
    draw_numbers <- as.numeric(lottery_data[i, 3:8])
    bonus_number <- as.numeric(lottery_data[i, 9])
    matched <- sum(user_numbers %in% draw_numbers)
    
    if (matched == 6) {
      winnings <- winnings + 2000000
    } else if (matched == 5 && bonus_number %in% user_numbers) {
      winnings <- winnings + 500000
    } else if (matched == 5) {
      winnings <- winnings + 1750
    } else if (matched == 4) {
      winnings <- winnings + 140
    } else if (matched == 3) {
      winnings <- winnings + 30
    }
  }
  
  cat("Total winnings based on past draws:", winnings, "\n")





