# Lottery Data Analysis Program
# --------------------------------
# This R script manages and analyzes lottery data for a two-year period.
# Users can generate random lottery draws, enter new weekly results, analyze trends,
# calculate potential winnings, and determine the best numbers to play based on past draws.
#
# Instructions:
# 1. Run `setup_lottery_data("historic.csv")` to generate the initial dataset.
# 2. Run `update_lottery_data("historic.csv")` every week to add new draws.
# 3. Run `lottery_summary("historic.csv")` to see the most frequent, least frequent, and coldest numbers.
# 4. Run `calculate_winnings("historic.csv")` to check how much a selected set of numbers would have won.
# 5. Run `best_numbers_to_play("historic.csv")` to find the best set of numbers to maximize winnings.
# --------------------------------





# Function to generate initial lottery data for two years and save it as a CSV file
setup_lottery_data <- function(filename) {
  start_date <- as.Date("2023-02-04")
  total_draws <- 104 * 2  # Two draws per week for 2 years
  
  # Create alternating draw days (Saturday and Wednesday)
  draw_days <- rep(c("Saturday", "Wednesday"), length.out = total_draws)
  draw_dates <- seq(start_date, by = "3 days", length.out = total_draws) #generates sequence of dates
  
  # Generate random lottery numbers for each draw
  lottery_data <- data.frame(
    Date = as.character(draw_dates), #stores the date of the draw
    Draw_Day = draw_days, #stores the day of the week for the draw
    t(sapply(1:total_draws, function(x) { #generates lottery numbers for each draw
      main_numbers <- sample(1:39, 6)  # Pick 6 unique numbers from 1 to 39
      bonus_number <- sample(setdiff(1:39, main_numbers), 1)  # Pick a unique bonus number from the remaining numbers
      c(main_numbers, bonus_number) #combines main and bonus numbers
    }))
  )
  
  colnames(lottery_data)[3:9] <- c("Main1", "Main2", "Main3", "Main4", "Main5", "Main6", "Bonus") #sets column names
  
  # Save the data to a CSV file
  write.csv(lottery_data, filename, row.names = FALSE)
  cat("Lottery data saved to", filename, "\n")
}

# Function to calculate winnings for a user-selected set of numbers
calculate_winnings <- function(filename) {
  if (!file.exists(filename)) { #checks if the file exists
    stop("Error: File not found. Run setup_lottery_data() first.")
  }
  
  lottery_data <- read.csv(filename, stringsAsFactors = FALSE) #reads the file
  
  repeat {
    cat("Enter your 6 lottery numbers (separated by spaces): ")
    user_numbers <- scan(what = integer(), quiet = TRUE, nmax = 6) #reads user input as integers

    # Validate input
    if (length(user_numbers) == 6 && all(user_numbers >= 1 & user_numbers <= 39) && length(unique(user_numbers)) == 6) {
      break #exists loop if input is invalid
    }
    
    cat(" Invalid input. Please enter exactly 6 unique numbers between 1 and 39.
")
  }
  
  winnings <- 0 #set winnings to 0/start fresh
  
  for (i in 1:nrow(lottery_data)) { #loops through each draw in the lottery data
    draw_numbers <- as.numeric(lottery_data[i, 3:8]) #extracts main numbers for the current draw
    bonus_number <- as.numeric(lottery_data[i, 9]) #extracts bonus numbers for the current draw
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
  
  cat(" Total winnings based on past draws:", winnings, "\n") #tells us about total winnings
}
  
# Function to find the best set of numbers for maximum winnings
best_numbers_to_play <- function(filename) {
  if (!file.exists(filename)) {
    stop("Error: File not found. Run setup_lottery_data() first.")
  }
  
  lottery_data <- read.csv(filename, stringsAsFactors = FALSE)
  main_numbers <- unlist(lottery_data[, 3:8])
  
  # Count frequency of each number and select the top 6
  freq_table <- table(main_numbers)
  best_numbers <- as.numeric(names(sort(freq_table, decreasing = TRUE)[1:6]))
  
  cat(" Best Numbers to Play Based on Past Data:", paste(best_numbers, collapse = ", "), "\n")
}
  
setup_lottery_data("historic.csv")


