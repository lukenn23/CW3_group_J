#Function to generate initial lottery data for two years and save it as a CSV file
setup_lottery_data <- function(filename) {
  # Starting date for the lottery
  start_date <- as.Date("2023-02-04")
  total_draws <- 104 * 2  # Two draws per week for 2 years
  
  # Create alternating drawing days
  draw_days <- rep(c("Saturday", "Wednesday"), length.out = total_draws)

  # Sequence of dates for the lotteryy, occuring every 3 days
  draw_dates <- seq(start_date, by = "3 days", length.out = total_draws)
  
  # Empty date frame to store lottery results
  lottery_data <- data.frame(Date = character(), Draw_Day = character(),
                             Main1 = integer(), Main2 = integer(), Main3 = integer(),
                             Main4 = integer(), Main5 = integer(), Main6 = integer(), Bonus = integer(),
                             stringsAsFactors = FALSE)
  
  # Generate lottery numbers for each draw
  for (i in 1:total_draws) {
    main_numbers <- sample(1:39, 6)  # Select 6 main numbers
    bonus_number <- sample(setdiff(1:39, main_numbers), 1)  # Ensure bonus number is unique
    
    # Assign values to respective row in data frame
    lottery_data[i, ] <- c(as.character(draw_dates[i]), draw_days[i], main_numbers, bonus_number)
  }
  
  # Save the data to a CSV file
  write.csv(lottery_data, filename, row.names = FALSE)

  # Confirm to the user that the file has been created
  cat("Lottery data saved to", filename, "\n")
}

# Run the function to generate and save lottery dataset
setup_lottery_data("historic.csv")