# simple_test.R - Test your Excel portfolio setup

library(readxl)
library(dplyr)

# Simple test function
test_excel_portfolio <- function(excel_path = "portfolio.xlsx") {
  
  cat("=== Simple Portfolio Test ===\n\n")
  
  # Step 1: Read your Excel file
  cat("1. Reading Excel file...\n")
  
  if (!file.exists(excel_path)) {
    cat("❌ File not found:", excel_path, "\n")
    return(FALSE)
  }
  
  raw_data <- read_excel(excel_path)
  cat("✅ Excel file read successfully\n")
  cat("   Columns:", paste(names(raw_data), collapse = ", "), "\n")
  cat("   Rows:", nrow(raw_data), "\n\n")
  
  # Step 2: Clean the data (handle comma decimals)
  cat("2. Cleaning data...\n")
  
  clean_data <- raw_data %>%
    rename_all(tolower) %>%
    mutate(
      date = as.Date(date),
      symbol = toupper(trimws(symbol)),
      weight = as.numeric(gsub(",", ".", as.character(weight)))  # Comma to dot
    ) %>%
    filter(!is.na(date), !is.na(symbol), weight > 0)
  
  cat("✅ Data cleaned\n")
  cat("   Valid rows:", nrow(clean_data), "\n")
  
  # Step 3: Show your portfolios
  cat("\n3. Your portfolios:\n")
  
  portfolio_dates <- unique(clean_data$date) %>% sort(decreasing = TRUE)
  
  for (i in seq_along(portfolio_dates)) {
    date <- portfolio_dates[i]
    date_data <- clean_data %>% filter(date == !!date)
    
    weight_sum <- sum(date_data$weight)
    status <- if (i == 1) "CURRENT" else "HISTORICAL"
    
    cat(sprintf("   %s - %s: %d stocks, weights sum = %.3f\n", 
                status, format(date, "%Y-%m-%d"), nrow(date_data), weight_sum))
    
    # Show stocks for this date
    for (j in 1:nrow(date_data)) {
      cat(sprintf("      %s: %.1f%%\n", 
                  date_data$symbol[j], 
                  date_data$weight[j] * 100))
    }
    cat("\n")
  }
  
  # Step 4: Test the simple reader function
  cat("4. Testing simple reader function...\n")
  
  # Load the function
  source("simple_excel_reader.R")
  
  portfolios <- read_portfolio_excel(excel_path)
  
  cat("✅ Portfolio reader function works\n")
  cat("   Created", length(portfolios), "portfolio objects\n")
  
  for (name in names(portfolios)) {
    portfolio <- portfolios[[name]]
    cat(sprintf("   - %s: %d stocks, start date: %s\n",
                name,
                length(portfolio$symbols),
                format(portfolio$start_date, "%Y-%m-%d")))
  }
  
  cat("\n🎉 Everything looks good! Your Excel file is ready for the app.\n")
  return(TRUE)
}

# Run the test
cat("Testing your portfolio Excel setup...\n\n")
test_result <- test_excel_portfolio()

if (test_result) {
  cat("\nYour portfolio.xlsx file is working correctly!\n")
  cat("You can now run: shiny::runApp() to start your portfolio monitor.\n")
}