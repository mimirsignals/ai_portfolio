# test_excel_setup.R - Test your Excel portfolio setup

# Load required libraries
library(readxl)
library(dplyr)
library(lubridate)

# Test function to verify Excel setup
test_excel_setup <- function(excel_path = "portfolio.xlsx") {
  cat("=== Excel Portfolio Setup Test ===\n\n")
  
  # Check if file exists
  if (!file.exists(excel_path)) {
    cat("❌ FAIL: Excel file not found at:", excel_path, "\n")
    cat("Please create the file with your portfolio data.\n")
    return(FALSE)
  }
  cat("✅ PASS: Excel file found\n")
  
  # Try to read the file
  tryCatch({
    raw_data <- read_excel(excel_path)
    cat("✅ PASS: Excel file can be read\n")
    
    # Check columns
    names(raw_data) <- tolower(trimws(names(raw_data)))
    required_cols <- c("date", "symbol", "weight")
    
    if (!all(required_cols %in% names(raw_data))) {
      cat("❌ FAIL: Missing required columns\n")
      cat("Required:", paste(required_cols, collapse = ", "), "\n")
      cat("Found:", paste(names(raw_data), collapse = ", "), "\n")
      return(FALSE)
    }
    cat("✅ PASS: All required columns found\n")
    
    # Clean and validate data
    clean_data <- raw_data %>%
      select(date, symbol, weight) %>%
      mutate(
        date = as.Date(date),
        symbol = toupper(trimws(symbol)),
        weight = as.numeric(gsub(",", ".", as.character(weight)))
      ) %>%
      filter(!is.na(date), !is.na(symbol), !is.na(weight), weight > 0)
    
    cat("✅ PASS: Data cleaned successfully\n")
    cat("📊 Found", nrow(clean_data), "valid portfolio entries\n")
    
    # Check dates
    unique_dates <- sort(unique(clean_data$date), decreasing = TRUE)
    cat("📅 Portfolio dates:\n")
    for (i in seq_along(unique_dates)) {
      if (i == 1) {
        cat("   Current:", format(unique_dates[i], "%Y-%m-%d"), "\n")
      } else {
        cat("   Historical:", format(unique_dates[i], "%Y-%m-%d"), "\n")
      }
    }
    
    # Check weights sum to 1 for each date
    weight_check <- clean_data %>%
      group_by(date) %>%
      summarise(
        symbols = paste(symbol, collapse = ", "),
        total_weight = sum(weight),
        n_positions = n(),
        .groups = 'drop'
      )
    
    cat("\n📋 Weight validation:\n")
    for (i in 1:nrow(weight_check)) {
      check_row <- weight_check[i, ]
      status <- if (abs(check_row$total_weight - 1) < 0.01) "✅" else "⚠️"
      cat(sprintf("   %s %s: %.3f (%.0f%%) - %d positions\n",
                  status,
                  format(check_row$date, "%Y-%m-%d"),
                  check_row$total_weight,
                  check_row$total_weight * 100,
                  check_row$n_positions))
    }
    
    # Show symbols
    current_date <- unique_dates[1]
    current_portfolio <- clean_data %>% filter(date == current_date)
    cat("\n🎯 Current portfolio composition:\n")
    for (i in 1:nrow(current_portfolio)) {
      cat(sprintf("   %s: %.1f%%\n", 
                  current_portfolio$symbol[i],
                  current_portfolio$weight[i] * 100))
    }
    
    cat("\n🎉 SUCCESS: Excel setup appears to be working correctly!\n")
    cat("You can now run your Shiny app.\n")
    return(TRUE)
    
  }, error = function(e) {
    cat("❌ FAIL: Error reading Excel file:", e$message, "\n")
    return(FALSE)
  })
}

# Test function to create example Excel file
create_example_excel <- function(filename = "portfolio_example.xlsx") {
  
  # Create example data based on your portfolio
  example_data <- data.frame(
    date = c(rep("2025-05-21", 10), rep("2025-03-15", 5)),
    symbol = c("PGEN", "INOD", "AMPX", "BEAM", "DDOG", 
               "NET", "SERV", "QUBT", "IOBT", "FSLR",
               "PGEN", "DDOG", "NET", "FSLR", "BEAM"),
    weight = c(rep(0.1, 10), 
               0.15, 0.25, 0.25, 0.20, 0.15)
  )
  
  # Write to Excel
  tryCatch({
    if (requireNamespace("writexl", quietly = TRUE)) {
      writexl::write_xlsx(example_data, filename)
      cat("📝 Example Excel file created:", filename, "\n")
      cat("You can use this as a template for your portfolio.xlsx\n")
    } else {
      # Fallback to CSV
      csv_filename <- gsub("\\.xlsx$", ".csv", filename)
      write.csv(example_data, csv_filename, row.names = FALSE)
      cat("📝 Example CSV file created:", csv_filename, "\n")
      cat("Convert this to Excel format for use with the app\n")
    }
  }, error = function(e) {
    cat("❌ Could not create example file:", e$message, "\n")
  })
}

# Run the test
cat("Testing your Excel setup...\n")
test_result <- test_excel_setup()

if (!test_result) {
  cat("\n💡 TIP: Create an example file to get started:\n")
  create_example_excel()
}