# debug_portfolio_data.R - Debug the portfolio data flow

# Load required libraries
library(readxl)
library(dplyr)
library(lubridate)

# Source the required files
source("ExcelPortfolioManager.R")
source("utils_data.R")
source("utils_portfolio.R")

# Debug function
debug_portfolio_flow <- function(excel_path = "portfolio.xlsx") {
  
  cat("=== Portfolio Data Flow Debug ===\n\n")
  
  # Step 1: Test Excel Manager
  cat("1. Testing Excel Portfolio Manager...\n")
  
  tryCatch({
    manager <- ExcelPortfolioManager$new(excel_path)
    portfolios <- manager$get_all_portfolios()
    
    cat("✅ Excel manager created successfully\n")
    cat("📊 Found", length(portfolios), "portfolios\n")
    
    for (name in names(portfolios)) {
      portfolio <- portfolios[[name]]
      cat(sprintf("   - %s: %d symbols, start: %s\n", 
                  name, 
                  length(portfolio$symbols), 
                  format(portfolio$start_date, "%Y-%m-%d")))
    }
    
    # Step 2: Test bridged calculator
    cat("\n2. Testing Bridged Calculator...\n")
    
    calc <- bridgedPortfolioCalculator()
    selected_portfolios <- names(portfolios)[1]  # Just test first portfolio
    
    cat("Selected portfolio for test:", selected_portfolios, "\n")
    
    # Test calculation
    result <- calc(
      portfolios = portfolios,
      selected_portfolios = selected_portfolios,
      show_sp500 = TRUE,
      show_btc = TRUE
    )
    
    cat("✅ Calculator executed\n")
    cat("📊 Result structure:\n")
    cat("   - Portfolios:", length(result$portfolios), "\n")
    cat("   - S&P 500:", !is.null(result$sp500), "\n")
    cat("   - Bitcoin:", !is.null(result$bitcoin), "\n")
    
    # Examine first portfolio result
    if (length(result$portfolios) > 0) {
      first_portfolio_name <- names(result$portfolios)[1]
      first_portfolio <- result$portfolios[[first_portfolio_name]]
      
      cat("\n3. Examining first portfolio result:\n")
      cat("   - Portfolio name:", first_portfolio_name, "\n")
      cat("   - Dates available:", !is.null(first_portfolio$dates), "\n")
      cat("   - Number of dates:", length(first_portfolio$dates), "\n")
      cat("   - Returns available:", !is.null(first_portfolio$cumulative_returns), "\n")
      cat("   - Number of returns:", length(first_portfolio$cumulative_returns), "\n")
      cat("   - Individual stocks:", !is.null(first_portfolio$individual_stocks), "\n")
      
      if (!is.null(first_portfolio$individual_stocks)) {
        cat("   - Individual stocks rows:", nrow(first_portfolio$individual_stocks), "\n")
        cat("   - Individual stocks symbols:", 
            paste(unique(first_portfolio$individual_stocks$symbol), collapse = ", "), "\n")
      }
      
      # Check data types
      if (!is.null(first_portfolio$cumulative_returns)) {
        returns <- first_portfolio$cumulative_returns
        cat("   - Returns class:", class(returns), "\n")
        cat("   - Returns range:", round(min(returns, na.rm = TRUE), 4), "to", 
            round(max(returns, na.rm = TRUE), 4), "\n")
        cat("   - Any NA returns:", any(is.na(returns)), "\n")
        cat("   - Any infinite returns:", any(!is.finite(returns)), "\n")
      }
      
      if (!is.null(first_portfolio$dates)) {
        dates <- first_portfolio$dates
        cat("   - Dates class:", class(dates), "\n")
        cat("   - Date range:", format(min(dates, na.rm = TRUE), "%Y-%m-%d"), "to", 
            format(max(dates, na.rm = TRUE), "%Y-%m-%d"), "\n")
      }
      
      # Check benchmark data
      cat("\n4. Examining benchmark data:\n")
      if (!is.null(result$sp500)) {
        cat("   - S&P 500 dates:", length(result$sp500$dates), "\n")
        cat("   - S&P 500 returns:", length(result$sp500$cumulative_returns), "\n")
        if (length(result$sp500$cumulative_returns) > 0) {
          sp_returns <- result$sp500$cumulative_returns
          cat("   - S&P 500 returns range:", round(min(sp_returns, na.rm = TRUE), 4), "to", 
              round(max(sp_returns, na.rm = TRUE), 4), "\n")
          cat("   - S&P 500 any NA:", any(is.na(sp_returns)), "\n")
          cat("   - S&P 500 any infinite:", any(!is.finite(sp_returns)), "\n")
        }
      }
      
      if (!is.null(result$bitcoin)) {
        cat("   - Bitcoin dates:", length(result$bitcoin$dates), "\n")
        cat("   - Bitcoin returns:", length(result$bitcoin$cumulative_returns), "\n")
        if (length(result$bitcoin$cumulative_returns) > 0) {
          btc_returns <- result$bitcoin$cumulative_returns
          cat("   - Bitcoin returns range:", round(min(btc_returns, na.rm = TRUE), 4), "to", 
              round(max(btc_returns, na.rm = TRUE), 4), "\n")
          cat("   - Bitcoin any NA:", any(is.na(btc_returns)), "\n")
          cat("   - Bitcoin any infinite:", any(!is.finite(btc_returns)), "\n")
        }
      }
    }
    
    # Step 5: Test plot data preparation
    cat("\n5. Testing plot data preparation:\n")
    
    tryCatch({
      # Simulate the prepare_performance_plot_data function
      plot_data <- data.frame()
      
      # Add portfolio data
      for (name in names(result$portfolios)) {
        portfolio <- result$portfolios[[name]]
        if (!is.null(portfolio$dates) && !is.null(portfolio$cumulative_returns) &&
            length(portfolio$dates) == length(portfolio$cumulative_returns)) {
          temp_data <- data.frame(
            date = portfolio$dates,
            return_pct = portfolio$cumulative_returns * 100,
            portfolio = name
          )
          plot_data <- rbind(plot_data, temp_data)
        }
      }
      
      # Add benchmark data
      if (!is.null(result$sp500) && !is.null(result$sp500$dates) && !is.null(result$sp500$cumulative_returns)) {
        if (length(result$sp500$dates) == length(result$sp500$cumulative_returns)) {
          temp_data <- data.frame(
            date = result$sp500$dates,
            return_pct = result$sp500$cumulative_returns * 100,
            portfolio = "S&P 500"
          )
          plot_data <- rbind(plot_data, temp_data)
        }
      }
      
      if (!is.null(result$bitcoin) && !is.null(result$bitcoin$dates) && !is.null(result$bitcoin$cumulative_returns)) {
        if (length(result$bitcoin$dates) == length(result$bitcoin$cumulative_returns)) {
          temp_data <- data.frame(
            date = result$bitcoin$dates,
            return_pct = result$bitcoin$cumulative_returns * 100,
            portfolio = "Bitcoin"
          )
          plot_data <- rbind(plot_data, temp_data)
        }
      }
      
      cat("✅ Plot data prepared successfully\n")
      cat("📊 Plot data rows:", nrow(plot_data), "\n")
      cat("📊 Plot data portfolios:", paste(unique(plot_data$portfolio), collapse = ", "), "\n")
      
      # Check for issues
      if (nrow(plot_data) == 0) {
        cat("❌ WARNING: No plot data generated\n")
      } else {
        # Check for data type issues
        cat("   - Date class:", class(plot_data$date), "\n")
        cat("   - Return_pct class:", class(plot_data$return_pct), "\n")
        cat("   - Any NA in returns:", any(is.na(plot_data$return_pct)), "\n")
        cat("   - Any infinite in returns:", any(!is.finite(plot_data$return_pct)), "\n")
        
        if (any(!is.finite(plot_data$return_pct))) {
          bad_rows <- which(!is.finite(plot_data$return_pct))
          cat("   - First few bad rows:", head(bad_rows, 5), "\n")
          cat("   - Bad data sample:\n")
          print(plot_data[head(bad_rows, 3), ])
        }
      }
      
    }, error = function(e) {
      cat("❌ Error in plot data preparation:", e$message, "\n")
    })
    
    cat("\n🎉 Debug completed successfully!\n")
    
  }, error = function(e) {
    cat("❌ Error in debug process:", e$message, "\n")
    cat("Traceback:\n")
    print(traceback())
  })
}

# Test with sample data creation
create_debug_excel <- function(filename = "debug_portfolio.xlsx") {
  # Create minimal test data
  test_data <- data.frame(
    date = as.Date("2025-01-01"),
    symbol = c("AAPL", "MSFT"),
    weight = c(0.5, 0.5)
  )
  
  tryCatch({
    if (requireNamespace("writexl", quietly = TRUE)) {
      writexl::write_xlsx(test_data, filename)
      cat("📝 Debug Excel file created:", filename, "\n")
    } else {
      # Fallback to CSV
      csv_filename <- gsub("\\.xlsx$", ".csv", filename)
      write.csv(test_data, csv_filename, row.names = FALSE)
      cat("📝 Debug CSV file created:", csv_filename, "\n")
    }
  }, error = function(e) {
    cat("❌ Could not create debug file:", e$message, "\n")
  })
}

# Run debug
cat("Running portfolio data flow debug...\n")
debug_result <- debug_portfolio_flow()

# If main file doesn't exist, create debug file
if (!file.exists("portfolio.xlsx")) {
  cat("\nCreating debug Excel file for testing...\n")
  create_debug_excel("portfolio.xlsx")
  cat("Now rerun the debug with: debug_portfolio_flow()\n")
}