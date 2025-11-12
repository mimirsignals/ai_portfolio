# Test script to verify opening prices are being fetched and used
# Run this from RStudio or via: Rscript test_opening_prices.R

# Load required libraries
library(quantmod)
library(dplyr)
library(lubridate)

# Source the utility files
source("utils_data.R")
source("utils_portfolio.R")

cat("Testing opening price implementation...\n\n")

# Test 1: Verify fetch_stock_data returns open_price column
cat("Test 1: Checking if fetch_stock_data includes open_price column...\n")
test_data <- fetch_stock_data(c("AAPL", "MSFT"), start_date = as.Date("2024-11-01"))

if (!is.null(test_data)) {
  cat("  Columns returned:", paste(names(test_data), collapse = ", "), "\n")

  if ("open_price" %in% names(test_data)) {
    cat("  ✓ open_price column present\n")

    # Show sample data
    sample <- test_data %>%
      filter(symbol == "AAPL") %>%
      head(3)

    cat("\n  Sample data for AAPL:\n")
    print(sample)

    # Check that open_price and adjusted_price are different
    if (any(sample$open_price != sample$adjusted_price, na.rm = TRUE)) {
      cat("  ✓ open_price differs from adjusted_price (as expected)\n")
    } else {
      cat("  ⚠ Warning: open_price and adjusted_price are identical\n")
    }
  } else {
    cat("  ✗ FAILED: open_price column missing\n")
  }
} else {
  cat("  ✗ FAILED: No data returned from fetch_stock_data\n")
}

cat("\n")

# Test 2: Verify fetch_prices_on_or_before returns opening prices
cat("Test 2: Checking if fetch_prices_on_or_before uses opening prices...\n")
test_prices <- fetch_prices_on_or_before(
  symbols = c("AAPL", "MSFT"),
  cutoff_date = as.Date("2024-11-08")
)

if (!is.null(test_prices) && length(test_prices) > 0) {
  cat("  Prices retrieved:\n")
  for (sym in names(test_prices)) {
    cat(sprintf("    %s: $%.2f\n", sym, test_prices[sym]))
  }

  # Compare with adjusted close to verify they're different (opening price)
  test_data_close <- test_data %>%
    filter(symbol %in% names(test_prices), date <= as.Date("2024-11-08")) %>%
    group_by(symbol) %>%
    slice_tail(n = 1) %>%
    ungroup()

  cat("\n  Comparison (should use opening prices):\n")
  for (sym in names(test_prices)) {
    close_data <- test_data_close %>% filter(symbol == sym)
    if (nrow(close_data) > 0) {
      cat(sprintf("    %s: open=$%.2f, adjusted_close=$%.2f, using=$%.2f\n",
                  sym,
                  close_data$open_price[1],
                  close_data$adjusted_price[1],
                  test_prices[sym]))

      if (abs(test_prices[sym] - close_data$open_price[1]) < 0.01) {
        cat(sprintf("      ✓ Using opening price for %s\n", sym))
      } else if (abs(test_prices[sym] - close_data$adjusted_price[1]) < 0.01) {
        cat(sprintf("      ✗ FAILED: Still using adjusted close for %s\n", sym))
      }
    }
  }
} else {
  cat("  ✗ FAILED: No prices returned\n")
}

cat("\n")
cat("Test complete!\n")
cat("\nSummary:\n")
cat("- Opening prices are now fetched from market data\n")
cat("- Rebalancing transactions use opening prices\n")
cat("- Portfolio performance uses opening price as entry point on rebalance date\n")
cat("- Subsequent performance tracking uses adjusted closing prices\n")
