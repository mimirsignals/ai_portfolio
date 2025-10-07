# Test script to verify the October 5-6 fix
# Run this to see debug output and verify the calculations are correct

source("global.R")

# Load portfolios
portfolios <- load_portfolios_from_excel("portfolio.xlsx")

cat("\n=== Portfolio Definitions ===\n")
for (name in names(portfolios)) {
  p <- portfolios[[name]]
  cat(sprintf("\n%s:\n", name))
  cat(sprintf("  Start date: %s\n", p$start_date))
  cat(sprintf("  Investment: $%.2f\n", p$total_investment))
  cat(sprintf("  Symbols: %s\n", paste(p$symbols, collapse = ", ")))
}

# Find high risk portfolio versions
high_risk_keys <- names(portfolios)[grepl("high risk", names(portfolios), ignore.case = TRUE)]

if (length(high_risk_keys) > 0) {
  cat("\n\n=== Testing High Risk Portfolio Versions ===\n")
  cat(sprintf("Found %d high risk portfolio versions\n", length(high_risk_keys)))

  # Calculate all high risk portfolios
  result <- calculate_all_portfolios_with_inheritance(
    portfolios = portfolios,
    selected_portfolios = high_risk_keys,
    show_sp500 = FALSE,
    show_btc = FALSE
  )

  if (!is.null(result) && !is.null(result$portfolios)) {
    cat("\n=== Results for October 5-6, 2025 ===\n")
    for (key in high_risk_keys) {
      portfolio_data <- result$portfolios[[key]]
      if (!is.null(portfolio_data)) {
        # Find Oct 5-6 data
        oct_idx <- portfolio_data$dates >= as.Date("2025-10-05") &
                   portfolio_data$dates <= as.Date("2025-10-06")

        if (any(oct_idx)) {
          cat(sprintf("\n%s:\n", key))
          oct_dates <- portfolio_data$dates[oct_idx]
          oct_returns <- portfolio_data$cumulative_returns[oct_idx]

          for (i in seq_along(oct_dates)) {
            cat(sprintf("  %s: %.4f%% cumulative return\n",
                        oct_dates[i], oct_returns[i] * 100))
          }
        } else {
          cat(sprintf("\n%s: No data for Oct 5-6\n", key))
        }
      } else {
        cat(sprintf("\n%s: No portfolio data calculated\n", key))
      }
    }
  } else {
    cat("\nERROR: Failed to calculate portfolios\n")
  }
} else {
  cat("\nNo 'high risk' portfolio found in portfolio.xlsx\n")
}

cat("\n=== Test Complete ===\n")
