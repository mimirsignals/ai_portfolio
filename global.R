# global.R - Load libraries and source modules (Updated with Rebalancing)

# Load required libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(quantmod)
library(plotly)
library(DT)
library(lubridate)
library(shinyWidgets)
library(R6)
library(zoo)
library(RColorBrewer)

# Optional libraries (install if needed)
suppressWarnings({
  if (!require(shinyjs, quietly = TRUE)) {
    message("shinyjs not available - some features may be limited")
  } else {
    library(shinyjs)
  }
})

# Application Configuration
APP_CONFIG <- list(
  csv_path = "portfolios_data.csv",
  default_symbols = c("DDOG", "FSLR", "NET", "IOBT", "BEAM", 
                      "AMPX", "PGEN", "SERV", "QUBT", "INOD"),
  default_investment = 10000,
  default_start_date_offset = 365,
  max_retries = 3,
  timeout_seconds = 30,
  max_portfolio_stocks = 20,
  min_weight_threshold = 0.01  # 1% minimum weight
)

# Source all modules and utilities
source("data_manager.R")           # Extended DataManager with rebalancing support
source("utils_data.R")             # Data fetching utilities
source("utils_portfolio.R")        # Portfolio calculation utilities
source("mod_performance.R")        # Performance overview module
source("mod_risk.R")              # Risk metrics module
source("mod_holdings.R")          # Holdings analysis module
source("mod_rebalance.R")         # NEW: Rebalancing module

# Optional modules (uncomment when implemented)
# source("utils_persistence.R")
# source("mod_portfolio_create.R")
# source("mod_portfolio_manage.R")

# Helper function for safe package loading
safe_require <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    warning(paste("Package", package, "is not installed. Some features may not work."))
    return(FALSE)
  }
  return(TRUE)
}

# Utility functions for rebalancing
#' Format currency for display
format_currency <- function(value, digits = 0) {
  paste0("$", formatC(value, format = "f", digits = digits, big.mark = ","))
}

#' Format percentage for display
format_percentage <- function(value, digits = 2) {
  paste0(round(value * 100, digits), "%")
}

#' Validate portfolio weights
#' @param weights Numeric vector of weights
#' @param tolerance Tolerance for sum check
#' @return TRUE if valid, FALSE otherwise
validate_weights <- function(weights, tolerance = 0.01) {
  if (any(weights < 0)) {
    return(FALSE)
  }
  
  if (abs(sum(weights) - 1) > tolerance) {
    return(FALSE)
  }
  
  return(TRUE)
}

#' Normalize weights to sum to 1
#' @param weights Numeric vector of weights
#' @return Normalized weights
normalize_weights <- function(weights) {
  if (sum(weights) == 0) {
    return(rep(1/length(weights), length(weights)))
  }
  return(weights / sum(weights))
}

#' Calculate rebalancing trades
#' @param current_values Current position values
#' @param target_weights Target allocation weights
#' @param total_value Total portfolio value
#' @return Data frame with trade information
calculate_trades <- function(current_values, target_weights, total_value) {
  current_weights <- current_values / sum(current_values)
  target_values <- target_weights * total_value
  
  trades <- data.frame(
    symbol = names(current_values),
    current_value = current_values,
    current_weight = current_weights,
    target_weight = target_weights,
    target_value = target_values,
    trade_amount = target_values - current_values,
    trade_type = ifelse(target_values > current_values, "BUY", "SELL"),
    stringsAsFactors = FALSE
  )
  
  # Only include meaningful trades (above threshold)
  trades$trade_amount[abs(trades$trade_amount) < 10] <- 0
  trades$trade_type[trades$trade_amount == 0] <- "HOLD"
  
  return(trades)
}

# Error handling wrapper for data operations
safe_data_operation <- function(operation, error_message = "Data operation failed") {
  tryCatch({
    operation()
  }, error = function(e) {
    warning(paste(error_message, ":", e$message))
    return(NULL)
  })
}

# Initialize application settings on load
message("Portfolio Dashboard initialized with rebalancing support")
message(paste("Default symbols:", paste(APP_CONFIG$default_symbols, collapse = ", ")))
message(paste("Portfolio data file:", APP_CONFIG$csv_path))