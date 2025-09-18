# global.R - Updated for Excel-based Portfolio Management

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
library(readxl)  # New: For Excel file reading

# Optional libraries (install if needed)
suppressWarnings({
  if (!require(shinyjs, quietly = TRUE)) {
    message("shinyjs not available - some features may be limited")
  } else {
    library(shinyjs)
  }
})

# Application Configuration - Updated for Excel
APP_CONFIG <- list(
  excel_path = "portfolio.xlsx",                    # Changed from CSV to Excel
  default_investment = 10000,
  max_retries = 3,
  timeout_seconds = 30,
  refresh_interval_minutes = 15                     # Auto-refresh interval
)

# Source required modules and utilities
source("ExcelPortfolioManager.R")     # New: Excel portfolio manager
source("utils_data.R")                # Data fetching utilities
source("utils_portfolio.R")           # Portfolio calculation utilities
source("mod_performance.R")           # Updated performance overview module
source("mod_risk.R")                  # Risk metrics module (will work with new structure)
source("mod_holdings.R")              # Holdings analysis module (will work with new structure)

# Removed: mod_rebalance.R and data_manager.R (no longer needed)

# Helper function for safe package loading
safe_require <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    warning(paste("Package", package, "is not installed. Some features may not work."))
    return(FALSE)
  }
  return(TRUE)
}

# Utility functions (keeping existing ones that are still useful)

#' Format currency for display
format_currency <- function(value, digits = 0) {
  paste0("$", formatC(value, format = "f", digits = digits, big.mark = ","))
}

#' Format percentage for display
format_percentage <- function(value, digits = 2) {
  paste0(round(value * 100, digits), "%")
}

#' Create empty plotly plot
plotly_empty <- function(message = "No data available") {
  plot_ly() %>%
    add_annotations(
      text = message,
      x = 0.5,
      y = 0.5,
      showarrow = FALSE,
      font = list(size = 16, color = "gray")
    ) %>%
    layout(
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE),
      plot_bgcolor = 'rgba(0,0,0,0)',
      paper_bgcolor = 'rgba(0,0,0,0)'
    )
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

# Portfolio validation function for Excel data
validate_portfolio_excel_data <- function(data) {
  required_columns <- c("date", "symbol", "weight")
  
  if (!all(required_columns %in% names(data))) {
    return(list(valid = FALSE, message = "Missing required columns"))
  }
  
  # Check for valid dates
  if (any(is.na(data$date))) {
    return(list(valid = FALSE, message = "Invalid dates found"))
  }
  
  # Check for valid symbols
  if (any(is.na(data$symbol) | data$symbol == "")) {
    return(list(valid = FALSE, message = "Invalid or missing symbols found"))
  }
  
  # Check for valid weights
  if (any(is.na(data$weight) | data$weight <= 0)) {
    return(list(valid = FALSE, message = "Invalid weights found"))
  }
  
  # Check that weights sum to approximately 1 for each date
  weight_sums <- data %>%
    group_by(date) %>%
    summarise(total_weight = sum(weight), .groups = 'drop')
  
  if (any(abs(weight_sums$total_weight - 1) > 0.05)) {
    return(list(valid = FALSE, message = "Weights don't sum to 1 for some dates"))
  }
  
  return(list(valid = TRUE, message = "Valid portfolio data"))
}

# Initialize application settings
message("Excel-based Portfolio Monitor initialized")
message(paste("Excel file path:", APP_CONFIG$excel_path))

# Check if Excel file exists and provide guidance
if (!file.exists(APP_CONFIG$excel_path)) {
  warning(paste("Excel file not found:", APP_CONFIG$excel_path))
  message("Please create portfolio.xlsx with columns: date, symbol, weight")
  message("Example data:")
  message("date       symbol  weight")
  message("2025-05-21 PGEN    0.1")
  message("2025-05-21 INOD    0.1")
  message("... etc ...")
} else {
  message("Excel file found - ready to load portfolio data")
}