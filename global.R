# global.R - Simplified for Excel-based Portfolio Management

# Load required libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(quantmod)
library(plotly)
library(DT)
library(lubridate)
library(shinyWidgets)
library(readxl)  # For Excel file reading
library(zoo)
library(RColorBrewer)

# Optional libraries
suppressWarnings({
  if (!require(shinyjs, quietly = TRUE)) {
    message("shinyjs not available - some features may be limited")
  } else {
    library(shinyjs)
  }
})

# Source required modules and utilities
source("simple_excel_reader.R")      # Simple Excel reader
source("utils_data.R")               # Data fetching utilities
source("utils_portfolio.R")          # Portfolio calculation utilities
source("mod_performance.R")          # Performance module
source("mod_risk.R")                 # Risk metrics module
source("mod_holdings.R")             # Holdings module

# Helper functions
format_currency <- function(value, digits = 0) {
  paste0("$", formatC(value, format = "f", digits = digits, big.mark = ","))
}

format_percentage <- function(value, digits = 2) {
  paste0(round(value * 100, digits), "%")
}

plotly_empty <- function(message = "No data available") {
  plot_ly() %>%
    add_annotations(
      text = message,
      x = 0.5, y = 0.5,
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

# Initialize
message("Simple Portfolio Monitor initialized")
message("Excel file: portfolio.xlsx")

# Check if Excel file exists
if (!file.exists("portfolio.xlsx")) {
  warning("Excel file 'portfolio.xlsx' not found")
  message("Please create portfolio.xlsx with columns: date, symbol, weight")
  message("Note: Use comma (,) as decimal separator if needed")
} else {
  message("Excel file found - ready to load portfolio data")
}