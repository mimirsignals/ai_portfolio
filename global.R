# global.R - Load libraries and source modules

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
  timeout_seconds = 30
)

# Source all modules and utilities
source("data_manager.R")
source("utils_data.R")
source("utils_portfolio.R") 
# source("utils_persistence.R")
source("mod_performance.R")
source("mod_risk.R")
source("mod_holdings.R")
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