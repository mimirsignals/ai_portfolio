# ExcelPortfolioManager.R - Excel-based Portfolio Data Manager

library(readxl)
library(dplyr)
library(lubridate)

#' Excel Portfolio Manager Class
#' Manages portfolio data from Excel file with format: date, symbol, weight
ExcelPortfolioManager <- R6Class("ExcelPortfolioManager",
  private = list(
    excel_path = NULL,
    portfolio_data = NULL,
    last_modified = NULL,
    default_investment = 10000
  ),
  
  public = list(
    #' Initialize the manager
    #' @param excel_path Path to the Excel file
    initialize = function(excel_path = "portfolio.xlsx") {
      private$excel_path <- excel_path
      private$default_investment <- 10000
      self$reload_portfolio_data()
    },
    
    #' Reload portfolio data from Excel file
    reload_portfolio_data = function() {
      if (!file.exists(private$excel_path)) {
        warning(paste("Excel file not found:", private$excel_path))
        private$portfolio_data <- data.frame()
        return(invisible(self))
      }
      
      # Check if file was modified
      current_modified <- file.info(private$excel_path)$mtime
      if (!is.null(private$last_modified) && 
          current_modified == private$last_modified) {
        return(invisible(self))  # No need to reload
      }
      
      tryCatch({
        # Read Excel file
        raw_data <- read_excel(private$excel_path)
        
        # Standardize column names
        names(raw_data) <- tolower(trimws(names(raw_data)))
        
        # Validate required columns
        required_cols <- c("date", "symbol", "weight")
        if (!all(required_cols %in% names(raw_data))) {
          stop(paste("Excel file must contain columns:", paste(required_cols, collapse = ", ")))
        }
        
        # Clean and process data
        private$portfolio_data <- raw_data %>%
          select(date, symbol, weight) %>%
          mutate(
            date = as.Date(date),
            symbol = toupper(trimws(symbol)),
            weight = as.numeric(gsub(",", ".", as.character(weight)))  # Handle European decimal format
          ) %>%
          filter(!is.na(date), !is.na(symbol), !is.na(weight), weight > 0) %>%
          arrange(date, symbol)
        
        private$last_modified <- current_modified
        message(paste("Successfully loaded", nrow(private$portfolio_data), "portfolio entries"))
        
      }, error = function(e) {
        warning(paste("Error reading Excel file:", e$message))
        private$portfolio_data <- data.frame()
      })
      
      invisible(self)
    },
    
    #' Get all unique portfolio dates
    get_portfolio_dates = function() {
      if (is.null(private$portfolio_data) || nrow(private$portfolio_data) == 0) {
        return(character())
      }
      
      unique_dates <- sort(unique(private$portfolio_data$date), decreasing = TRUE)
      return(unique_dates)
    },
    
    #' Get the current (most recent) portfolio
    get_current_portfolio = function() {
      dates <- self$get_portfolio_dates()
      if (length(dates) == 0) return(NULL)
      
      current_date <- dates[1]  # Most recent date
      return(self$get_portfolio_by_date(current_date))
    },
    
    #' Get portfolio by specific date
    #' @param target_date Date to get portfolio for
    get_portfolio_by_date = function(target_date) {
      if (is.null(private$portfolio_data) || nrow(private$portfolio_data) == 0) {
        return(NULL)
      }
      
      target_date <- as.Date(target_date)
      portfolio_data <- private$portfolio_data %>%
        filter(date == target_date)
      
      if (nrow(portfolio_data) == 0) return(NULL)
      
      # Normalize weights to sum to 1
      total_weight <- sum(portfolio_data$weight)
      portfolio_data$weight <- portfolio_data$weight / total_weight
      
      list(
        symbols = portfolio_data$symbol,
        weights = portfolio_data$weight,
        start_date = target_date,
        total_investment = private$default_investment,
        created_at = target_date,
        modified_at = target_date
      )
    },
    
    #' Get all portfolios formatted for the app
    get_all_portfolios = function() {
      dates <- self$get_portfolio_dates()
      if (length(dates) == 0) return(list())
      
      portfolios <- list()
      
      for (i in seq_along(dates)) {
        date <- dates[i]
        portfolio <- self$get_portfolio_by_date(date)
        
        if (!is.null(portfolio)) {
          # Create descriptive name
          if (i == 1) {
            portfolio_name <- paste("Current Portfolio", format(date, "%Y-%m-%d"))
          } else {
            portfolio_name <- paste("Portfolio", format(date, "%Y-%m-%d"))
          }
          
          portfolios[[portfolio_name]] <- portfolio
        }
      }
      
      return(portfolios)
    },
    
    #' Get portfolio summary
    get_portfolio_summary = function() {
      if (is.null(private$portfolio_data) || nrow(private$portfolio_data) == 0) {
        return(data.frame(Message = "No portfolio data available"))
      }
      
      summary <- private$portfolio_data %>%
        group_by(date) %>%
        summarise(
          symbols = paste(symbol, collapse = ", "),
          total_positions = n(),
          weight_check = round(sum(weight), 3),
          .groups = 'drop'
        ) %>%
        arrange(desc(date)) %>%
        mutate(
          portfolio_name = ifelse(row_number() == 1, 
                                 paste("Current Portfolio", format(date, "%Y-%m-%d")),
                                 paste("Portfolio", format(date, "%Y-%m-%d"))),
          status = ifelse(abs(weight_check - 1) < 0.01, "Valid", "Needs Review")
        ) %>%
        select(portfolio_name, date, total_positions, weight_check, status)
      
      return(summary)
    },
    
    #' Print current state
    print = function() {
      cat("Excel Portfolio Manager\n")
      cat("Excel file:", private$excel_path, "\n")
      cat("File exists:", file.exists(private$excel_path), "\n")
      
      if (!is.null(private$portfolio_data)) {
        dates <- self$get_portfolio_dates()
        cat("Portfolio dates available:", length(dates), "\n")
        if (length(dates) > 0) {
          cat("Current portfolio date:", format(dates[1], "%Y-%m-%d"), "\n")
          cat("Oldest portfolio date:", format(dates[length(dates)], "%Y-%m-%d"), "\n")
        }
      }
    }
  )
)