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
            symbol = toupper(trimws(as.character(symbol))),
            weight = as.numeric(gsub(",", ".", as.character(weight)))  # Handle European decimal format
          ) %>%
          filter(!is.na(date), !is.na(symbol), !is.na(weight), weight > 0, is.finite(weight)) %>%
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

# Bridge Portfolio Calculator that works with Excel data
bridgedPortfolioCalculator <- function() {
  function(portfolios, selected_portfolios, show_sp500 = FALSE, show_btc = FALSE) {
    
    if (length(portfolios) == 0 || length(selected_portfolios) == 0) {
      return(list(portfolios = list(), sp500 = NULL, bitcoin = NULL))
    }
    
    result <- list(portfolios = list(), sp500 = NULL, bitcoin = NULL)
    
    # Process each selected portfolio
    for (portfolio_name in selected_portfolios) {
      if (portfolio_name %in% names(portfolios)) {
        portfolio_info <- portfolios[[portfolio_name]]
        
        tryCatch({
          # Validate portfolio info
          if (is.null(portfolio_info$symbols) || is.null(portfolio_info$weights) || 
              is.null(portfolio_info$start_date) || is.null(portfolio_info$total_investment)) {
            warning(paste("Invalid portfolio data for", portfolio_name))
            next
          }
          
          # Ensure numeric types
          portfolio_info$weights <- as.numeric(portfolio_info$weights)
          portfolio_info$total_investment <- as.numeric(portfolio_info$total_investment)
          portfolio_info$start_date <- as.Date(portfolio_info$start_date)
          
          # Validate weights
          if (any(is.na(portfolio_info$weights)) || any(!is.finite(portfolio_info$weights))) {
            warning(paste("Invalid weights for portfolio", portfolio_name))
            next
          }
          
          # Get portfolio performance using the existing function
          portfolio_data <- calculate_portfolio_performance(
            symbols = portfolio_info$symbols,
            weights = portfolio_info$weights,
            start_date = portfolio_info$start_date,
            total_investment = portfolio_info$total_investment,
            portfolio_name = portfolio_name
          )
          
          if (!is.null(portfolio_data) && !is.null(portfolio_data$portfolio_tbl) && 
              nrow(portfolio_data$portfolio_tbl) > 0) {
            
            # Convert to format expected by modules
            dates <- as.Date(portfolio_data$portfolio_tbl$date)
            values <- as.numeric(portfolio_data$portfolio_tbl$investment)
            
            # Remove any non-finite values
            valid_indices <- is.finite(values) & !is.na(dates)
            dates <- dates[valid_indices]
            values <- values[valid_indices]
            
            if (length(dates) == 0 || length(values) == 0) {
              warning(paste("No valid data for portfolio", portfolio_name))
              next
            }
            
            # Calculate cumulative returns
            initial_value <- as.numeric(portfolio_info$total_investment)
            if (is.na(initial_value) || initial_value <= 0) {
              warning(paste("Invalid initial value for portfolio", portfolio_name))
              next
            }
            
            cumulative_returns <- (values / initial_value) - 1
            
            # Validate cumulative returns
            if (any(!is.finite(cumulative_returns))) {
              warning(paste("Invalid returns calculated for portfolio", portfolio_name))
              next
            }
            
            # Clean individual stocks data
            clean_individual_stocks <- NULL
            if (!is.null(portfolio_data$individual_stocks)) {
              clean_individual_stocks <- portfolio_data$individual_stocks %>%
                mutate(
                  date = as.Date(date),
                  investment = as.numeric(investment),
                  daily_return = as.numeric(daily_return),
                  cumulative_return = as.numeric(cumulative_return)
                ) %>%
                filter(is.finite(investment), is.finite(daily_return), is.finite(cumulative_return))
            }
            
            result$portfolios[[portfolio_name]] <- list(
              dates = dates,
              cumulative_returns = cumulative_returns,
              individual_stocks = clean_individual_stocks,
              portfolio_tbl = portfolio_data$portfolio_tbl
            )
          }
        }, error = function(e) {
          warning(paste("Error calculating portfolio", portfolio_name, ":", e$message))
        })
      }
    }
    
    # Add benchmark data if requested and we have valid portfolios
    if ((show_sp500 || show_btc) && length(result$portfolios) > 0) {
      # Find earliest start date
      earliest_date <- min(sapply(selected_portfolios, function(name) {
        if (name %in% names(portfolios) && !is.null(portfolios[[name]]$start_date)) {
          as.Date(portfolios[[name]]$start_date)
        } else {
          Sys.Date()
        }
      }))
      
      if (show_sp500) {
        tryCatch({
          sp500_raw <- fetch_benchmark_data("^GSPC", earliest_date, initial_value = 10000)
          if (!is.null(sp500_raw) && nrow(sp500_raw) > 0) {
            # Clean benchmark data
            sp500_clean <- sp500_raw %>%
              mutate(
                date = as.Date(date),
                value = as.numeric(value)
              ) %>%
              filter(is.finite(value), !is.na(date))
            
            if (nrow(sp500_clean) > 0) {
              # Convert to expected format
              result$sp500 <- list(
                dates = sp500_clean$date,
                cumulative_returns = (sp500_clean$value / 10000) - 1
              )
            }
          }
        }, error = function(e) {
          warning(paste("Error fetching S&P 500 data:", e$message))
        })
      }
      
      if (show_btc) {
        tryCatch({
          btc_raw <- fetch_benchmark_data("BTC-USD", earliest_date, initial_value = 10000)
          if (!is.null(btc_raw) && nrow(btc_raw) > 0) {
            # Clean benchmark data
            btc_clean <- btc_raw %>%
              mutate(
                date = as.Date(date),
                value = as.numeric(value)
              ) %>%
              filter(is.finite(value), !is.na(date))
            
            if (nrow(btc_clean) > 0) {
              # Convert to expected format
              result$bitcoin <- list(
                dates = btc_clean$date,
                cumulative_returns = (btc_clean$value / 10000) - 1
              )
            }
          }
        }, error = function(e) {
          warning(paste("Error fetching Bitcoin data:", e$message))
        })
      }
    }
    
    return(result)
  }
}