# utils_data.R - Fixed Portfolio Performance Calculation

library(quantmod)
library(dplyr)
library(lubridate)

#' Calculate weighted portfolio performance - FIXED VERSION
#' @param stock_data Data frame with stock data
#' @param symbols Vector of symbols
#' @param weights Vector of weights
#' @param total_investment Total investment amount
#' @return List with portfolio and individual stock performance
calculate_weighted_portfolio <- function(stock_data, symbols, weights, total_investment) {
  
  if (is.null(stock_data) || nrow(stock_data) == 0) {
    return(NULL)
  }
  
  tryCatch({
    # Ensure all inputs are properly typed
    weights <- as.numeric(weights)
    total_investment <- as.numeric(total_investment)
    symbols <- as.character(symbols)
    
    # Validate inputs
    if (any(is.na(weights)) || any(is.na(symbols)) || is.na(total_investment)) {
      warning("Invalid input data (NA values found)")
      return(NULL)
    }
    
    if (total_investment <= 0) {
      warning("Total investment must be positive")
      return(NULL)
    }
    
    # Calculate individual stock investments
    symbol_investments <- total_investment * weights
    names(symbol_investments) <- symbols
    
    # Calculate individual stock performance
    individual_stocks <- data.frame()
    
    for (i in seq_along(symbols)) {
      current_symbol <- symbols[i]
      investment <- symbol_investments[current_symbol]
      
      # FIX: Corrected the filter condition
      symbol_data <- stock_data %>%
        filter(symbol == current_symbol) %>%  # FIXED: was filter(symbol == !symbol)
        arrange(date)
      
      if (nrow(symbol_data) > 0) {
        # Calculate performance based on price changes
        initial_price <- symbol_data$adjusted_price[1]
        
        if (is.na(initial_price) || initial_price <= 0) {
          warning(paste("Invalid initial price for symbol:", current_symbol))
          next
        }
        
        symbol_performance <- symbol_data %>%
          mutate(
            # Ensure all calculations use numeric values
            adjusted_price = as.numeric(adjusted_price),
            price_return = adjusted_price / initial_price,
            investment = as.numeric(investment * price_return),
            daily_return = c(0, diff(log(adjusted_price)))  # Log returns
          ) %>%
          # Add symbol as character to avoid factor issues
          mutate(symbol = as.character(current_symbol)) %>%
          select(date, symbol, investment, daily_return) %>%
          # Filter out any invalid values
          filter(is.finite(investment), is.finite(daily_return))
        
        individual_stocks <- rbind(individual_stocks, symbol_performance)
      }
    }
    
    if (nrow(individual_stocks) == 0) {
      warning("No valid individual stock data calculated")
      return(NULL)
    }
    
    # Ensure individual_stocks data types are correct
    individual_stocks <- individual_stocks %>%
      mutate(
        date = as.Date(date),
        symbol = as.character(symbol),
        investment = as.numeric(investment),
        daily_return = as.numeric(daily_return)
      ) %>%
      filter(is.finite(investment), is.finite(daily_return))
    
    # Calculate portfolio totals with proper error handling
    portfolio_tbl <- individual_stocks %>%
      group_by(date) %>%
      summarise(
        investment = sum(investment, na.rm = TRUE),
        # Fix weighted mean calculation
        daily_return = {
          valid_returns <- daily_return[is.finite(daily_return)]
          valid_weights <- symbol_investments[symbol[is.finite(daily_return)]]
          if (length(valid_returns) > 0 && sum(valid_weights, na.rm = TRUE) > 0) {
            weighted.mean(valid_returns, w = valid_weights, na.rm = TRUE)
          } else {
            0
          }
        },
        .groups = 'drop'
      ) %>%
      arrange(date)
    
    # Ensure we have valid portfolio data
    portfolio_tbl <- portfolio_tbl %>%
      mutate(
        investment = as.numeric(investment),
        daily_return = as.numeric(daily_return)
      ) %>%
      filter(!is.na(investment), is.finite(investment), investment > 0)
    
    if (nrow(portfolio_tbl) == 0) {
      warning("No valid portfolio data after filtering")
      return(NULL)
    }
    
    return(list(
      portfolio_tbl = portfolio_tbl,
      individual_stocks = individual_stocks
    ))
    
  }, error = function(e) {
    warning(paste("Error in weighted portfolio calculation:", e$message))
    return(NULL)
  })
}

#' Calculate portfolio performance with proper error handling - FIXED VERSION
#' @param symbols Vector of stock symbols
#' @param weights Vector of portfolio weights
#' @param start_date Start date for calculation
#' @param total_investment Total investment amount
#' @param portfolio_name Name of portfolio (for identification)
#' @return List containing portfolio_tbl and individual_stocks data
run_portfolio_calculations <- function(symbols, weights, start_date, total_investment, portfolio_name = "Portfolio") {
  
  # Input validation with better error messages
  if (length(symbols) == 0 || length(weights) == 0) {
    warning(paste("Empty symbols or weights provided for portfolio:", portfolio_name))
    return(NULL)
  }
  
  if (length(symbols) != length(weights)) {
    warning(paste("Symbols and weights have different lengths for portfolio:", portfolio_name))
    return(NULL)
  }
  
  # Ensure numeric types with better error handling
  tryCatch({
    weights <- as.numeric(weights)
    total_investment <- as.numeric(total_investment)
    start_date <- as.Date(start_date)
    symbols <- as.character(symbols)
  }, error = function(e) {
    warning(paste("Error converting data types for portfolio:", portfolio_name, "-", e$message))
    return(NULL)
  })
  
  # Validate inputs
  if (any(is.na(weights)) || any(weights < 0) || sum(weights) == 0) {
    warning(paste("Invalid weights provided for portfolio:", portfolio_name))
    return(NULL)
  }
  
  if (is.na(total_investment) || total_investment <= 0) {
    warning(paste("Invalid total investment amount for portfolio:", portfolio_name))
    return(NULL)
  }
  
  if (is.na(start_date)) {
    warning(paste("Invalid start date for portfolio:", portfolio_name))
    return(NULL)
  }
  
  # Remove any empty or NA symbols
  valid_indices <- !is.na(symbols) & symbols != ""
  symbols <- symbols[valid_indices]
  weights <- weights[valid_indices]
  
  if (length(symbols) == 0) {
    warning(paste("No valid symbols for portfolio:", portfolio_name))
    return(NULL)
  }
  
  # Normalize weights
  weights <- weights / sum(weights)
  
  tryCatch({
    # Get stock data
    stock_data <- fetch_stock_data(symbols, start_date)
    
    if (is.null(stock_data) || nrow(stock_data) == 0) {
      warning(paste("No stock data available for portfolio:", portfolio_name))
      return(NULL)
    }
    
    # Calculate portfolio performance
    portfolio_performance <- calculate_weighted_portfolio(stock_data, symbols, weights, total_investment)
    
    if (is.null(portfolio_performance)) {
      warning(paste("Portfolio calculation failed for portfolio:", portfolio_name))
      return(NULL)
    }
    
    return(list(
      portfolio_tbl = portfolio_performance$portfolio_tbl,
      individual_stocks = portfolio_performance$individual_stocks
    ))
    
  }, error = function(e) {
    warning(paste("Error calculating portfolio performance for", portfolio_name, ":", e$message))
    return(NULL)
  })
}

#' Fetch stock data for given symbols - ENHANCED VERSION
#' @param symbols Vector of stock symbols
#' @param start_date Start date for data
#' @return Data frame with stock data
fetch_stock_data <- function(symbols, start_date) {
  
  # Ensure we have valid symbols
  symbols <- unique(symbols[!is.na(symbols) & symbols != ""])
  
  if (length(symbols) == 0) {
    warning("No valid symbols provided to fetch_stock_data")
    return(NULL)
  }
  
  # Set reasonable end date (today + 1 to include today's data)
  end_date <- Sys.Date() + 1
  
  # Ensure start_date is reasonable
  if (start_date > end_date) {
    start_date <- end_date - 365  # Default to 1 year back
  }
  
  all_stock_data <- data.frame()
  
  for (symbol in symbols) {
    tryCatch({
      # Fetch data using quantmod
      stock_xts <- getSymbols(symbol, 
                             from = start_date, 
                             to = end_date, 
                             auto.assign = FALSE,
                             warnings = FALSE)
      
      if (!is.null(stock_xts) && nrow(stock_xts) > 0) {
        # Convert to data frame with proper data types
        stock_df <- data.frame(
          date = as.Date(index(stock_xts)),
          symbol = as.character(symbol),
          adjusted_price = as.numeric(Ad(stock_xts)),
          stringsAsFactors = FALSE
        )
        
        # Filter out invalid prices and ensure data types
        stock_df <- stock_df %>%
          mutate(
            date = as.Date(date),
            symbol = as.character(symbol),
            adjusted_price = as.numeric(adjusted_price)
          ) %>%
          filter(
            !is.na(adjusted_price), 
            is.finite(adjusted_price), 
            adjusted_price > 0, 
            !is.na(date)
          )
        
        if (nrow(stock_df) > 0) {
          all_stock_data <- rbind(all_stock_data, stock_df)
        }
      }
    }, error = function(e) {
      warning(paste("Failed to fetch data for", symbol, ":", e$message))
    })
    
    # Small delay to be nice to data providers
    Sys.sleep(0.1)
  }
  
  if (nrow(all_stock_data) == 0) {
    warning("No valid stock data retrieved")
    return(NULL)
  }
  
  # Final data type validation
  all_stock_data <- all_stock_data %>%
    mutate(
      date = as.Date(date),
      symbol = as.character(symbol),
      adjusted_price = as.numeric(adjusted_price)
    ) %>%
    filter(
      !is.na(date),
      !is.na(symbol),
      symbol != "",
      !is.na(adjusted_price),
      is.finite(adjusted_price),
      adjusted_price > 0
    )
  
  return(all_stock_data)
}