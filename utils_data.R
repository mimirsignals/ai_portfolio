# utils_data.R - Fixed Portfolio Performance Calculation

library(quantmod)
library(dplyr)
library(lubridate)

#' Calculate portfolio performance with proper error handling
#' @param symbols Vector of stock symbols
#' @param weights Vector of portfolio weights
#' @param start_date Start date for calculation
#' @param total_investment Total investment amount
#' @param portfolio_name Name of portfolio (for identification)
#' @return List containing portfolio_tbl and individual_stocks data
run_portfolio_calculations <- function(symbols, weights, start_date, total_investment, portfolio_name = "Portfolio") {
  
  # Input validation
  if (length(symbols) == 0 || length(weights) == 0) {
    warning("Empty symbols or weights provided")
    return(NULL)
  }
  
  if (length(symbols) != length(weights)) {
    warning("Symbols and weights have different lengths")
    return(NULL)
  }
  
  # Ensure numeric types
  weights <- as.numeric(weights)
  total_investment <- as.numeric(total_investment)
  start_date <- as.Date(start_date)
  
  # Validate inputs
  if (any(is.na(weights)) || any(weights < 0) || sum(weights) == 0) {
    warning("Invalid weights provided")
    return(NULL)
  }
  
  if (is.na(total_investment) || total_investment <= 0) {
    warning("Invalid total investment amount")
    return(NULL)
  }
  
  if (is.na(start_date)) {
    warning("Invalid start date")
    return(NULL)
  }
  
  # Normalize weights
  weights <- weights / sum(weights)
  
  tryCatch({
    # Get stock data
    stock_data <- fetch_stock_data(symbols, start_date)
    
    if (is.null(stock_data) || nrow(stock_data) == 0) {
      warning("No stock data available")
      return(NULL)
    }
    
    # Calculate portfolio performance
    portfolio_performance <- calculate_weighted_portfolio(stock_data, symbols, weights, total_investment)
    
    if (is.null(portfolio_performance)) {
      warning("Portfolio calculation failed")
      return(NULL)
    }
    
    return(list(
      portfolio_tbl = portfolio_performance$portfolio_tbl,
      individual_stocks = portfolio_performance$individual_stocks
    ))
    
  }, error = function(e) {
    warning(paste("Error calculating portfolio performance:", e$message))
    return(NULL)
  })
}

#' Fetch stock data for given symbols
#' @param symbols Vector of stock symbols
#' @param start_date Start date for data
#' @return Data frame with stock data
fetch_stock_data <- function(symbols, start_date) {
  
  # Ensure we have valid symbols
  symbols <- unique(symbols[!is.na(symbols) & symbols != ""])
  
  if (length(symbols) == 0) {
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
        # Convert to data frame
        stock_df <- data.frame(
          date = index(stock_xts),
          symbol = symbol,
          adjusted_price = as.numeric(Ad(stock_xts)),
          stringsAsFactors = FALSE
        )
        
        # Filter out invalid prices
        stock_df <- stock_df %>%
          filter(!is.na(adjusted_price), adjusted_price > 0, !is.na(date))
        
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
  
  return(all_stock_data)
}

#' Calculate weighted portfolio performance
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
    # Calculate individual stock investments
    symbol_investments <- total_investment * weights
    names(symbol_investments) <- symbols
    
    # Calculate individual stock performance
    individual_stocks <- data.frame()
    
    for (i in seq_along(symbols)) {
      symbol <- symbols[i]
      investment <- symbol_investments[symbol]
      
      symbol_data <- stock_data %>%
        filter(symbol == !!symbol) %>%
        arrange(date)
      
      if (nrow(symbol_data) > 0) {
        # Calculate performance based on price changes
        initial_price <- symbol_data$adjusted_price[1]
        
        symbol_performance <- symbol_data %>%
          mutate(
            price_return = adjusted_price / initial_price,
            investment = investment * price_return,
            daily_return = c(0, diff(log(adjusted_price)))  # Log returns
          ) %>%
          select(date, symbol, investment, daily_return)
        
        individual_stocks <- rbind(individual_stocks, symbol_performance)
      }
    }
    
    if (nrow(individual_stocks) == 0) {
      return(NULL)
    }
    
    # Calculate portfolio totals
    portfolio_tbl <- individual_stocks %>%
      group_by(date) %>%
      summarise(
        investment = sum(investment, na.rm = TRUE),
        daily_return = weighted.mean(daily_return, w = symbol_investments[symbol], na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      arrange(date)
    
    # Ensure we have valid portfolio data
    portfolio_tbl <- portfolio_tbl %>%
      filter(!is.na(investment), is.finite(investment), investment > 0)
    
    if (nrow(portfolio_tbl) == 0) {
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