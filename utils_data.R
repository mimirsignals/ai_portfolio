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


#' Calculate portfolio performance with rebalancing inheritance
#' @param symbols Vector of stock symbols
#' @param weights Vector of portfolio weights  
#' @param start_date Start date for calculation
#' @param total_investment Total investment amount
#' @param portfolio_name Name of portfolio (for identification)
#' @param all_portfolios List of all portfolio definitions (for inheritance)
#' @return List containing portfolio_tbl and individual_stocks data
run_portfolio_calculations_with_inheritance <- function(symbols, weights, start_date, total_investment, portfolio_name = "Portfolio", all_portfolios = NULL) {
  
  # Input validation
  if (length(symbols) == 0 || length(weights) == 0) {
    warning("Empty symbols or weights provided")
    return(NULL)
  }
  
  # Check if this portfolio should inherit from a previous one
  parent_performance <- find_parent_portfolio_performance(start_date, all_portfolios, total_investment)
  
  if (!is.null(parent_performance)) {
    cat("Portfolio", portfolio_name, "inheriting performance from parent portfolio\n")
    
    # Calculate performance from rebalancing date forward
    current_performance <- calculate_portfolio_from_date(symbols, weights, start_date, total_investment, portfolio_name)
    
    if (!is.null(current_performance)) {
      # Merge inherited performance with current performance
      merged_performance <- merge_portfolio_performances(parent_performance, current_performance, start_date, total_investment)
      return(merged_performance)
    } else {
      # If current calculation fails, return inherited performance up to start date
      return(truncate_performance_to_date(parent_performance, start_date))
    }
  } else {
    # No inheritance needed, calculate normally
    return(run_portfolio_calculations(symbols, weights, start_date, total_investment, portfolio_name))
  }
}

#' Find parent portfolio performance to inherit from
find_parent_portfolio_performance <- function(current_start_date, all_portfolios, total_investment) {
  if (is.null(all_portfolios) || length(all_portfolios) == 0) return(NULL)
  
  # Find the most recent portfolio that started before current_start_date
  eligible_portfolios <- all_portfolios[sapply(all_portfolios, function(p) p$start_date < current_start_date)]
  
  if (length(eligible_portfolios) == 0) return(NULL)
  
  # Get the most recent parent (latest start date)
  parent_dates <- sapply(eligible_portfolios, function(p) p$start_date)
  most_recent_idx <- which.max(parent_dates)
  parent_portfolio <- eligible_portfolios[[most_recent_idx]]
  
  # Calculate parent portfolio performance
  parent_performance <- run_portfolio_calculations(
    parent_portfolio$symbols, 
    parent_portfolio$weights, 
    parent_portfolio$start_date, 
    total_investment,  # Use same total investment
    "Parent"
  )
  
  return(parent_performance)
}

#' Calculate portfolio performance starting from a specific date
calculate_portfolio_from_date <- function(symbols, weights, start_date, total_investment, portfolio_name) {
  # This uses the existing run_portfolio_calculations function
  return(run_portfolio_calculations(symbols, weights, start_date, total_investment, portfolio_name))
}

#' Merge parent performance with current performance at rebalancing date
merge_portfolio_performances <- function(parent_performance, current_performance, rebalance_date, total_investment) {
  if (is.null(parent_performance) || is.null(current_performance)) return(current_performance)
  
  # Get parent performance up to (but not including) rebalance date
  parent_pre_rebalance <- parent_performance$portfolio_tbl %>%
    filter(date < rebalance_date)
  
  # Get current performance from rebalance date forward
  current_post_rebalance <- current_performance$portfolio_tbl %>%
    filter(date >= rebalance_date)
  
  if (nrow(parent_pre_rebalance) == 0) {
    return(current_performance)
  }
  
  if (nrow(current_post_rebalance) == 0) {
    return(list(
      portfolio_tbl = parent_pre_rebalance,
      individual_stocks = parent_performance$individual_stocks %>%
        filter(date < rebalance_date)
    ))
  }
  
  # Get the portfolio value at rebalancing date from parent
  last_parent_value <- tail(parent_pre_rebalance$investment, 1)
  
  # Adjust current performance to start from parent's final value
  adjustment_factor <- last_parent_value / total_investment
  
  current_adjusted <- current_post_rebalance %>%
    mutate(investment = investment * adjustment_factor)
  
  # Combine the performances
  merged_portfolio_tbl <- bind_rows(parent_pre_rebalance, current_adjusted)
  
  # Handle individual stocks (inherit parent stocks pre-rebalance, current stocks post-rebalance)
  parent_stocks_pre <- parent_performance$individual_stocks %>%
    filter(date < rebalance_date)
  
  current_stocks_post <- current_performance$individual_stocks %>%
    filter(date >= rebalance_date) %>%
    mutate(investment = investment * adjustment_factor)
  
  merged_individual_stocks <- bind_rows(parent_stocks_pre, current_stocks_post)
  
  return(list(
    portfolio_tbl = merged_portfolio_tbl,
    individual_stocks = merged_individual_stocks
  ))
}

#' Truncate performance data to a specific date
truncate_performance_to_date <- function(performance, end_date) {
  if (is.null(performance)) return(NULL)
  
  return(list(
    portfolio_tbl = performance$portfolio_tbl %>% filter(date <= end_date),
    individual_stocks = performance$individual_stocks %>% filter(date <= end_date)
  ))
}