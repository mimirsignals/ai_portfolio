# R/utils_portfolio.R

#' Main calculation function for selected portfolios and benchmarks
calculate_all_portfolios <- function(portfolios, selected_portfolios, show_sp500 = TRUE, show_btc = TRUE) {
  if (length(portfolios) == 0 || length(selected_portfolios) == 0) return(NULL)

  calculated_portfolios <- list()
  earliest_date <- Sys.Date()

  for (name in selected_portfolios) {
    if (name %in% names(portfolios)) {
      p_def <- portfolios[[name]]
      earliest_date <- min(earliest_date, p_def$start_date)
      
      perf_data <- run_portfolio_calculations(
        p_def$symbols, p_def$weights, p_def$start_date, p_def$total_investment, name
      )
      
      if (!is.null(perf_data)) {
        cumulative_returns <- (perf_data$portfolio_tbl$investment / p_def$total_investment) - 1
        calculated_portfolios[[name]] <- list(
          dates = perf_data$portfolio_tbl$date,
          cumulative_returns = cumulative_returns,
          individual_stocks = perf_data$individual_stocks
        )
      } else {
        warning(paste("Could not calculate performance for portfolio:", name))
      }
    }
  }

  fetch_benchmark <- function(symbol, start) {
    raw_data <- fetch_stock_data(symbol, start_date = start)
    if (is.null(raw_data) || nrow(raw_data) == 0) return(NULL)
    
    # Calculate cumulative return for the benchmark
    initial_price <- raw_data$adjusted_price[1]
    raw_data <- raw_data %>%
      dplyr::mutate(cumulative_return = (adjusted_price / initial_price) - 1)
      
    list(dates = raw_data$date, cumulative_returns = raw_data$cumulative_return)
  }

  list(
    portfolios = calculated_portfolios,
    sp500 = if (show_sp500) fetch_benchmark("^GSPC", earliest_date) else NULL,
    bitcoin = if (show_btc) fetch_benchmark("BTC-USD", earliest_date) else NULL
  )
}


#' Enhanced calculation function with portfolio inheritance support
calculate_all_portfolios_with_inheritance <- function(portfolios, selected_portfolios, show_sp500 = TRUE, show_btc = TRUE) {
  if (length(portfolios) == 0 || length(selected_portfolios) == 0) return(NULL)

  available <- intersect(selected_portfolios, names(portfolios))
  if (length(available) == 0) return(NULL)

  portfolio_groups <- split(available, vapply(portfolios[available], function(p) p$portfolio_name, character(1)))

  group_earliest <- lapply(portfolio_groups, function(keys) {
    dates <- vapply(keys, function(key) as.Date(portfolios[[key]]$start_date), as.Date(Sys.Date()))
    min(dates, na.rm = TRUE)
  })

  earliest_by_group <- vapply(group_earliest, identity, as.Date(Sys.Date()))
  earliest_date <- min(earliest_by_group, na.rm = TRUE)

  calculated_portfolios <- list()

  for (name in available) {
    p_def <- portfolios[[name]]

    perf_data <- run_portfolio_calculations_with_inheritance(
      p_def$symbols, p_def$weights, p_def$start_date, p_def$total_investment, name, portfolios
    )

    if (!is.null(perf_data)) {
      perf_tbl <- perf_data$portfolio_tbl %>%
        dplyr::mutate(date = as.Date(date)) %>%
        dplyr::arrange(date)

      earliest_group_date <- earliest_by_group[[portfolios[[name]]$portfolio_name]]
      perf_tbl <- perf_data$portfolio_tbl %>%
        dplyr::mutate(date = as.Date(date)) %>%
        dplyr::arrange(date)

      keep <- perf_tbl$date >= earliest_group_date
      perf_tbl <- perf_tbl[keep, , drop = FALSE]

      if (nrow(perf_tbl) == 0) {
        warning(paste("No performance data after earliest group date for portfolio:", name))
        next
      }

      baseline <- perf_tbl$investment[1]
      if (is.na(baseline) || baseline == 0) {
        warning(paste("Baseline investment missing at earliest group date for portfolio:", name))
        next
      }

      cumulative_returns <- (perf_tbl$investment / baseline) - 1

      individual_stocks <- perf_data$individual_stocks
      if (!is.null(individual_stocks)) {
        individual_stocks <- individual_stocks %>%
          dplyr::mutate(date = as.Date(date)) %>%
          dplyr::filter(date >= earliest_group_date)
      }

      calculated_portfolios[[name]] <- list(
        dates = perf_tbl$date,
        cumulative_returns = cumulative_returns,
        individual_stocks = individual_stocks
      )
    } else {
      warning(paste("Could not calculate performance for portfolio:", name))
    }
  }

  fetch_benchmark <- function(symbol, start) {
    raw_data <- fetch_stock_data(symbol, start_date = start)
    if (is.null(raw_data) || nrow(raw_data) == 0) return(NULL)

    initial_price <- raw_data$adjusted_price[1]
    raw_data <- raw_data %>%
      dplyr::mutate(cumulative_return = (adjusted_price / initial_price) - 1)

    list(dates = raw_data$date, cumulative_returns = raw_data$cumulative_return)
  }

  list(
    portfolios = calculated_portfolios,
    sp500 = if (show_sp500) fetch_benchmark("^GSPC", earliest_date) else NULL,
    bitcoin = if (show_btc) fetch_benchmark("BTC-USD", earliest_date) else NULL
  )
}



# utils_portfolio_fixed.R - Complete Portfolio Inheritance Implementation

#' Portfolio calculations with inheritance support - COMPLETE IMPLEMENTATION
#' @param symbols Vector of stock symbols for current portfolio
#' @param weights Vector of portfolio weights for current portfolio  
#' @param start_date Start date for current portfolio (rebalancing date)
#' @param total_investment Initial investment amount (only used for first portfolio)
#' @param portfolio_name Name of current portfolio
#' @param all_portfolios List of all portfolio definitions (for inheritance detection)
#' @return List containing portfolio_tbl and individual_stocks data
run_portfolio_calculations_with_inheritance <- function(symbols, weights, start_date, total_investment, portfolio_name, all_portfolios) {
  
  # Input validation
  if (length(symbols) == 0 || length(weights) == 0) {
    warning(paste("Empty symbols or weights provided for portfolio:", portfolio_name))
    return(NULL)
  }
  
  if (length(symbols) != length(weights)) {
    warning(paste("Symbols and weights have different lengths for portfolio:", portfolio_name))
    return(NULL)
  }
  
  # Convert types
  weights <- as.numeric(weights)
  total_investment <- as.numeric(total_investment)
  start_date <- as.Date(start_date)
  symbols <- as.character(symbols)
  
  # Validate inputs
  if (any(is.na(weights)) || any(weights < 0) || sum(weights) == 0) {
    warning(paste("Invalid weights provided for portfolio:", portfolio_name))
    return(NULL)
  }
  
  weights <- weights / sum(weights)  # Normalize weights
  
  # INHERITANCE LOGIC: Find previous portfolio
  portfolio_dates <- sapply(all_portfolios, function(p) as.Date(p$start_date))
  portfolio_dates_sorted <- sort(portfolio_dates)
  current_date <- start_date
  
  # Check if this is the first portfolio
  is_first_portfolio <- current_date == min(portfolio_dates_sorted)
  
  if (!is_first_portfolio) {
    # Find the previous portfolio
    previous_dates <- portfolio_dates_sorted[portfolio_dates_sorted < current_date]
    
    if (length(previous_dates) > 0) {
      previous_date <- max(previous_dates)
      previous_portfolio_name <- names(all_portfolios)[portfolio_dates == previous_date][1]
      previous_portfolio <- all_portfolios[[previous_portfolio_name]]
      
      # Calculate previous portfolio performance to get inherited value
      previous_perf <- run_portfolio_calculations(
        previous_portfolio$symbols,
        previous_portfolio$weights,
        previous_portfolio$start_date,
        previous_portfolio$total_investment,
        previous_portfolio_name
      )
      
      if (!is.null(previous_perf)) {
        # Find the portfolio value on the rebalancing date
        rebalance_date_value <- previous_perf$portfolio_tbl %>%
          filter(date <= current_date) %>%
          slice_tail(n = 1) %>%
          pull(investment)
        
        if (length(rebalance_date_value) > 0 && !is.na(rebalance_date_value)) {
          # Use inherited value instead of original investment
          total_investment <- rebalance_date_value
        }
      }
    }
  }
  
  # Now calculate the current portfolio performance
  tryCatch({
    # Get stock data starting from the day AFTER rebalancing
    # This ensures rebalancing happens "after market close"
    actual_start_date <- if (is_first_portfolio) start_date else start_date + 1
    
    stock_data <- fetch_stock_data(symbols, actual_start_date)
    
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
    
    # For inherited portfolios, we need to add the rebalancing date with inherited value
    if (!is_first_portfolio) {
      # Add the rebalancing date entry with inherited value
      rebalance_entry <- data.frame(
        date = start_date,
        investment = total_investment,
        daily_return = 0,
        stringsAsFactors = FALSE
      )
      
      # Combine rebalance date + calculated performance
      portfolio_performance$portfolio_tbl <- rbind(
        rebalance_entry,
        portfolio_performance$portfolio_tbl
      ) %>%
        arrange(date)
      
      # Also add individual stock entries for rebalancing date
      individual_rebalance <- data.frame(
        date = rep(start_date, length(symbols)),
        symbol = symbols,
        investment = total_investment * weights,
        daily_return = rep(0, length(symbols)),
        stringsAsFactors = FALSE
      )
      
      portfolio_performance$individual_stocks <- rbind(
        individual_rebalance,
        portfolio_performance$individual_stocks
      ) %>%
        arrange(date, symbol)
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

#' Portfolio calculations with inheritance support - COMPLETE IMPLEMENTATION
#' @param symbols Vector of stock symbols for current portfolio
#' @param weights Vector of portfolio weights for current portfolio  
#' @param start_date Start date for current portfolio (rebalancing date)
#' @param total_investment Initial investment amount (only used for first portfolio)
#' @param portfolio_name Name of current portfolio
#' @param all_portfolios List of all portfolio definitions (for inheritance detection)
#' @return List containing portfolio_tbl and individual_stocks data
run_portfolio_calculations_with_inheritance <- function(symbols, weights, start_date, total_investment, portfolio_name, all_portfolios) {
  
  # Input validation
  if (length(symbols) == 0 || length(weights) == 0) {
    warning(paste("Empty symbols or weights provided for portfolio:", portfolio_name))
    return(NULL)
  }
  
  if (length(symbols) != length(weights)) {
    warning(paste("Symbols and weights have different lengths for portfolio:", portfolio_name))
    return(NULL)
  }
  
  # Convert types
  weights <- as.numeric(weights)
  total_investment <- as.numeric(total_investment)
  start_date <- as.Date(start_date)
  symbols <- as.character(symbols)
  
  # Validate inputs
  if (any(is.na(weights)) || any(weights < 0) || sum(weights) == 0) {
    warning(paste("Invalid weights provided for portfolio:", portfolio_name))
    return(NULL)
  }
  
  weights <- weights / sum(weights)  # Normalize weights
  
  # INHERITANCE LOGIC: Find previous portfolio
  portfolio_dates <- sapply(all_portfolios, function(p) as.Date(p$start_date))
  portfolio_dates_sorted <- sort(portfolio_dates)
  current_date <- start_date
  
  # Check if this is the first portfolio
  is_first_portfolio <- current_date == min(portfolio_dates_sorted)
  
  if (!is_first_portfolio) {
    # Find the previous portfolio
    previous_dates <- portfolio_dates_sorted[portfolio_dates_sorted < current_date]
    
    if (length(previous_dates) > 0) {
      previous_date <- max(previous_dates)
      previous_portfolio_name <- names(all_portfolios)[portfolio_dates == previous_date][1]
      previous_portfolio <- all_portfolios[[previous_portfolio_name]]
      
      # Calculate previous portfolio performance 
      previous_perf <- run_portfolio_calculations(
        previous_portfolio$symbols,
        previous_portfolio$weights,
        previous_portfolio$start_date,
        previous_portfolio$total_investment,
        previous_portfolio_name
      )
      
      if (!is.null(previous_perf)) {
        # Get historical performance up to rebalancing date
        historical_performance <- previous_perf$portfolio_tbl %>%
          filter(date <= current_date)
        
        historical_individual <- previous_perf$individual_stocks %>%
          filter(date <= current_date)
        
        # Get the portfolio value on the rebalancing date for continuation
        rebalance_date_value <- historical_performance %>%
          slice_tail(n = 1) %>%
          pull(investment)
        
        if (length(rebalance_date_value) > 0 && !is.na(rebalance_date_value)) {
          total_investment <- rebalance_date_value
        }
        
        # Calculate NEW portfolio performance from rebalancing date forward
        stock_data <- fetch_stock_data(symbols, current_date)
        
        if (!is.null(stock_data) && nrow(stock_data) > 0) {
          new_portfolio_performance <- calculate_weighted_portfolio(stock_data, symbols, weights, total_investment)
          
          if (!is.null(new_portfolio_performance)) {
            # Combine historical + new performance
            combined_portfolio <- rbind(
              historical_performance,
              new_portfolio_performance$portfolio_tbl %>% filter(date > current_date)
            ) %>%
              arrange(date)
            
            combined_individual <- rbind(
              historical_individual,
              new_portfolio_performance$individual_stocks %>% filter(date > current_date)
            ) %>%
              arrange(date, symbol)
            
            return(list(
              portfolio_tbl = combined_portfolio,
              individual_stocks = combined_individual
            ))
          }
        }
      }
    }
  }
  
  # Now calculate the current portfolio performance
  tryCatch({
    # Get stock data starting from the day AFTER rebalancing
    # This ensures rebalancing happens "after market close"
    actual_start_date <- if (is_first_portfolio) start_date else start_date + 1
    
    stock_data <- fetch_stock_data(symbols, actual_start_date)
    
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
    
    # For inherited portfolios, adjust the initial investment for the first day
    if (!is_first_portfolio && !is.null(portfolio_performance)) {
      # The first day in the portfolio_tbl should use the inherited value
      # but calculate returns from the rebalancing date forward
      portfolio_performance$portfolio_tbl$investment[1] <- total_investment
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



