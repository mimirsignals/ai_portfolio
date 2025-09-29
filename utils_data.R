# utils_data.R - Fixed Portfolio Performance Calculation

library(quantmod)
library(dplyr)
library(lubridate)


.stock_data_cache <- new.env(parent = emptyenv())

clear_stock_data_cache <- function(symbols = NULL) {
  if (is.null(symbols)) {
    rm(list = ls(.stock_data_cache), envir = .stock_data_cache)
  } else {
    rm(list = intersect(as.character(symbols), ls(.stock_data_cache)), envir = .stock_data_cache)
  }
  invisible(TRUE)
}

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
        filter(symbol == current_symbol) %>% 
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

  symbols <- unique(symbols[!is.na(symbols) & symbols != ""])
  if (length(symbols) == 0) {
    warning("No valid symbols provided to fetch_stock_data")
    return(NULL)
  }

  start_date <- suppressWarnings(as.Date(start_date, origin = "1970-01-01"))
  end_date <- Sys.Date() + 1  # Add 1 day to include current date data

  if (is.na(start_date)) {
    start_date <- end_date - 365
  }

  if (start_date > end_date) {
    start_date <- end_date - 365
  }

  if (as.numeric(end_date - start_date) < 5) {
    start_date <- end_date - 5
  }

  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  fetch_quantmod <- function(symbol) {
    tryCatch({
      xts_data <- suppressWarnings(suppressMessages(
        quantmod::getSymbols(
          symbol,
          from = start_date,
          to = end_date,
          auto.assign = FALSE
        )
      ))

      if (is.null(xts_data) || nrow(xts_data) == 0) {
        return(NULL)
      }

      stock_df <- data.frame(
        date = as.Date(index(xts_data)),
        symbol = as.character(symbol),
        adjusted_price = as.numeric(quantmod::Ad(xts_data)),
        stringsAsFactors = FALSE
      )

      stock_df[is.finite(stock_df$adjusted_price) & stock_df$adjusted_price > 0 & !is.na(stock_df$date), , drop = FALSE]
    }, error = function(e) {
      NULL
    })
  }

  fetch_yahoo_csv <- function(symbol) {
    period1 <- as.integer(as.POSIXct(start_date, tz = "UTC"))
    period2 <- as.integer(as.POSIXct(end_date + 1, tz = "UTC"))
    yahoo_url <- sprintf(
      "https://query1.finance.yahoo.com/v7/finance/download/%s?period1=%d&period2=%d&interval=1d&events=history&includeAdjustedClose=true",
      utils::URLencode(symbol, reserved = TRUE),
      period1,
      period2
    )

    csv_df <- tryCatch(
      utils::read.csv(yahoo_url, stringsAsFactors = FALSE),
      error = function(e) NULL,
      warning = function(w) NULL
    )

    if (is.null(csv_df) || !("Adj.Close" %in% names(csv_df))) {
      return(NULL)
    }

    csv_df <- csv_df[!is.na(csv_df$Adj.Close) & csv_df$Adj.Close > 0 & !is.na(csv_df$Date), , drop = FALSE]
    if (nrow(csv_df) == 0) {
      return(NULL)
    }

    data.frame(
      date = as.Date(csv_df$Date),
      symbol = as.character(symbol),
      adjusted_price = as.numeric(csv_df$Adj.Close),
      stringsAsFactors = FALSE
    )
  }

  get_symbol_data <- function(symbol) {
    cache_key <- paste(symbol, start_date, end_date, sep = "|")
    if (exists(cache_key, envir = .stock_data_cache, inherits = FALSE)) {
      return(get(cache_key, envir = .stock_data_cache, inherits = FALSE))
    }

    stock_df <- fetch_quantmod(symbol)
    if (is.null(stock_df) || nrow(stock_df) == 0) {
      stock_df <- fetch_yahoo_csv(symbol)
    }

    if (!is.null(stock_df) && nrow(stock_df) > 0) {
      assign(cache_key, stock_df, envir = .stock_data_cache)
      return(stock_df)
    }

    NULL
  }

  stock_data_list <- lapply(symbols, get_symbol_data)
  valid_entries <- vapply(stock_data_list, function(x) !is.null(x) && nrow(x) > 0, logical(1))

  if (!any(valid_entries)) {
    warning("No valid stock data retrieved")
    return(NULL)
  }

  all_stock_data <- dplyr::bind_rows(stock_data_list[valid_entries]) %>%
    dplyr::mutate(
      date = as.Date(date),
      symbol = as.character(symbol),
      adjusted_price = as.numeric(adjusted_price)
    ) %>%
    dplyr::filter(
      !is.na(date),
      !is.na(symbol),
      symbol != "",
      !is.na(adjusted_price),
      is.finite(adjusted_price),
      adjusted_price > 0,
      date >= start_date,
      date <= end_date
    ) %>%
    dplyr::arrange(date, symbol) %>%
    dplyr::distinct(date, symbol, .keep_all = TRUE)

  if (nrow(all_stock_data) == 0) {
    warning("No valid stock data retrieved")
    return(NULL)
  }

  return(all_stock_data)
}
