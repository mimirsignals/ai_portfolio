# R/utils_data.R - Data Fetching and Processing Utilities

#' Fetch stock data for multiple symbols
#' @param symbols Character vector of stock symbols
#' @param start_date Start date for data
#' @param end_date End date for data (default: today + 1)
#' @param auto_assign Whether to auto-assign to global environment
#' @return List of data frames with stock data
fetch_stock_data <- function(symbols, start_date, end_date = today() + 1, auto_assign = FALSE) {
  data_list <- list()
  successful_symbols <- c()
  failed_symbols <- c()
  
  for (symbol in symbols) {
    tryCatch({
      # Fetch data using quantmod
      stock_data <- getSymbols(symbol,
                              from = start_date,
                              to = end_date,
                              auto.assign = auto_assign)
      
      # Process and store
      processed_data <- stock_data %>%
        as.data.frame() %>%
        rownames_to_column("date") %>%
        as_tibble() %>%
        select(date, contains("Close")) %>%
        set_names("date", "price") %>%
        mutate(
          symbol = symbol,
          date = ymd(date)
        )
      
      data_list[[symbol]] <- processed_data
      successful_symbols <- c(successful_symbols, symbol)
      
    }, error = function(e) {
      warning(paste("Failed to fetch data for", symbol, ":", e$message))
      failed_symbols <- c(failed_symbols, symbol)
    })
  }
  
  # Return results with metadata
  list(
    data = data_list,
    successful = successful_symbols,
    failed = failed_symbols,
    success_rate = length(successful_symbols) / length(symbols)
  )
}

#' Fetch benchmark data (S&P 500, Bitcoin, etc.)
#' @param benchmark_symbol Benchmark symbol (e.g., "^GSPC", "BTC-USD")
#' @param start_date Start date for data
#' @param end_date End date for data
#' @param initial_value Initial investment value for normalization
#' @return Data frame with benchmark data
fetch_benchmark_data <- function(benchmark_symbol, start_date, 
                                end_date = today() + 1, initial_value = 10000) {
  tryCatch({
    # Fetch benchmark data
    benchmark_data <- getSymbols(benchmark_symbol,
                                from = start_date,
                                to = end_date,
                                auto.assign = FALSE)
    
    # Process data
    processed_data <- benchmark_data %>%
      as.data.frame() %>%
      rownames_to_column("date") %>%
      as_tibble() %>%
      select(date, contains("Close")) %>%
      set_names("date", "price") %>%
      mutate(
        date = ymd(date),
        returns = price / lag(price) - 1,
        returns = if_else(row_number() == 1, 0, returns),
        cumulative_return = cumprod(1 + returns),
        value = initial_value * cumulative_return
      ) %>%
      select(date, price, returns, value)
    
    # Add benchmark name column
    benchmark_name <- switch(benchmark_symbol,
                           "^GSPC" = "sp500",
                           "BTC-USD" = "btc",
                           "^DJI" = "dow",
                           "^IXIC" = "nasdaq",
                           tolower(gsub("[^A-Za-z]", "", benchmark_symbol)))
    
    names(processed_data)[4] <- benchmark_name
    
    return(processed_data)
    
  }, error = function(e) {
    warning(paste("Failed to fetch benchmark data for", benchmark_symbol, ":", e$message))
    return(NULL)
  })
}

#' Combine stock data from multiple sources
#' @param data_list List of data frames with stock data
#' @return Combined data frame
combine_stock_data <- function(data_list) {
  if (length(data_list) == 0) {
    return(data.frame())
  }
  
  # Filter out NULL entries
  data_list <- data_list[!sapply(data_list, is.null)]
  
  if (length(data_list) == 0) {
    return(data.frame())
  }
  
  # Combine all data frames
  combined_data <- bind_rows(data_list)
  
  return(combined_data)
}

#' Calculate returns for stock data
#' @param stock_data Data frame with price data
#' @param group_by Column to group by (e.g., "symbol")
#' @return Data frame with returns added
calculate_returns <- function(stock_data, group_by = "symbol") {
  stock_data %>%
    arrange(!!sym(group_by), date) %>%
    group_by(!!sym(group_by)) %>%
    mutate(
      daily_return = price / lag(price) - 1,
      daily_return = if_else(row_number() == 1, 0, daily_return),
      cumulative_return = cumprod(1 + daily_return),
      log_return = log(price / lag(price)),
      log_return = if_else(row_number() == 1, 0, log_return)
    ) %>%
    ungroup()
}

#' Fetch market cap data for stocks (simplified version)
#' @param symbols Character vector of stock symbols
#' @return Data frame with market cap data
fetch_market_cap_data <- function(symbols) {
  # This is a simplified version - in production, you'd use a real API
  # For now, return mock data or equal weights
  
  market_caps <- data.frame(
    symbol = symbols,
    market_cap = runif(length(symbols), 1e9, 1e12),  # Mock data
    stringsAsFactors = FALSE
  )
  
  market_caps %>%
    mutate(
      weight = market_cap / sum(market_cap)
    )
}

#' Validate stock symbols
#' @param symbols Character vector of stock symbols to validate
#' @param check_date Date to check for data availability
#' @return List with validation results
validate_symbols <- function(symbols, check_date = Sys.Date() - 30) {
  validation_results <- list()
  
  for (symbol in symbols) {
    tryCatch({
      # Try to fetch recent data
      test_data <- getSymbols(symbol,
                             from = check_date,
                             to = Sys.Date(),
                             auto.assign = FALSE)
      
      # Check if we got any data
      if (nrow(test_data) > 0) {
        validation_results[[symbol]] <- list(
          valid = TRUE,
          message = "Valid",
          data_points = nrow(test_data),
          last_price = as.numeric(tail(Cl(test_data), 1))
        )
      } else {
        validation_results[[symbol]] <- list(
          valid = FALSE,
          message = "No data available",
          data_points = 0,
          last_price = NA
        )
      }
    }, error = function(e) {
      validation_results[[symbol]] <- list(
        valid = FALSE,
        message = paste("Error:", e$message),
        data_points = 0,
        last_price = NA
      )
    })
  }
  
  return(validation_results)
}

#' Clean and prepare data for analysis
#' @param data Raw data frame
#' @param remove_na Whether to remove NA values
#' @param fill_method Method to fill missing values ("forward", "backward", "interpolate")
#' @return Cleaned data frame
clean_data <- function(data, remove_na = TRUE, fill_method = "forward") {
  if (nrow(data) == 0) return(data)
  
  # Remove duplicates
  data <- data %>% distinct()
  
  # Handle missing values
  if (remove_na) {
    data <- data %>% filter(complete.cases(.))
  } else if (fill_method == "forward") {
    data <- data %>% tidyr::fill(everything(), .direction = "down")
  } else if (fill_method == "backward") {
    data <- data %>% tidyr::fill(everything(), .direction = "up")
  } else if (fill_method == "interpolate") {
    # Linear interpolation for numeric columns
    numeric_cols <- names(data)[sapply(data, is.numeric)]
    for (col in numeric_cols) {
      data[[col]] <- zoo::na.approx(data[[col]], na.rm = FALSE)
    }
  }
  
  return(data)
}

#' Get latest price for stocks
#' @param symbols Character vector of stock symbols
#' @return Data frame with latest prices
get_latest_prices <- function(symbols) {
  prices <- data.frame(
    symbol = character(),
    latest_price = numeric(),
    change = numeric(),
    change_pct = numeric(),
    timestamp = as.POSIXct(character()),
    stringsAsFactors = FALSE
  )
  
  for (symbol in symbols) {
    tryCatch({
      # Get last 2 days of data
      recent_data <- getSymbols(symbol,
                               from = Sys.Date() - 5,
                               to = Sys.Date() + 1,
                               auto.assign = FALSE)
      
      if (nrow(recent_data) >= 2) {
        close_prices <- Cl(recent_data)
        latest_price <- as.numeric(tail(close_prices, 1))
        prev_price <- as.numeric(tail(close_prices, 2)[1])
        
        prices <- rbind(prices, data.frame(
          symbol = symbol,
          latest_price = latest_price,
          change = latest_price - prev_price,
          change_pct = (latest_price / prev_price - 1) * 100,
          timestamp = Sys.time(),
          stringsAsFactors = FALSE
        ))
      }
    }, error = function(e) {
      warning(paste("Could not fetch latest price for", symbol))
    })
  }
  
  return(prices)
}