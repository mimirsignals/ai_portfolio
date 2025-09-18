# simple_excel_reader.R - Simple Excel Portfolio Reader

library(readxl)
library(dplyr)
library(lubridate)

# Simple function to read portfolio Excel and convert to app format
read_portfolio_excel <- function(excel_path = "portfolio.xlsx") {
  
  # Read Excel file
  raw_data <- read_excel(excel_path)
  
  # Clean column names and handle comma decimals
  clean_data <- raw_data %>%
    rename_all(tolower) %>%
    mutate(
      date = as.Date(date),
      symbol = toupper(trimws(symbol)),
      weight = as.numeric(gsub(",", ".", as.character(weight)))  # Handle European decimals
    ) %>%
    filter(!is.na(date), !is.na(symbol), weight > 0) %>%
    arrange(desc(date), symbol)  # Most recent first
  
  # Get unique dates (most recent first)
  portfolio_dates <- unique(clean_data$date)
  
  # Create portfolio list for each date
  portfolios <- list()
  
  for (i in seq_along(portfolio_dates)) {
    date <- portfolio_dates[i]
    
    # Get data for this date
    date_data <- clean_data %>% filter(date == !!date)
    
    # Normalize weights to sum to 1
    total_weight <- sum(date_data$weight)
    normalized_weights <- date_data$weight / total_weight
    
    # Create portfolio name
    portfolio_name <- if (i == 1) {
      paste("Current Portfolio", format(date, "%Y-%m-%d"))
    } else {
      paste("Portfolio", format(date, "%Y-%m-%d"))
    }
    
    # Create portfolio object in format expected by existing functions
    portfolios[[portfolio_name]] <- list(
      symbols = date_data$symbol,
      weights = normalized_weights,
      start_date = date,
      total_investment = 10000,  # Default investment amount
      created_at = date,
      modified_at = date
    )
  }
  
  return(portfolios)
}

# Simple portfolio calculator that works directly with Excel data
simple_portfolio_calculator <- function(portfolios, selected_portfolios, show_sp500 = FALSE, show_btc = FALSE) {
  
  if (length(portfolios) == 0 || length(selected_portfolios) == 0) {
    return(list(portfolios = list(), sp500 = NULL, bitcoin = NULL))
  }
  
  result <- list(portfolios = list(), sp500 = NULL, bitcoin = NULL)
  
  # Sort selected portfolios by date (oldest first) for chaining
  portfolio_dates <- sapply(selected_portfolios, function(name) {
    if (name %in% names(portfolios)) {
      portfolios[[name]]$start_date
    } else {
      Sys.Date()
    }
  })
  selected_sorted <- selected_portfolios[order(portfolio_dates)]
  
  # Track chained returns
  cumulative_chain_factor <- 1.0  # Start at 1.0 (0% return)
  last_portfolio_end_date <- NULL
  
  # Calculate performance for each selected portfolio in chronological order
  for (i in seq_along(selected_sorted)) {
    portfolio_name <- selected_sorted[i]
    
    if (portfolio_name %in% names(portfolios)) {
      portfolio <- portfolios[[portfolio_name]]
      
      # Use existing portfolio calculation function
      portfolio_data <- calculate_portfolio_performance(
        symbols = portfolio$symbols,
        weights = portfolio$weights,
        start_date = portfolio$start_date,
        total_investment = portfolio$total_investment,
        portfolio_name = portfolio_name
      )
      
      if (!is.null(portfolio_data) && !is.null(portfolio_data$portfolio_tbl)) {
        # Get the raw returns for this portfolio period
        dates <- portfolio_data$portfolio_tbl$date
        values <- portfolio_data$portfolio_tbl$investment
        raw_returns <- (values / portfolio$total_investment) - 1
        
        # If this is not the first portfolio, chain the returns
        if (i > 1 && !is.null(last_portfolio_end_date)) {
          # Find where previous portfolio ended and this one begins
          portfolio_start_value <- cumulative_chain_factor
          
          # Chain this portfolio's returns to the previous cumulative return
          chained_returns <- (raw_returns + 1) * portfolio_start_value - 1
          
          # Update the cumulative chain factor for the next portfolio
          if (length(chained_returns) > 0) {
            cumulative_chain_factor <- tail(chained_returns + 1, 1)
          }
        } else {
          # First portfolio - use raw returns
          chained_returns <- raw_returns
          if (length(chained_returns) > 0) {
            cumulative_chain_factor <- tail(chained_returns + 1, 1)
          }
        }
        
        # Store the end date for chaining
        last_portfolio_end_date <- max(dates)
        
        result$portfolios[[portfolio_name]] <- list(
          dates = dates,
          cumulative_returns = chained_returns,
          individual_stocks = portfolio_data$individual_stocks,
          original_returns = raw_returns  # Keep original for reference
        )
      }
    }
  }
  
  # Add benchmarks if requested and we have valid portfolios
  if ((show_sp500 || show_btc) && length(result$portfolios) > 0) {
    
    # Find earliest start date across all portfolios
    earliest_date <- min(portfolio_dates)
    
    if (show_sp500) {
      sp500_data <- fetch_benchmark_data("^GSPC", earliest_date, initial_value = 10000)
      if (!is.null(sp500_data) && nrow(sp500_data) > 0) {
        result$sp500 <- list(
          dates = sp500_data$date,
          cumulative_returns = (sp500_data$value / 10000) - 1
        )
      }
    }
    
    if (show_btc) {
      btc_data <- fetch_benchmark_data("BTC-USD", earliest_date, initial_value = 10000)
      if (!is.null(btc_data) && nrow(btc_data) > 0) {
        result$bitcoin <- list(
          dates = btc_data$date,
          cumulative_returns = (btc_data$value / 10000) - 1
        )
      }
    }
  }
  
  return(result)
}