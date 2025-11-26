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

#' Extract all unique symbols from portfolio definitions
#' @param portfolios List of portfolio definitions
#' @return Character vector of unique symbols (excluding CASH)
extract_all_symbols <- function(portfolios) {
  if (length(portfolios) == 0) return(character(0))

  all_symbols <- unique(unlist(lapply(portfolios, function(p) {
    if (!is.null(p$symbols)) as.character(p$symbols) else character(0)
  })))

  # Remove CASH and empty strings
  all_symbols <- all_symbols[!is.na(all_symbols) &
                              all_symbols != "" &
                              toupper(all_symbols) != "CASH"]
  unique(all_symbols)
}

#' Preload stock data for all symbols with progress tracking
#' @param symbols Character vector of stock symbols
#' @param start_date Start date for data fetch (default: "2025-05-20")
#' @return Invisible TRUE if successful
preload_stock_data <- function(symbols, start_date = "2025-05-20") {
  if (length(symbols) == 0) {
    message("No symbols to preload")
    return(invisible(TRUE))
  }

  symbols <- symbols[toupper(symbols) != "CASH"]
  start_date <- as.Date(start_date)
  end_date <- Sys.Date() + 1

  # Check which symbols are already cached
  if (isTRUE(getOption("portfolio_cache_enabled", FALSE))) {
    cached_symbols <- sapply(symbols, function(sym) {
      cache_key <- paste(sym, start_date, end_date, sep = "|")
      exists(cache_key, envir = .stock_data_cache, inherits = FALSE)
    })
    symbols_to_fetch <- symbols[!cached_symbols]

    if (length(symbols_to_fetch) < length(symbols)) {
      message(sprintf("Skipping %d already-cached symbols", sum(cached_symbols)))
    }
  } else {
    symbols_to_fetch <- symbols
  }

  if (length(symbols_to_fetch) == 0) {
    message("All symbols already cached")
    return(invisible(TRUE))
  }

  message(sprintf("Preloading %d symbols from %s onwards...",
                  length(symbols_to_fetch), start_date))

  # Fetch sequentially with progress
  results <- lapply(seq_along(symbols_to_fetch), function(i) {
    sym <- symbols_to_fetch[i]

    # Update progress if we're in a withProgress context
    tryCatch({
      incProgress(1/length(symbols_to_fetch),
                  detail = sprintf("Fetching %s (%d/%d)", sym, i, length(symbols_to_fetch)))
    }, error = function(e) {
      # Silently ignore if not in a progress context
    })

    tryCatch({
      fetch_stock_data(sym, start_date)
      TRUE
    }, error = function(e) {
      warning(sprintf("Failed to preload %s: %s", sym, e$message))
      FALSE
    })
  })

  success_count <- sum(unlist(results))
  message(sprintf("Preloaded %d/%d symbols successfully",
                  success_count, length(symbols_to_fetch)))

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

    # Get the earliest date from the stock_data to use as the baseline
    earliest_date <- min(stock_data$date, na.rm = TRUE)

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
        # Use opening price on first date as entry point (rebalancing happens at market open)
        # Then track performance using adjusted closing prices
        initial_price <- symbol_data$open_price[1]
        first_date <- symbol_data$date[1]

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

        # If the first trading date for this symbol is after the earliest date,
        # prepend a row at the earliest date with the initial investment value
        if (first_date > earliest_date) {
          initial_row <- data.frame(
            date = earliest_date,
            symbol = current_symbol,
            investment = investment,
            daily_return = 0,
            stringsAsFactors = FALSE
          )
          symbol_performance <- rbind(initial_row, symbol_performance)
        }

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
      arrange(date) %>%
      ungroup()

    # Ensure we have valid portfolio data
    portfolio_tbl <- portfolio_tbl %>%
      mutate(
        investment = as.numeric(investment),
        daily_return = as.numeric(daily_return)
      ) %>%
      filter(!is.na(investment), is.finite(investment), investment > 0)

    # CRITICAL FIX: Ensure the first row matches the exact total_investment
    # This ensures cumulative returns always start at exactly 0%
    # Must be done AFTER type conversion and filtering to ensure it persists
    if (nrow(portfolio_tbl) > 0) {
      first_date <- portfolio_tbl$date[1]
      first_investment_before <- portfolio_tbl$investment[1]

      portfolio_tbl[1, "investment"] <- as.numeric(total_investment)
      portfolio_tbl[1, "daily_return"] <- 0

      # Debug: Detect large discrepancies (potential data issues)
      discrepancy_pct <- abs((first_investment_before - total_investment) / total_investment) * 100
      if (discrepancy_pct > 5) {
        message(sprintf("WARNING: Large discrepancy in first day calculation for %s: expected %.2f, calculated %.2f (%.1f%% diff)",
                        first_date, total_investment, first_investment_before, discrepancy_pct))
      }
    }
    
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

  cash_idx <- toupper(symbols) == "CASH"
  cash_weight <- sum(weights[cash_idx], na.rm = TRUE)
  cash_weight <- min(max(cash_weight, 0), 1)

  non_cash_symbols <- symbols[!cash_idx]
  non_cash_weights <- weights[!cash_idx]
  non_cash_weight_sum <- sum(non_cash_weights)

  cash_value <- as.numeric(total_investment * cash_weight)
  investable_total <- as.numeric(total_investment - cash_value)
  if (!is.finite(investable_total) || investable_total < 0) {
    investable_total <- 0
  }

  investable_fraction <- if (total_investment > 0) investable_total / total_investment else 0

  tryCatch({
    if (length(non_cash_symbols) > 0 && investable_total > 0 && non_cash_weight_sum > 0) {
      adjusted_weights <- non_cash_weights / non_cash_weight_sum

      stock_data <- fetch_stock_data(non_cash_symbols, start_date)
      if (is.null(stock_data) || nrow(stock_data) == 0) {
        warning(paste("No stock data available for portfolio:", portfolio_name))
        return(NULL)
      }

      portfolio_performance <- calculate_weighted_portfolio(
        stock_data,
        non_cash_symbols,
        adjusted_weights,
        investable_total
      )

      if (is.null(portfolio_performance)) {
        warning(paste("Portfolio calculation failed for portfolio:", portfolio_name))
        return(NULL)
      }

      portfolio_tbl <- portfolio_performance$portfolio_tbl %>%
        dplyr::mutate(
          investment = investment + cash_value,
          daily_return = daily_return * investable_fraction
        )

      individual_stocks <- portfolio_performance$individual_stocks
      if (cash_value > 0) {
        cash_rows <- tibble::tibble(
          date = portfolio_tbl$date,
          symbol = "CASH",
          investment = cash_value,
          daily_return = 0
        )
        individual_stocks <- dplyr::bind_rows(individual_stocks, cash_rows) %>%
          dplyr::arrange(date, symbol)
      }

    } else if (cash_value > 0) {
      base_date <- as.Date(start_date)
      portfolio_tbl <- tibble::tibble(
        date = base_date,
        investment = total_investment,
        daily_return = 0
      )
      individual_stocks <- tibble::tibble(
        date = base_date,
        symbol = "CASH",
        investment = total_investment,
        daily_return = 0
      )

    } else {
      warning(paste("No investable assets or cash for portfolio:", portfolio_name))
      return(NULL)
    }

    return(list(
      portfolio_tbl = portfolio_tbl,
      individual_stocks = individual_stocks
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

  use_cache <- isTRUE(getOption("portfolio_cache_enabled", FALSE))

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
        open_price = as.numeric(quantmod::Op(xts_data)),
        adjusted_price = as.numeric(quantmod::Ad(xts_data)),
        stringsAsFactors = FALSE
      )

      stock_df[is.finite(stock_df$adjusted_price) & stock_df$adjusted_price > 0 &
               is.finite(stock_df$open_price) & stock_df$open_price > 0 &
               !is.na(stock_df$date), , drop = FALSE]
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

    if (is.null(csv_df) || !("Adj.Close" %in% names(csv_df)) || !("Open" %in% names(csv_df))) {
      return(NULL)
    }

    csv_df <- csv_df[!is.na(csv_df$Adj.Close) & csv_df$Adj.Close > 0 &
                     !is.na(csv_df$Open) & csv_df$Open > 0 &
                     !is.na(csv_df$Date), , drop = FALSE]
    if (nrow(csv_df) == 0) {
      return(NULL)
    }

    data.frame(
      date = as.Date(csv_df$Date),
      symbol = as.character(symbol),
      open_price = as.numeric(csv_df$Open),
      adjusted_price = as.numeric(csv_df$Adj.Close),
      stringsAsFactors = FALSE
    )
  }

  get_symbol_data <- function(symbol) {
    cache_key <- paste(symbol, start_date, end_date, sep = "|")
    if (use_cache && exists(cache_key, envir = .stock_data_cache, inherits = FALSE)) {
      return(get(cache_key, envir = .stock_data_cache, inherits = FALSE))
    }

    stock_df <- fetch_quantmod(symbol)
    if (is.null(stock_df) || nrow(stock_df) == 0) {
      stock_df <- fetch_yahoo_csv(symbol)
    }

    if (!is.null(stock_df) && nrow(stock_df) > 0) {
      if (use_cache) {
        assign(cache_key, stock_df, envir = .stock_data_cache)
      }
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

  combined_data <- dplyr::bind_rows(stock_data_list[valid_entries]) %>%
    dplyr::mutate(
      date = as.Date(date),
      symbol = as.character(symbol),
      open_price = as.numeric(open_price),
      adjusted_price = as.numeric(adjusted_price),
      weekday = lubridate::wday(date)  # 1 = Sunday, 7 = Saturday
    )

  # Check for weekend data before filtering
  weekend_data <- combined_data %>%
    dplyr::filter(weekday == 1 | weekday == 7)

  if (nrow(weekend_data) > 0) {
    weekend_dates <- unique(weekend_data$date)
    message(sprintf("WARNING: Found %d weekend data points, filtering them out: %s",
                    nrow(weekend_data),
                    paste(format(head(weekend_dates, 5), "%Y-%m-%d"), collapse = ", ")))
  }

  all_stock_data <- combined_data %>%
    dplyr::filter(
      !is.na(date),
      !is.na(symbol),
      symbol != "",
      !is.na(open_price),
      is.finite(open_price),
      open_price > 0,
      !is.na(adjusted_price),
      is.finite(adjusted_price),
      adjusted_price > 0,
      date >= start_date,
      date <= end_date,
      weekday >= 2,  # Monday = 2
      weekday <= 6   # Friday = 6
    ) %>%
    dplyr::select(-weekday) %>%  # Remove the helper column
    dplyr::arrange(date, symbol) %>%
    dplyr::distinct(date, symbol, .keep_all = TRUE)

  if (nrow(all_stock_data) == 0) {
    warning("No valid stock data retrieved")
    return(NULL)
  }

  # Debug: Check for data completeness in recent dates (weekdays only)
  max_date <- max(all_stock_data$date, na.rm = TRUE)
  if (max_date >= as.Date("2025-10-04")) {
    recent_data <- all_stock_data %>%
      dplyr::filter(date >= as.Date("2025-10-04"), date <= as.Date("2025-10-07"))

    if (nrow(recent_data) > 0) {
      message(sprintf("DEBUG fetch_stock_data: Oct 4-7 data (weekdays only) found for %d symbol-date combinations", nrow(recent_data)))
      for (sym in unique(recent_data$symbol)) {
        sym_dates <- recent_data$date[recent_data$symbol == sym]
        message(sprintf("  - %s: dates %s (should skip Oct 4-5 weekends)", sym, paste(format(sym_dates, "%Y-%m-%d"), collapse = ", ")))
      }
    } else {
      message("DEBUG fetch_stock_data: NO Oct 4-7 data found despite max_date >= 2025-10-04")
    }
  }

  return(all_stock_data)
}
