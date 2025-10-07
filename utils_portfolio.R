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

  portfolio_names <- vapply(
    portfolios,
    function(p) if (!is.null(p$portfolio_name)) as.character(p$portfolio_name) else "",
    character(1)
  )

  results <- list()
  transaction_records <- list()
  earliest_start <- as.Date(Sys.Date())

  for (group_name in unique(portfolio_names[available])) {
    group_keys <- names(portfolios)[portfolio_names == group_name]
    if (length(group_keys) == 0) next

    group_keys <- group_keys[order(vapply(group_keys, function(key) as.Date(portfolios[[key]]$start_date), as.Date(Sys.Date())))]
    selected_in_group <- intersect(available, group_keys)
    if (length(selected_in_group) == 0) next

    cutoff_index <- max(match(selected_in_group, group_keys))
    chain_keys <- group_keys[seq_len(cutoff_index)]

    group_results <- list()
    previous_key <- NULL

    for (key in chain_keys) {
      def <- portfolios[[key]]
      start_date <- as.Date(def$start_date)
      earliest_start <- min(earliest_start, start_date, na.rm = TRUE)
      portfolio_label <- if (!is.null(def$portfolio_name)) def$portfolio_name else key

      if (is.null(previous_key)) {
        base_result <- run_portfolio_calculations(def$symbols, def$weights, start_date, def$total_investment, key)
        if (is.null(base_result)) {
          warning(paste("Could not calculate base portfolio for", key))
          next
        }

        portfolio_tbl <- base_result$portfolio_tbl %>%
          dplyr::mutate(
            date = as.Date(date),
            investment = as.numeric(investment),
            daily_return = as.numeric(daily_return)
          ) %>%
          dplyr::arrange(date)

        if (nrow(portfolio_tbl) == 0) {
          warning(paste("No portfolio data returned for", key))
          next
        }

        original_investment <- as.numeric(def$total_investment)
        cumulative_returns <- (portfolio_tbl$investment / original_investment) - 1

        individual_stocks <- base_result$individual_stocks %>%
          dplyr::mutate(
            date = as.Date(date),
            investment = as.numeric(investment),
            daily_return = as.numeric(daily_return)
          )

        # Debug: Check Oct 5-6 for base portfolio
        if (any(portfolio_tbl$date >= as.Date("2025-10-05"))) {
          oct_idx <- portfolio_tbl$date >= as.Date("2025-10-05") & portfolio_tbl$date <= as.Date("2025-10-06")
          if (any(oct_idx)) {
            message(sprintf("DEBUG BASE PORTFOLIO [%s]:", key))
            message(sprintf("  - original_investment: %.2f", original_investment))
            oct_data <- portfolio_tbl[oct_idx, ]
            for (i in seq_len(nrow(oct_data))) {
              message(sprintf("  - %s: investment=%.2f, cum_return=%.4f%%",
                              oct_data$date[i], oct_data$investment[i],
                              cumulative_returns[which(portfolio_tbl$date == oct_data$date[i])] * 100))
            }
          }
        }

        group_results[[key]] <- list(
          dates = portfolio_tbl$date,
          cumulative_returns = cumulative_returns,
          individual_stocks = individual_stocks,
          portfolio_tbl = portfolio_tbl,
          original_investment = original_investment
        )

        target_values <- compute_target_values(def$symbols, def$weights, def$total_investment)

        transaction_records[[key]] <- build_transaction_log(
          portfolio_name = portfolio_label,
          portfolio_key = key,
          rebalance_date = start_date,
          price_date = start_date,
          prev_holdings = tibble::tibble(symbol = character(), investment = numeric(), date = as.Date(character())),
          target_values = target_values,
          total_value = as.numeric(def$total_investment),
          price_lookup = fetch_prices_on_or_before(names(target_values), start_date)
        )

        previous_key <- key
        next
      }

      previous_entry <- group_results[[previous_key]]
      rebalance <- run_rebalance_segment(
        current_def = def,
        previous_entry = previous_entry,
        portfolio_key = key
      )

      if (is.null(rebalance)) {
        warning(paste("Unable to calculate inherited performance for", key))
        next
      }

      group_results[[key]] <- list(
        dates = rebalance$dates,
        cumulative_returns = rebalance$cumulative_returns,
        individual_stocks = rebalance$individual_stocks,
        portfolio_tbl = rebalance$portfolio_tbl,
        original_investment = rebalance$original_investment
      )

      transaction_records[[key]] <- rebalance$transactions
      previous_key <- key
    }

    for (key in intersect(names(group_results), available)) {
      results[[key]] <- group_results[[key]]
    }
  }

  if (length(results) == 0) return(NULL)

  fetch_benchmark <- function(symbol, start) {
    raw_data <- fetch_stock_data(symbol, start_date = start)
    if (is.null(raw_data) || nrow(raw_data) == 0) return(NULL)

    initial_price <- raw_data$adjusted_price[1]
    raw_data <- raw_data %>%
      dplyr::mutate(cumulative_return = (adjusted_price / initial_price) - 1)

    list(dates = raw_data$date, cumulative_returns = raw_data$cumulative_return)
  }

  earliest_start <- suppressWarnings(min(earliest_start, na.rm = TRUE))
  if (!is.finite(earliest_start)) {
    earliest_start <- Sys.Date() - 365
  }

  transactions_out <- lapply(names(results), function(key) {
    tx <- transaction_records[[key]]
    if (is.null(tx)) tibble::tibble()
    else tx
  })
  names(transactions_out) <- names(results)

  list(
    portfolios = results,
    transactions = transactions_out,
    sp500 = if (show_sp500) fetch_benchmark("^GSPC", earliest_start) else NULL,
    bitcoin = if (show_btc) fetch_benchmark("BTC-USD", earliest_start) else NULL
  )
}

compute_target_values <- function(symbols, weights, total_investment) {
  symbols <- as.character(symbols)
  weights <- as.numeric(weights)
  if (length(symbols) == 0 || length(weights) == 0) {
    return(setNames(numeric(0), character(0)))
  }

  if (length(weights) != length(symbols)) {
    weights <- weights[seq_along(symbols)]
  }

  weights[is.na(weights)] <- 0
  total_weight <- sum(weights)
  if (total_weight == 0) {
    return(setNames(rep(0, length(symbols)), symbols))
  }

  weights <- weights / total_weight
  names(weights) <- symbols

  total_val <- as.numeric(total_investment)
  if (!is.finite(total_val) || total_val <= 0) {
    return(setNames(rep(0, length(symbols)), symbols))
  }

  total_val * weights
}

run_rebalance_segment <- function(current_def, previous_entry, portfolio_key) {
  rebalance_date <- as.Date(current_def$start_date)
  snapshot <- get_latest_portfolio_snapshot(previous_entry$portfolio_tbl, rebalance_date)
  if (is.null(snapshot)) return(NULL)

  total_investment <- as.numeric(snapshot$investment)
  if (!is.finite(total_investment) || total_investment <= 0) return(NULL)

  price_reference_date <- as.Date(snapshot$date)
  portfolio_label <- if (!is.null(current_def$portfolio_name)) current_def$portfolio_name else portfolio_key

  previous_holdings <- extract_holdings_at_date(previous_entry$individual_stocks, price_reference_date)

  target_values <- compute_target_values(current_def$symbols, current_def$weights, total_investment)
  if (length(target_values) == 0) return(NULL)

  price_lookup <- fetch_prices_on_or_before(
    symbols = unique(c(names(target_values), previous_holdings$symbol)),
    cutoff_date = price_reference_date
  )

  transactions <- build_transaction_log(
    portfolio_name = portfolio_label,
    portfolio_key = portfolio_key,
    rebalance_date = rebalance_date,
    price_date = price_reference_date,
    prev_holdings = previous_holdings,
    target_values = target_values,
    total_value = total_investment,
    price_lookup = price_lookup
  )

  segment <- build_rebalanced_segment(
    symbols = names(target_values),
    weights = target_values / total_investment,
    total_investment = total_investment,
    rebalance_date = rebalance_date
  )

  if (is.null(segment$portfolio_tbl) || nrow(segment$portfolio_tbl) == 0) return(NULL)

  segment$portfolio_tbl <- segment$portfolio_tbl %>%
    dplyr::mutate(
      date = as.Date(date),
      investment = as.numeric(investment),
      daily_return = as.numeric(daily_return)
    ) %>%
    dplyr::arrange(date)

  # Get the original investment baseline to maintain cumulative returns across rebalances
  original_investment <- get_original_investment_baseline(previous_entry, current_def)
  cumulative_returns <- (segment$portfolio_tbl$investment / original_investment) - 1

  # Debug: Check for anomalies in recent dates
  if (any(segment$portfolio_tbl$date >= as.Date("2025-10-05"))) {
    recent_idx <- segment$portfolio_tbl$date >= as.Date("2025-10-05") & segment$portfolio_tbl$date <= as.Date("2025-10-06")
    if (any(recent_idx)) {
      message(sprintf("DEBUG run_rebalance_segment [%s]:", portfolio_key))
      message(sprintf("  - original_investment: %.2f", original_investment))
      message(sprintf("  - total_investment (rebalance): %.2f", total_investment))
      message(sprintf("  - rebalance_date: %s", rebalance_date))

      recent_data <- segment$portfolio_tbl[recent_idx, ]
      for (i in seq_len(nrow(recent_data))) {
        message(sprintf("  - Oct %s: investment=%.2f, cum_return=%.4f",
                        format(recent_data$date[i], "%d"),
                        recent_data$investment[i],
                        cumulative_returns[which(segment$portfolio_tbl$date == recent_data$date[i])]))
      }
    }
  }

  individual_stocks <- segment$individual_stocks %>%
    dplyr::mutate(
      date = as.Date(date),
      investment = as.numeric(investment),
      daily_return = as.numeric(daily_return)
    )

  list(
    portfolio_tbl = segment$portfolio_tbl,
    individual_stocks = individual_stocks,
    dates = segment$portfolio_tbl$date,
    cumulative_returns = cumulative_returns,
    transactions = transactions,
    original_investment = original_investment
  )
}

get_latest_portfolio_snapshot <- function(portfolio_tbl, cutoff_date) {
  if (is.null(portfolio_tbl) || nrow(portfolio_tbl) == 0) return(NULL)
  cutoff_date <- as.Date(cutoff_date)

  snapshot <- portfolio_tbl %>%
    dplyr::mutate(date = as.Date(date)) %>%
    dplyr::filter(date <= cutoff_date) %>%
    dplyr::arrange(date) %>%
    dplyr::slice_tail(n = 1)

  if (nrow(snapshot) == 0) return(NULL)
  snapshot$investment <- as.numeric(snapshot$investment)
  snapshot
}

get_original_investment_baseline <- function(previous_entry, current_def) {
  # For the first portfolio in a chain, use the initial investment amount
  if (is.null(previous_entry)) {
    return(as.numeric(current_def$total_investment))
  }

  # For rebalanced portfolios, use the original investment from the chain
  # This ensures cumulative returns are calculated consistently across all rebalances
  if (!is.null(previous_entry$original_investment)) {
    return(previous_entry$original_investment)
  }

  # Fallback to current definition (for backward compatibility)
  return(as.numeric(current_def$total_investment))
}

extract_holdings_at_date <- function(individual_stocks, cutoff_date) {
  if (is.null(individual_stocks) || nrow(individual_stocks) == 0) {
    return(tibble::tibble(symbol = character(), investment = numeric(), date = as.Date(character())))
  }

  cutoff_date <- as.Date(cutoff_date)

  individual_stocks %>%
    dplyr::mutate(date = as.Date(date), investment = as.numeric(investment)) %>%
    dplyr::filter(date <= cutoff_date) %>%
    dplyr::arrange(date) %>%
    dplyr::group_by(symbol) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::ungroup()
}

fetch_prices_on_or_before <- function(symbols, cutoff_date) {
  symbols <- unique(as.character(symbols))
  if (length(symbols) == 0) return(setNames(numeric(0), character(0)))

  cutoff_date <- as.Date(cutoff_date)
  start_fetch <- cutoff_date - 30

  price_data <- fetch_stock_data(symbols, start_fetch)
  if (is.null(price_data) || nrow(price_data) == 0) {
    return(setNames(rep(NA_real_, length(symbols)), symbols))
  }

  price_tbl <- price_data %>%
    dplyr::mutate(date = as.Date(date)) %>%
    dplyr::filter(date <= cutoff_date) %>%
    dplyr::arrange(date) %>%
    dplyr::group_by(symbol) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::ungroup()

  prices <- price_tbl$adjusted_price
  names(prices) <- price_tbl$symbol

  missing_symbols <- setdiff(symbols, names(prices))
  if (length(missing_symbols) > 0) {
    prices <- c(prices, setNames(rep(NA_real_, length(missing_symbols)), missing_symbols))
  }

  prices[symbols]
}

build_transaction_log <- function(portfolio_name, portfolio_key, rebalance_date, price_date, prev_holdings, target_values, total_value, price_lookup) {
  prev_df <- prev_holdings %>%
    dplyr::transmute(
      symbol = symbol,
      previous_value = as.numeric(investment),
      previous_weight = NA_real_,
      previous_date = as.Date(date)
    )

  total_value <- as.numeric(total_value)
  if (!is.finite(total_value) || total_value <= 0) {
    total_value <- sum(prev_df$previous_value, na.rm = TRUE)
  }

  union_symbols <- union(names(target_values), prev_df$symbol)

  log_df <- tibble::tibble(symbol = union_symbols) %>%
    dplyr::left_join(prev_df, by = "symbol") %>%
    dplyr::mutate(
      previous_value = dplyr::coalesce(previous_value, 0),
      previous_weight = if (!is.na(total_value) && total_value > 0) previous_value / total_value else NA_real_,
      target_value = dplyr::coalesce(target_values[symbol], 0),
      target_weight = if (!is.na(total_value) && total_value > 0) target_value / total_value else NA_real_,
      delta_value = target_value - previous_value,
      action = dplyr::case_when(
        delta_value > 1e-6 ~ "buy",
        delta_value < -1e-6 ~ "sell",
        TRUE ~ "hold"
      ),
      trade_value = abs(delta_value),
      price = price_lookup[symbol],
      shares_change = dplyr::if_else(!is.na(price) & price > 0, delta_value / price, NA_real_)
    ) %>%
    dplyr::filter(action != "hold") %>%
    dplyr::arrange(match(action, c("sell", "buy")), dplyr::desc(trade_value), symbol)

  if (nrow(log_df) == 0) {
    return(tibble::tibble(
      rebalance_date = as.Date(rebalance_date),
      price_date = as.Date(price_date),
      portfolio = portfolio_name,
      version = portfolio_key,
      symbol = character(),
      action = character(),
      trade_value = numeric(),
      shares_change = numeric(),
      price = numeric(),
      previous_value = numeric(),
      target_value = numeric(),
      previous_weight = numeric(),
      target_weight = numeric()
    ))
  }

  log_df %>%
    dplyr::mutate(
      rebalance_date = as.Date(rebalance_date),
      price_date = as.Date(price_date),
      portfolio = portfolio_name,
      version = portfolio_key
    ) %>%
    dplyr::select(
      rebalance_date,
      price_date,
      portfolio,
      version,
      symbol,
      action,
      trade_value,
      shares_change,
      price,
      previous_value,
      target_value,
      previous_weight,
      target_weight
    )
}

build_rebalanced_segment <- function(symbols, weights, total_investment, rebalance_date) {
  symbols <- as.character(symbols)
  weights <- as.numeric(weights)
  if (length(symbols) == 0 || length(weights) == 0 || sum(weights, na.rm = TRUE) == 0) {
    return(list(
      portfolio_tbl = tibble::tibble(),
      individual_stocks = tibble::tibble()
    ))
  }

  weights <- weights / sum(weights, na.rm = TRUE)
  names(weights) <- symbols

  total_investment <- as.numeric(total_investment)
  if (!is.finite(total_investment)) total_investment <- 0

  target_values <- total_investment * weights

  stock_data <- fetch_stock_data(symbols, rebalance_date)
  future_data <- NULL
  if (!is.null(stock_data) && nrow(stock_data) > 0) {
    future_data <- stock_data %>%
      dplyr::mutate(date = as.Date(date)) %>%
      dplyr::filter(date >= rebalance_date)
  }

  performance <- NULL
  if (!is.null(future_data) && nrow(future_data) > 0) {
    # Check if we have data for all symbols on or near the rebalance date
    earliest_by_symbol <- future_data %>%
      dplyr::group_by(symbol) %>%
      dplyr::summarise(first_date = min(date), .groups = 'drop')

    missing_rebalance_date <- !all(earliest_by_symbol$first_date <= rebalance_date)

    if (missing_rebalance_date) {
      max_delay <- max(earliest_by_symbol$first_date) - as.Date(rebalance_date)
      message(sprintf("DEBUG build_rebalanced_segment: rebalance_date=%s, max_delay=%d days",
                      rebalance_date, as.numeric(max_delay)))
      for (i in seq_len(nrow(earliest_by_symbol))) {
        message(sprintf("  - %s: first_date=%s", earliest_by_symbol$symbol[i], earliest_by_symbol$first_date[i]))
      }
    }

    performance <- calculate_weighted_portfolio(future_data, symbols, weights, total_investment)
  }

  # Create the rebalance baseline row with the inherited value
  portfolio_row <- tibble::tibble(
    date = as.Date(rebalance_date),
    investment = total_investment,
    daily_return = 0
  )

  if (!is.null(performance) && !is.null(performance$portfolio_tbl) && nrow(performance$portfolio_tbl) > 0) {
    # Always prepend the rebalance row, then remove duplicates
    # This ensures the rebalance value (total_investment) takes precedence
    portfolio_tbl <- dplyr::bind_rows(portfolio_row, performance$portfolio_tbl) %>%
      dplyr::mutate(
        date = as.Date(date),
        investment = as.numeric(investment),
        daily_return = as.numeric(daily_return)
      ) %>%
      dplyr::arrange(date) %>%
      dplyr::distinct(date, .keep_all = TRUE)
  } else {
    # No performance data - just use the rebalance row
    portfolio_tbl <- portfolio_row
  }

  # Handle individual stocks similarly
  individual_rows <- tibble::tibble(
    date = rep(as.Date(rebalance_date), length(symbols)),
    symbol = symbols,
    investment = target_values,
    daily_return = 0
  )

  if (!is.null(performance) && !is.null(performance$individual_stocks) && nrow(performance$individual_stocks) > 0) {
    individual_tbl <- dplyr::bind_rows(individual_rows, performance$individual_stocks) %>%
      dplyr::mutate(
        date = as.Date(date),
        investment = as.numeric(investment),
        daily_return = as.numeric(daily_return)
      ) %>%
      dplyr::arrange(date, symbol) %>%
      dplyr::distinct(date, symbol, .keep_all = TRUE)
  } else {
    individual_tbl <- individual_rows
  }

  list(
    portfolio_tbl = portfolio_tbl,
    individual_stocks = individual_tbl
  )
}
