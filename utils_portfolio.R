# R/utils_portfolio.R - Portfolio Calculation Utilities

#' Portfolio Calculator Function
#' @return Function that calculates portfolio performance
portfolioCalculator <- function() {
  
  function(portfolios, selected_portfolios, show_sp500 = FALSE, show_btc = FALSE) {
    
    if (length(portfolios) == 0 || length(selected_portfolios) == 0) {
      return(NULL)
    }
    
    all_portfolio_data <- list()
    
    # Process each selected portfolio
    for (portfolio_name in selected_portfolios) {
      if (portfolio_name %in% names(portfolios)) {
        portfolio_info <- portfolios[[portfolio_name]]
        
        # Calculate portfolio performance
        portfolio_data <- calculate_portfolio_performance(
          symbols = portfolio_info$symbols,
          weights = portfolio_info$weights,
          start_date = portfolio_info$start_date,
          total_investment = portfolio_info$total_investment,
          portfolio_name = portfolio_name
        )
        
        if (!is.null(portfolio_data)) {
          all_portfolio_data[[portfolio_name]] <- portfolio_data
        }
      }
    }
    
    # Fetch benchmark data if needed
    benchmark_data <- list()
    if (show_sp500 || show_btc) {
      # Find earliest start date
      earliest_date <- min(sapply(selected_portfolios, function(name) {
        if (name %in% names(portfolios)) {
          portfolios[[name]]$start_date
        } else {
          Sys.Date()
        }
      }))
      
      if (show_sp500) {
        benchmark_data$sp500 <- fetch_benchmark_data("^GSPC", earliest_date)
      }
      
      if (show_btc) {
        benchmark_data$btc <- fetch_benchmark_data("BTC-USD", earliest_date)
      }
    }
    
    list(
      portfolios = all_portfolio_data,
      benchmarks = benchmark_data
    )
  }
}

#' Calculate portfolio performance
#' @param symbols Stock symbols
#' @param weights Portfolio weights
#' @param start_date Start date
#' @param total_investment Total investment amount
#' @param portfolio_name Name of the portfolio
#' @return List with portfolio performance data
calculate_portfolio_performance <- function(symbols, weights, start_date, 
                                          total_investment, portfolio_name) {
  
  # Fetch stock data
  stock_data_result <- fetch_stock_data(symbols, start_date)
  
  if (length(stock_data_result$successful) == 0) {
    warning(paste("No data available for portfolio:", portfolio_name))
    return(NULL)
  }
  
  # Combine stock data
  combined_data <- combine_stock_data(stock_data_result$data)
  
  # Add returns
  stock_returns <- calculate_returns(combined_data, "symbol")
  
  # Adjust weights for successful symbols only
  successful_indices <- which(symbols %in% stock_data_result$successful)
  adjusted_weights <- weights[successful_indices]
  adjusted_weights <- adjusted_weights / sum(adjusted_weights)  # Renormalize
  
  # Create weight mapping
  weight_df <- data.frame(
    symbol = stock_data_result$successful,
    weight = adjusted_weights,
    stringsAsFactors = FALSE
  )
  
  # Calculate individual stock investments
  individual_stocks <- stock_returns %>%
    left_join(weight_df, by = "symbol") %>%
    group_by(symbol) %>%
    mutate(
      investment = total_investment * weight * cumulative_return
    ) %>%
    ungroup()
  
  # Calculate portfolio total
  portfolio_tbl <- individual_stocks %>%
    group_by(date) %>%
    summarise(
      investment = sum(investment),
      .groups = 'drop'
    ) %>%
    mutate(portfolio_name = portfolio_name)
  
  # Return results
  list(
    portfolio_tbl = portfolio_tbl,
    individual_stocks = individual_stocks,
    successful_symbols = stock_data_result$successful,
    failed_symbols = stock_data_result$failed,
    total_investment = total_investment,
    weights = adjusted_weights
  )
}

#' Calculate portfolio metrics
#' @param portfolio_data Portfolio performance data
#' @return Data frame with portfolio metrics
calculate_portfolio_metrics <- function(portfolio_data) {
  
  if (is.null(portfolio_data) || nrow(portfolio_data$portfolio_tbl) < 2) {
    return(data.frame())
  }
  
  portfolio_tbl <- portfolio_data$portfolio_tbl
  total_investment <- portfolio_data$total_investment
  
  # Calculate returns
  returns <- portfolio_tbl %>%
    arrange(date) %>%
    mutate(
      daily_return = investment / lag(investment) - 1
    ) %>%
    filter(!is.na(daily_return))
  
  if (nrow(returns) == 0) {
    return(data.frame())
  }
  
  # Calculate metrics
  total_return <- (tail(portfolio_tbl$investment, 1) / total_investment - 1) * 100
  
  # Annualized return
  days <- as.numeric(difftime(max(portfolio_tbl$date), min(portfolio_tbl$date), units = "days"))
  years <- days / 365.25
  annualized_return <- ((tail(portfolio_tbl$investment, 1) / total_investment) ^ (1/years) - 1) * 100
  
  # Volatility
  daily_vol <- sd(returns$daily_return, na.rm = TRUE)
  annual_vol <- daily_vol * sqrt(252) * 100
  
  # Sharpe ratio (assuming 0% risk-free rate)
  sharpe <- mean(returns$daily_return, na.rm = TRUE) / daily_vol * sqrt(252)
  
  # Sortino ratio
  downside_returns <- returns$daily_return[returns$daily_return < 0]
  downside_vol <- if(length(downside_returns) > 0) {
    sd(downside_returns) * sqrt(252)
  } else {
    0
  }
  sortino <- if(downside_vol > 0) {
    mean(returns$daily_return, na.rm = TRUE) / sd(downside_returns) * sqrt(252)
  } else {
    NA
  }
  
  # Maximum drawdown
  cumulative <- cumprod(1 + returns$daily_return)
  running_max <- cummax(cumulative)
  drawdown <- (cumulative / running_max - 1)
  max_drawdown <- min(drawdown, na.rm = TRUE) * 100
  
  # Calmar ratio
  calmar <- if(max_drawdown != 0) {
    annualized_return / abs(max_drawdown)
  } else {
    NA
  }
  
  # Win rate
  win_rate <- sum(returns$daily_return > 0, na.rm = TRUE) / nrow(returns) * 100
  
  # Best and worst days
  best_day <- max(returns$daily_return, na.rm = TRUE) * 100
  worst_day <- min(returns$daily_return, na.rm = TRUE) * 100
  
  # Create metrics data frame
  data.frame(
    Total_Return = round(total_return, 2),
    Annualized_Return = round(annualized_return, 2),
    Volatility = round(annual_vol, 2),
    Sharpe_Ratio = round(sharpe, 3),
    Sortino_Ratio = round(sortino, 3),
    Max_Drawdown = round(max_drawdown, 2),
    Calmar_Ratio = round(calmar, 3),
    Win_Rate = round(win_rate, 1),
    Best_Day = round(best_day, 2),
    Worst_Day = round(worst_day, 2),
    stringsAsFactors = FALSE
  )
}

#' Optimize portfolio weights using mean-variance optimization
#' @param returns_matrix Matrix of returns
#' @param target_return Target return (optional)
#' @param constraints List of constraints
#' @return Optimized weights
optimize_portfolio_weights <- function(returns_matrix, target_return = NULL, 
                                      constraints = list()) {
  
  n_assets <- ncol(returns_matrix)
  
  # Default constraints
  if (is.null(constraints$min_weight)) constraints$min_weight <- 0
  if (is.null(constraints$max_weight)) constraints$max_weight <- 1
  
  # Calculate covariance matrix
  cov_matrix <- cov(returns_matrix)
  
  # Mean returns
  mean_returns <- colMeans(returns_matrix)
  
  # Use quadprog for optimization (simplified version)
  # In production, you'd use portfolio optimization packages
  
  # For now, return equal weights
  weights <- rep(1/n_assets, n_assets)
  
  return(weights)
}

#' Calculate portfolio correlation matrix
#' @param portfolio_data Portfolio performance data with individual stocks
#' @return Correlation matrix
calculate_correlation_matrix <- function(portfolio_data) {
  
  if (is.null(portfolio_data$individual_stocks)) {
    return(NULL)
  }
  
  # Pivot data to wide format
  returns_wide <- portfolio_data$individual_stocks %>%
    select(date, symbol, daily_return) %>%
    pivot_wider(names_from = symbol, values_from = daily_return)
  
  # Calculate correlation matrix
  cor_matrix <- cor(returns_wide[, -1], use = "complete.obs")
  
  return(cor_matrix)
}

#' Calculate portfolio beta
#' @param portfolio_returns Portfolio returns
#' @param market_returns Market returns (e.g., S&P 500)
#' @return Beta value
calculate_beta <- function(portfolio_returns, market_returns) {
  
  if (length(portfolio_returns) != length(market_returns)) {
    # Align dates
    combined <- merge(
      data.frame(date = seq_along(portfolio_returns), portfolio = portfolio_returns),
      data.frame(date = seq_along(market_returns), market = market_returns),
      by = "date"
    )
    portfolio_returns <- combined$portfolio
    market_returns <- combined$market
  }
  
  # Calculate beta
  cov_pm <- cov(portfolio_returns, market_returns, use = "complete.obs")
  var_m <- var(market_returns, na.rm = TRUE)
  
  beta <- cov_pm / var_m
  
  return(beta)
}

#' Rebalance portfolio to target weights
#' @param current_values Current values of holdings
#' @param target_weights Target weights
#' @param total_value Total portfolio value
#' @return Rebalancing transactions
calculate_rebalancing <- function(current_values, target_weights, total_value) {
  
  current_weights <- current_values / sum(current_values)
  target_values <- target_weights * total_value
  
  transactions <- data.frame(
    current_value = current_values,
    current_weight = current_weights,
    target_weight = target_weights,
    target_value = target_values,
    change = target_values - current_values,
    change_pct = (target_values / current_values - 1) * 100,
    stringsAsFactors = FALSE
  )
  
  return(transactions)
}