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
      mutate(cumulative_return = (adjusted_price / initial_price) - 1)
      
    list(dates = raw_data$date, cumulative_returns = raw_data$cumulative_return)
  }

  list(
    portfolios = calculated_portfolios,
    sp500 = if (show_sp500) fetch_benchmark("^GSPC", earliest_date) else NULL,
    bitcoin = if (show_btc) fetch_benchmark("BTC-USD", earliest_date) else NULL
  )
}