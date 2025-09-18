# R/utils_portfolio.R


#' Main calculation function for selected portfolios and benchmarks
run_portfolio_calculations <- function(portfolios, selected_portfolios, show_sp500 = TRUE, show_btc = TRUE) {
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
      }
    }
  }

  fetch_benchmark <- function(symbol, start) {
    raw <- fetch_stock_data(symbol, start_date = start_date)$data[[symbol]]
    if (is.null(raw)) return(NULL)
    returns <- calculate_returns(raw, "symbol")
    list(dates = returns$date, cumulative_returns = returns$cumulative_return - 1)
  }

  list(
    portfolios = calculated_portfolios,
    sp500 = if (show_sp500) fetch_benchmark("^GSPC", earliest_date) else NULL,
    bitcoin = if (show_btc) fetch_benchmark("BTC-USD", earliest_date) else NULL
  )
}