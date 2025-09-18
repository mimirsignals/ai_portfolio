# Replace this function in your existing simple_excel_reader.R
simple_portfolio_calculator <- function(portfolios, selected_portfolios, show_sp500 = FALSE, show_btc = FALSE) {
  
  if (length(portfolios) == 0 || length(selected_portfolios) == 0) {
    return(list(portfolios = list(), sp500 = NULL, bitcoin = NULL))
  }
  
  result <- list(portfolios = list(), sp500 = NULL, bitcoin = NULL)
  
  # Use your existing portfolio calculation functions
  for (portfolio_name in selected_portfolios) {
    if (portfolio_name %in% names(portfolios)) {
      portfolio <- portfolios[[portfolio_name]]
      
      # Call your existing calculate_portfolio_performance function
      portfolio_data <- calculate_portfolio_performance(
        symbols = portfolio$symbols,
        weights = portfolio$weights,
        start_date = portfolio$start_date,
        total_investment = portfolio$total_investment,
        portfolio_name = portfolio_name
      )
      
      if (!is.null(portfolio_data)) {
        # Convert to format expected by plots
        portfolio_tbl <- portfolio_data$portfolio_tbl
        if (nrow(portfolio_tbl) > 0) {
          cumulative_returns <- (portfolio_tbl$investment / portfolio$total_investment) - 1
          
          result$portfolios[[portfolio_name]] <- list(
            dates = portfolio_tbl$date,
            cumulative_returns = as.numeric(cumulative_returns),
            individual_stocks = portfolio_data$individual_stocks
          )
        }
      }
    }
  }
  
  return(result)
}