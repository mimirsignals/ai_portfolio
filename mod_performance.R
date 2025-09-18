# mod_performance.R

performanceUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = "Portfolio Selection & Comparison", status = "primary", solidHeader = TRUE, width = 12,
        uiOutput(ns("portfolio_selector_ui"))
      )
    ),
    fluidRow(
      box(
        title = "Portfolio Performance vs BTC & S&P 500", status = "primary", solidHeader = TRUE, width = 12,
        plotlyOutput(ns("performance_plot"), height = "500px")
      )
    ),
    fluidRow(uiOutput(ns("portfolio_value_boxes"))),
    fluidRow(
      box(
        title = "Performance Metrics Comparison", status = "info", solidHeader = TRUE, width = 12,
        DT::dataTableOutput(ns("metrics_table"))
      )
    )
  )
}

performanceServer <- function(id, portfolios_reactive, portfolio_calc) {
  moduleServer(id, function(input, output, session) {
    
    all_portfolio_names <- reactive(names(portfolios_reactive()))
    
    output$portfolio_selector_ui <- renderUI({
      ns <- session$ns
      p_names <- all_portfolio_names()
      if (length(p_names) == 0) return(p("No portfolios loaded."))
      
      selected_defaults <- if (length(p_names) > 1) unique(c(p_names[1], tail(p_names, 1))) else p_names
      
      checkboxGroupInput(
        ns("selected_portfolios"), "Select portfolios to compare:",
        choices = p_names, selected = selected_defaults, inline = TRUE
      )
    })
    
    selected_portfolios_reactive <- reactive(req(input$selected_portfolios))
    
    portfolio_data <- reactive({
      portfolio_calc(
        portfolios = portfolios_reactive(),
        selected_portfolios = selected_portfolios_reactive(),
        show_sp500 = TRUE,
        show_btc = TRUE
      )
    })
    
    output$performance_plot <- renderPlotly({
      req(portfolio_data())
      
      plot_df <- bind_rows(
        map_dfr(names(portfolio_data()$portfolios), ~tibble(
          date = portfolio_data()$portfolios[[.x]]$dates,
          return_pct = portfolio_data()$portfolios[[.x]]$cumulative_returns * 100,
          portfolio = .x
        )),
        if(!is.null(portfolio_data()$sp500)) tibble(date = portfolio_data()$sp500$dates, return_pct = portfolio_data()$sp500$cumulative_returns * 100, portfolio = "S&P 500"),
        if(!is.null(portfolio_data()$bitcoin)) tibble(date = portfolio_data()$bitcoin$dates, return_pct = portfolio_data()$bitcoin$cumulative_returns * 100, portfolio = "Bitcoin")
      )
      
      if (nrow(plot_df) == 0) return(plotly_empty())

      plot_ly(plot_df, x = ~date, y = ~return_pct, color = ~portfolio, type = 'scatter', mode = 'lines') %>%
        layout(title = "Performance Comparison", yaxis = list(title = "Cumulative Return (%)"), hovermode = 'x unified')
    })
    
    # ... Other outputs like value_boxes and metrics_table remain largely the same ...
    # (Code for these is omitted for brevity but should be kept in your file)
    
    return(reactive(list(selected = selected_portfolios_reactive())))
  })
}

# Add these helper functions to the end of mod_performance.R
calculate_max_drawdown <- function(returns) {
  if (is.null(returns) || length(returns) < 2) return(0)
  prices <- 1 + returns
  running_max <- cummax(prices)
  drawdown <- (prices / running_max - 1)
  min(drawdown, na.rm = TRUE)
}

calculate_performance_metrics <- function(data) {
  # This function from your original file can be kept, but it needs cumulative_returns
  # A simplified version is shown below
  metrics_list <- list()
  
  process_series <- function(name, series) {
    if (is.null(series) || length(series$cumulative_returns) < 2) return(NULL)
    total_ret <- tail(series$cumulative_returns, 1)
    daily_ret <- diff(log1p(series$cumulative_returns))
    
    vol <- sd(daily_ret, na.rm = TRUE) * sqrt(252)
    sharpe <- if (vol > 0) (mean(daily_ret, na.rm = TRUE) * 252) / vol else 0
    max_dd <- calculate_max_drawdown(series$cumulative_returns)
    
    tibble(
      Portfolio = name, Total_Return = total_ret, Volatility = vol,
      Sharpe_Ratio = sharpe, Max_Drawdown = max_dd
    )
  }
  
  for (name in names(data$portfolios)) {
    metrics_list[[name]] <- process_series(name, data$portfolios[[name]])
  }
  metrics_list[["S&P 500"]] <- process_series("S&P 500", data$sp500)
  metrics_list[["Bitcoin"]] <- process_series("Bitcoin", data$bitcoin)
  
  bind_rows(metrics_list)
}