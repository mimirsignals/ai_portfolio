# mod_performance.R - Updated Performance Overview Module for Excel Integration

#' Performance Module UI
#' @param id Module namespace ID
performanceUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "Portfolio Selection & Comparison",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        fluidRow(
          column(6,
            h4("Current Portfolio"),
            p("Always displayed:", style = "color: #3c8dbc; font-weight: bold;"),
            verbatimTextOutput(ns("current_portfolio_info"))
          ),
          column(6,
            h4("Add Historical Portfolios for Comparison"),
            checkboxGroupInput(
              ns("historical_portfolios"),
              "Select historical portfolios:",
              choices = NULL,
              selected = NULL
            )
          )
        )
      )
    ),
    fluidRow(
      box(
        title = "Portfolio Performance vs BTC & S&P 500",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        plotlyOutput(ns("performance_plot"), height = "500px")
      )
    ),
    fluidRow(
      uiOutput(ns("portfolio_value_boxes"))
    ),
    fluidRow(
      box(
        title = "Performance Metrics Comparison",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        DT::dataTableOutput(ns("metrics_table"))
      )
    )
  )
}

#' Performance Module Server
#' @param id Module namespace ID
#' @param portfolios Reactive expression containing portfolio data
#' @param portfolio_calculations Reactive containing calculated portfolio data
performanceServer <- function(id, portfolios, portfolio_calculations) {
  moduleServer(id, function(input, output, session) {
    
    # Get all available portfolio names
    all_portfolio_names <- reactive({
      names(portfolios())
    })
    
    # Identify current vs historical portfolios
    portfolio_info <- reactive({
      portfolio_names <- all_portfolio_names()
      
      if (length(portfolio_names) == 0) return(list(current = NULL, historical = NULL))
      
      # First portfolio is always current (most recent)
      current_portfolio <- portfolio_names[1]
      historical_portfolios <- if (length(portfolio_names) > 1) portfolio_names[-1] else NULL
      
      list(current = current_portfolio, historical = historical_portfolios)
    })
    
    # Update historical portfolio choices
    observe({
      info <- portfolio_info()
      
      if (!is.null(info$historical)) {
        updateCheckboxGroupInput(
          session,
          "historical_portfolios",
          choices = setNames(info$historical, info$historical),
          selected = NULL
        )
      } else {
        updateCheckboxGroupInput(
          session,
          "historical_portfolios", 
          choices = NULL,
          selected = NULL
        )
      }
    })
    
    # Show current portfolio info
    output$current_portfolio_info <- renderText({
      info <- portfolio_info()
      if (is.null(info$current)) return("No portfolio data available")
      
      current <- portfolios()[[info$current]]
      if (is.null(current)) return("Current portfolio not found")
      
      paste0(
        "Portfolio: ", info$current, "\n",
        "Symbols: ", paste(current$symbols, collapse = ", "), "\n",
        "Date: ", format(current$start_date, "%Y-%m-%d"), "\n",
        "Positions: ", length(current$symbols)
      )
    })
    
    # Get selected portfolios (always include current + selected historical)
    selected_portfolios <- reactive({
      info <- portfolio_info()
      selected <- c(info$current, input$historical_portfolios)
      selected[!is.na(selected)]
    })
    
    # Get portfolio data with calculations
    portfolio_data <- reactive({
      selected <- selected_portfolios()
      
      if (length(selected) == 0) return(NULL)
      
      portfolio_calculations(
        portfolios = portfolios(),
        selected_portfolios = selected,
        show_sp500 = TRUE,  # Always show S&P 500
        show_btc = TRUE     # Always show Bitcoin
      )
    })
    
    # Main performance plot
    output$performance_plot <- renderPlotly({
      data <- portfolio_data()
      if (is.null(data)) return(plotly_empty())
      
      plot_data <- prepare_performance_plot_data(data)
      
      # Create color scheme
      colors <- c(
        "#3c8dbc",  # Current portfolio (blue)
        "#f39c12",  # Historical portfolios (orange)
        "#e74c3c",  # Additional historical (red)
        "#2ecc71",  # Additional historical (green)
        "#9b59b6",  # Additional historical (purple)
        "#1abc9c",  # Bitcoin (teal)  
        "#34495e"   # S&P 500 (dark gray)
      )
      
      plot <- plot_ly(plot_data, x = ~date, y = ~return_pct, color = ~portfolio,
                     colors = colors, type = 'scatter', mode = 'lines',
                     hovertemplate = "<b>%{fullData.name}</b><br>" +
                                   "Date: %{x}<br>" +
                                   "Return: %{y:.2f}%<br>" +
                                   "<extra></extra>") %>%
        layout(
          title = "Portfolio Performance Comparison vs Benchmarks",
          xaxis = list(title = "Date"),
          yaxis = list(title = "Cumulative Return (%)"),
          hovermode = 'x unified',
          legend = list(orientation = "h", x = 0, y = -0.2),
          margin = list(b = 100)
        )
      
      plot
    })
    
    # Portfolio value boxes
    output$portfolio_value_boxes <- renderUI({
      data <- portfolio_data()
      if (is.null(data)) return(NULL)
      
      boxes <- list()
      
      # Create boxes for each portfolio and benchmark
      for (name in names(data$portfolios)) {
        portfolio <- data$portfolios[[name]]
        final_return <- tail(portfolio$cumulative_returns, 1) * 100
        
        status <- if (grepl("Current", name)) "primary" else "warning"
        
        boxes[[length(boxes) + 1]] <- valueBox(
          value = paste0(round(final_return, 2), "%"),
          subtitle = name,
          icon = icon("chart-line"),
          color = if (status == "primary") "blue" else "yellow"
        )
      }
      
      # Add benchmark boxes
      if (!is.null(data$sp500)) {
        sp500_return <- tail(data$sp500$cumulative_returns, 1) * 100
        boxes[[length(boxes) + 1]] <- valueBox(
          value = paste0(round(sp500_return, 2), "%"),
          subtitle = "S&P 500",
          icon = icon("chart-bar"),
          color = "green"
        )
      }
      
      if (!is.null(data$bitcoin)) {
        btc_return <- tail(data$bitcoin$cumulative_returns, 1) * 100
        boxes[[length(boxes) + 1]] <- valueBox(
          value = paste0(round(btc_return, 2), "%"),
          subtitle = "Bitcoin",
          icon = icon("bitcoin"),
          color = "orange"
        )
      }
      
      do.call(fluidRow, boxes)
    })
    
    # Performance metrics table
    output$metrics_table <- renderDT({
      data <- portfolio_data()
      if (is.null(data)) return(data.frame())
      
      metrics_data <- calculate_performance_metrics(data)
      
      DT::datatable(
        metrics_data,
        options = list(
          dom = 't',
          pageLength = 20,
          scrollX = TRUE,
          columnDefs = list(
            list(className = 'dt-center', targets = "_all")
          )
        ),
        rownames = FALSE
      ) %>%
        formatPercentage(c('Total_Return', 'Volatility', 'Max_Drawdown'), 2) %>%
        formatRound(c('Sharpe_Ratio'), 3)
    })
    
    # Return selections for other modules
    list(
      selected = selected_portfolios,
      show_sp500 = reactive(TRUE),
      show_btc = reactive(TRUE)
    )
  })
}

# Helper function to prepare plot data
prepare_performance_plot_data <- function(data) {
  plot_data <- data.frame()
  
  # Add portfolio data
  for (name in names(data$portfolios)) {
    portfolio <- data$portfolios[[name]]
    temp_data <- data.frame(
      date = portfolio$dates,
      return_pct = portfolio$cumulative_returns * 100,
      portfolio = name
    )
    plot_data <- rbind(plot_data, temp_data)
  }
  
  # Add S&P 500 data
  if (!is.null(data$sp500)) {
    temp_data <- data.frame(
      date = data$sp500$dates,
      return_pct = data$sp500$cumulative_returns * 100,
      portfolio = "S&P 500"
    )
    plot_data <- rbind(plot_data, temp_data)
  }
  
  # Add Bitcoin data
  if (!is.null(data$bitcoin)) {
    temp_data <- data.frame(
      date = data$bitcoin$dates,
      return_pct = data$bitcoin$cumulative_returns * 100,
      portfolio = "Bitcoin"
    )
    plot_data <- rbind(plot_data, temp_data)
  }
  
  return(plot_data)
}

# Helper function to calculate metrics
calculate_performance_metrics <- function(data) {
  metrics <- data.frame()
  
  # Portfolio metrics
  for (name in names(data$portfolios)) {
    portfolio <- data$portfolios[[name]]
    returns <- diff(log(portfolio$cumulative_returns + 1))
    
    metric_row <- data.frame(
      Portfolio = name,
      Total_Return = tail(portfolio$cumulative_returns, 1),
      Volatility = sd(returns, na.rm = TRUE) * sqrt(252),
      Sharpe_Ratio = mean(returns, na.rm = TRUE) / sd(returns, na.rm = TRUE) * sqrt(252),
      Max_Drawdown = calculate_max_drawdown(portfolio$cumulative_returns)
    )
    metrics <- rbind(metrics, metric_row)
  }
  
  # Benchmark metrics
  if (!is.null(data$sp500)) {
    returns <- diff(log(data$sp500$cumulative_returns + 1))
    metric_row <- data.frame(
      Portfolio = "S&P 500",
      Total_Return = tail(data$sp500$cumulative_returns, 1),
      Volatility = sd(returns, na.rm = TRUE) * sqrt(252),
      Sharpe_Ratio = mean(returns, na.rm = TRUE) / sd(returns, na.rm = TRUE) * sqrt(252),
      Max_Drawdown = calculate_max_drawdown(data$sp500$cumulative_returns)
    )
    metrics <- rbind(metrics, metric_row)
  }
  
  if (!is.null(data$bitcoin)) {
    returns <- diff(log(data$bitcoin$cumulative_returns + 1))
    metric_row <- data.frame(
      Portfolio = "Bitcoin",
      Total_Return = tail(data$bitcoin$cumulative_returns, 1),
      Volatility = sd(returns, na.rm = TRUE) * sqrt(252),
      Sharpe_Ratio = mean(returns, na.rm = TRUE) / sd(returns, na.rm = TRUE) * sqrt(252),
      Max_Drawdown = calculate_max_drawdown(data$bitcoin$cumulative_returns)
    )
    metrics <- rbind(metrics, metric_row)
  }
  
  return(metrics)
}

# Helper function for max drawdown
calculate_max_drawdown <- function(returns) {
  cumulative <- cumprod(1 + returns)
  running_max <- cummax(cumulative)
  drawdown <- (cumulative - running_max) / running_max
  return(min(drawdown, na.rm = TRUE))
}