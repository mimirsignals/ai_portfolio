# mod_performance.R - Performance Overview Module

#' Performance Module UI
#' @param id Module namespace ID
performanceUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "Portfolio Selection",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        checkboxGroupInput(ns("selected_portfolios"), 
                          "Select Portfolios to Display:",
                          choices = c(), 
                          selected = c(), 
                          inline = TRUE),
        checkboxInput(ns("show_sp500"), "Show S&P 500", value = TRUE),
        checkboxInput(ns("show_btc"), "Show Bitcoin", value = TRUE)
      )
    ),
    fluidRow(
      box(
        title = "Portfolio Performance Comparison",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        plotlyOutput(ns("performance_plot"), height = "400px")
      )
    ),
    fluidRow(
      uiOutput(ns("portfolio_value_boxes"))
    ),
    fluidRow(
      box(
        title = "Performance Metrics",
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
    
    # Update portfolio choices
    observe({
      choices <- names(portfolios())
      selected <- if(length(choices) > 0) choices[1] else character(0)
      
      updateCheckboxGroupInput(session, "selected_portfolios",
                               choices = choices,
                               selected = selected)
    })
    
    # Get selected portfolio data - FIX: Add portfolios argument
    selected_data <- reactive({
      req(input$selected_portfolios)
      
      portfolio_calculations(
        portfolios = portfolios(),           # ADD THIS LINE
        selected_portfolios = input$selected_portfolios,
        show_sp500 = input$show_sp500,
        show_btc = input$show_btc
      )
    })
    
    # Rest of the function remains the same...
    
    # Main performance plot
    output$performance_plot <- renderPlotly({
      data <- selected_data()
      if (is.null(data)) return(plotly_empty())
      
      plot_data <- prepare_performance_plot_data(data)
      
      plot_ly(plot_data, x = ~date, y = ~return_pct, color = ~type,
              type = "scatter", mode = "lines") %>%
        layout(
          title = "Portfolio Performance Comparison",
          xaxis = list(title = "Date"),
          yaxis = list(title = "Return (%)"),
          hovermode = "x unified"
        )
    })
    
    # Dynamic value boxes
    output$portfolio_value_boxes <- renderUI({
      data <- selected_data()
      if (is.null(data)) return(NULL)
      
      create_value_boxes(data, input$show_sp500, input$show_btc)
    })
    
    # Performance metrics table
    output$metrics_table <- DT::renderDataTable({
      data <- selected_data()
      if (is.null(data)) return(data.frame())
      
      metrics <- calculate_performance_metrics(data)
      
      DT::datatable(metrics, options = list(dom = 't'))
    })
    
    # Return selected portfolios for use by other modules
    reactive({
      list(
        selected = input$selected_portfolios,
        show_sp500 = input$show_sp500,
        show_btc = input$show_btc
      )
    })
  })
}

# Helper functions (could be in utils_performance.R)

#' Prepare data for performance plot
prepare_performance_plot_data <- function(data) {
  all_data <- list()
  
  # Add portfolio data
  for (portfolio_name in names(data$portfolios)) {
    portfolio_info <- data$portfolios[[portfolio_name]]
    total_investment <- portfolio_info$total_investment
    
    portfolio_data <- portfolio_info$portfolio_tbl %>%
      mutate(
        type = portfolio_name,
        return_pct = (investment / total_investment - 1) * 100,
        date = ymd(date)
      ) %>%
      select(date, type, return_pct)
    
    all_data[[portfolio_name]] <- portfolio_data
  }
  
  # Add benchmark data
  if (!is.null(data$benchmarks$sp500)) {
    all_data[["S&P 500"]] <- data$benchmarks$sp500 %>%
      mutate(
        type = "S&P 500",
        return_pct = (sp500 / 10000 - 1) * 100,
        date = ymd(date)
      ) %>%
      select(date, type, return_pct)
  }
  
  if (!is.null(data$benchmarks$btc)) {
    all_data[["Bitcoin"]] <- data$benchmarks$btc %>%
      mutate(
        type = "Bitcoin",
        return_pct = (btc / 10000 - 1) * 100,
        date = ymd(date)
      ) %>%
      select(date, type, return_pct)
  }
  
  bind_rows(all_data)
}

#' Create value boxes for portfolios and benchmarks
create_value_boxes <- function(data, show_sp500, show_btc) {
  value_boxes <- list()
  
  # Portfolio value boxes
  for (portfolio_name in names(data$portfolios)) {
    portfolio_info <- data$portfolios[[portfolio_name]]
    current_value <- tail(portfolio_info$portfolio_tbl$investment, 1)
    total_investment <- portfolio_info$total_investment
    return_pct <- (current_value / total_investment - 1) * 100
    
    value_boxes[[portfolio_name]] <- valueBox(
      value = paste0("$", formatC(round(current_value), 
                                  format = "f", digits = 0, big.mark = ",")),
      subtitle = paste0(portfolio_name, " (", round(return_pct, 1), "%)"),
      icon = icon("chart-line"),
      color = if (return_pct > 0) "green" else "red",
      width = 4
    )
  }
  
  # Add benchmark boxes if needed
  if (!is.null(data$benchmarks$sp500) && show_sp500) {
    current_value <- tail(data$benchmarks$sp500$sp500, 1)
    return_pct <- (current_value / 10000 - 1) * 100
    
    value_boxes[["SP500"]] <- valueBox(
      value = paste0("$", formatC(round(current_value), 
                                  format = "f", digits = 0, big.mark = ",")),
      subtitle = paste0("S&P 500 (", round(return_pct, 1), "%)"),
      icon = icon("chart-area"),
      color = if (return_pct > 0) "blue" else "red",
      width = 4
    )
  }
  
  if (!is.null(data$benchmarks$btc) && show_btc) {
    current_value <- tail(data$benchmarks$btc$btc, 1)
    return_pct <- (current_value / 10000 - 1) * 100
    
    value_boxes[["BTC"]] <- valueBox(
      value = paste0("$", formatC(round(current_value), 
                                  format = "f", digits = 0, big.mark = ",")),
      subtitle = paste0("Bitcoin (", round(return_pct, 1), "%)"),
      icon = icon("bitcoin"),
      color = if (return_pct > 0) "yellow" else "red",
      width = 4
    )
  }
  
  do.call(fluidRow, value_boxes)
}

#' Calculate performance metrics for portfolios
calculate_performance_metrics <- function(data) {
  metrics_list <- list()
  
  for (portfolio_name in names(data$portfolios)) {
    portfolio_info <- data$portfolios[[portfolio_name]]
    portfolio_tbl <- portfolio_info$portfolio_tbl
    total_investment <- portfolio_info$total_investment
    
    # Calculate returns
    returns <- portfolio_tbl %>%
      arrange(date) %>%
      mutate(daily_return = (investment / lag(investment)) - 1) %>%
      filter(!is.na(daily_return))
    
    if (nrow(returns) > 0) {
      total_return <- (tail(portfolio_tbl$investment, 1) / total_investment - 1) * 100
      volatility <- sd(returns$daily_return, na.rm = TRUE) * sqrt(252) * 100
      sharpe <- mean(returns$daily_return, na.rm = TRUE) / 
                sd(returns$daily_return, na.rm = TRUE) * sqrt(252)
      
      # Calculate max drawdown
      cumulative <- cumprod(1 + returns$daily_return)
      max_drawdown <- min((cumulative / cummax(cumulative) - 1) * 100, na.rm = TRUE)
      
      metrics_list[[portfolio_name]] <- data.frame(
        Portfolio = portfolio_name,
        Total_Return = paste0(round(total_return, 2), "%"),
        Volatility = paste0(round(volatility, 2), "%"),
        Sharpe_Ratio = round(sharpe, 3),
        Max_Drawdown = paste0(round(max_drawdown, 2), "%")
      )
    }
  }
  
  if (length(metrics_list) > 0) {
    bind_rows(metrics_list)
  } else {
    data.frame()
  }
}