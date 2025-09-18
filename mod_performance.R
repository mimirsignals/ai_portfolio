# mod_performance.R - Fixed Performance Overview Module

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
            div(style = "color: #3c8dbc; font-weight: bold; margin-bottom: 10px;",
                "Always displayed and compared vs BTC & S&P 500"),
            div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; border-left: 4px solid #3c8dbc;",
                verbatimTextOutput(ns("current_portfolio_info"))
            )
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
#' @param portfolio_calculations Portfolio calculator function
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
      selected[!is.na(selected) & selected != ""]
    })
    
    # Get portfolio data with calculations
    portfolio_data <- reactive({
      selected <- selected_portfolios()
      
      if (length(selected) == 0) return(NULL)
      
      # Debug: Check if portfolios exist
      available_portfolios <- portfolios()
      missing_portfolios <- selected[!selected %in% names(available_portfolios)]
      
      if (length(missing_portfolios) > 0) {
        message("Missing portfolios: ", paste(missing_portfolios, collapse = ", "))
        return(NULL)
      }
      
      tryCatch({
        portfolio_calculations(
          portfolios = portfolios(),
          selected_portfolios = selected,
          show_sp500 = TRUE,  # Always show S&P 500
          show_btc = TRUE     # Always show Bitcoin
        )
      }, error = function(e) {
        message("Error in portfolio calculations: ", e$message)
        return(NULL)
      })
    })
    
    # Main performance plot
    output$performance_plot <- renderPlotly({
      data <- portfolio_data()
      if (is.null(data)) return(plotly_empty("No data available or calculation error"))
      
      tryCatch({
        plot_data <- data.frame()
        
        # Add portfolio data with proper data validation
        for (name in names(data$portfolios)) {
          portfolio <- data$portfolios[[name]]
          if (!is.null(portfolio$dates) && !is.null(portfolio$cumulative_returns) &&
              length(portfolio$dates) == length(portfolio$cumulative_returns)) {
            
            # Ensure proper data types
            dates <- as.Date(portfolio$dates)
            returns <- as.numeric(portfolio$cumulative_returns)
            
            # Remove any invalid data
            valid_idx <- !is.na(dates) & is.finite(returns)
            dates <- dates[valid_idx]
            returns <- returns[valid_idx]
            
            if (length(dates) > 0) {
              temp_data <- data.frame(
                date = dates,
                return_pct = returns * 100,
                portfolio = name,
                stringsAsFactors = FALSE
              )
              plot_data <- rbind(plot_data, temp_data)
            }
          }
        }
        
        # Add S&P 500 data
        if (!is.null(data$sp500) && !is.null(data$sp500$dates) && !is.null(data$sp500$cumulative_returns)) {
          if (length(data$sp500$dates) == length(data$sp500$cumulative_returns)) {
            dates <- as.Date(data$sp500$dates)
            returns <- as.numeric(data$sp500$cumulative_returns)
            
            valid_idx <- !is.na(dates) & is.finite(returns)
            dates <- dates[valid_idx]
            returns <- returns[valid_idx]
            
            if (length(dates) > 0) {
              temp_data <- data.frame(
                date = dates,
                return_pct = returns * 100,
                portfolio = "S&P 500",
                stringsAsFactors = FALSE
              )
              plot_data <- rbind(plot_data, temp_data)
            }
          }
        }
        
        # Add Bitcoin data
        if (!is.null(data$bitcoin) && !is.null(data$bitcoin$dates) && !is.null(data$bitcoin$cumulative_returns)) {
          if (length(data$bitcoin$dates) == length(data$bitcoin$cumulative_returns)) {
            dates <- as.Date(data$bitcoin$dates)
            returns <- as.numeric(data$bitcoin$cumulative_returns)
            
            valid_idx <- !is.na(dates) & is.finite(returns)
            dates <- dates[valid_idx]
            returns <- returns[valid_idx]
            
            if (length(dates) > 0) {
              temp_data <- data.frame(
                date = dates,
                return_pct = returns * 100,
                portfolio = "Bitcoin",
                stringsAsFactors = FALSE
              )
              plot_data <- rbind(plot_data, temp_data)
            }
          }
        }
        
        if (nrow(plot_data) == 0) {
          return(plotly_empty("No valid plot data available"))
        }
        
        # Ensure all data is properly typed for plotly
        plot_data$date <- as.Date(plot_data$date)
        plot_data$return_pct <- as.numeric(plot_data$return_pct)
        plot_data$portfolio <- as.character(plot_data$portfolio)
        
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
      }, error = function(e) {
        message("Error creating plot: ", e$message)
        return(plotly_empty(paste("Plot error:", e$message)))
      })
    })
    
    # Portfolio value boxes
    output$portfolio_value_boxes <- renderUI({
      data <- portfolio_data()
      if (is.null(data)) return(NULL)
      
      boxes <- list()
      
      # Create boxes for each portfolio
      for (name in names(data$portfolios)) {
        portfolio <- data$portfolios[[name]]
        if (!is.null(portfolio$cumulative_returns) && length(portfolio$cumulative_returns) > 0) {
          final_return <- as.numeric(tail(portfolio$cumulative_returns, 1)) * 100
          if (is.finite(final_return)) {
            status <- if (grepl("Current", name)) "primary" else "warning"
            
            boxes[[length(boxes) + 1]] <- valueBox(
              value = paste0(round(final_return, 2), "%"),
              subtitle = name,
              icon = icon("chart-line"),
              color = if (status == "primary") "blue" else "yellow"
            )
          }
        }
      }
      
      # Add S&P 500 box
      if (!is.null(data$sp500) && !is.null(data$sp500$cumulative_returns) && 
          length(data$sp500$cumulative_returns) > 0) {
        sp500_return <- as.numeric(tail(data$sp500$cumulative_returns, 1)) * 100
        if (is.finite(sp500_return)) {
          boxes[[length(boxes) + 1]] <- valueBox(
            value = paste0(round(sp500_return, 2), "%"),
            subtitle = "S&P 500",
            icon = icon("chart-bar"),
            color = "green"
          )
        }
      }
      
      # Add Bitcoin box
      if (!is.null(data$bitcoin) && !is.null(data$bitcoin$cumulative_returns) && 
          length(data$bitcoin$cumulative_returns) > 0) {
        btc_return <- as.numeric(tail(data$bitcoin$cumulative_returns, 1)) * 100
        if (is.finite(btc_return)) {
          boxes[[length(boxes) + 1]] <- valueBox(
            value = paste0(round(btc_return, 2), "%"),
            subtitle = "Bitcoin",
            icon = icon("bitcoin"),
            color = "orange"
          )
        }
      }
      
      if (length(boxes) > 0) {
        do.call(fluidRow, boxes)
      } else {
        NULL
      }
    })
    
    # Performance metrics table
    output$metrics_table <- renderDT({
      data <- portfolio_data()
      if (is.null(data)) return(data.frame())
      
      tryCatch({
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
      }, error = function(e) {
        data.frame(Error = paste("Metrics calculation error:", e$message))
      })
    })
    
    # Return selections for other modules - this is the key fix!
    return(list(
      selected = selected_portfolios,
      show_sp500 = reactive(TRUE),
      show_btc = reactive(TRUE)
    ))
  })
}

# Helper function to prepare plot data
prepare_performance_plot_data <- function(data) {
  plot_data <- data.frame()
  
  # Add portfolio data
  for (name in names(data$portfolios)) {
    portfolio <- data$portfolios[[name]]
    if (!is.null(portfolio$dates) && !is.null(portfolio$cumulative_returns) &&
        length(portfolio$dates) == length(portfolio$cumulative_returns)) {
      temp_data <- data.frame(
        date = portfolio$dates,
        return_pct = portfolio$cumulative_returns * 100,
        portfolio = name
      )
      plot_data <- rbind(plot_data, temp_data)
    }
  }
  
  # Add S&P 500 data
  if (!is.null(data$sp500) && !is.null(data$sp500$dates) && !is.null(data$sp500$cumulative_returns)) {
    if (length(data$sp500$dates) == length(data$sp500$cumulative_returns)) {
      temp_data <- data.frame(
        date = data$sp500$dates,
        return_pct = data$sp500$cumulative_returns * 100,
        portfolio = "S&P 500"
      )
      plot_data <- rbind(plot_data, temp_data)
    }
  }
  
  # Add Bitcoin data
  if (!is.null(data$bitcoin) && !is.null(data$bitcoin$dates) && !is.null(data$bitcoin$cumulative_returns)) {
    if (length(data$bitcoin$dates) == length(data$bitcoin$cumulative_returns)) {
      temp_data <- data.frame(
        date = data$bitcoin$dates,
        return_pct = data$bitcoin$cumulative_returns * 100,
        portfolio = "Bitcoin"
      )
      plot_data <- rbind(plot_data, temp_data)
    }
  }
  
  return(plot_data)
}

# Helper function to calculate metrics
calculate_performance_metrics <- function(data) {
  metrics <- data.frame()
  
  # Portfolio metrics
  for (name in names(data$portfolios)) {
    portfolio <- data$portfolios[[name]]
    if (!is.null(portfolio$cumulative_returns) && length(portfolio$cumulative_returns) > 1) {
      returns <- diff(log(portfolio$cumulative_returns + 1))
      returns <- returns[is.finite(returns)]  # Remove infinite values
      
      if (length(returns) > 0) {
        metric_row <- data.frame(
          Portfolio = name,
          Total_Return = tail(portfolio$cumulative_returns, 1),
          Volatility = if (length(returns) > 1) sd(returns, na.rm = TRUE) * sqrt(252) else 0,
          Sharpe_Ratio = if (length(returns) > 1 && sd(returns, na.rm = TRUE) > 0) {
            mean(returns, na.rm = TRUE) / sd(returns, na.rm = TRUE) * sqrt(252)
          } else 0,
          Max_Drawdown = calculate_max_drawdown(portfolio$cumulative_returns)
        )
        metrics <- rbind(metrics, metric_row)
      }
    }
  }
  
  # Benchmark metrics
  if (!is.null(data$sp500) && !is.null(data$sp500$cumulative_returns)) {
    returns <- diff(log(data$sp500$cumulative_returns + 1))
    returns <- returns[is.finite(returns)]
    
    if (length(returns) > 0) {
      metric_row <- data.frame(
        Portfolio = "S&P 500",
        Total_Return = tail(data$sp500$cumulative_returns, 1),
        Volatility = if (length(returns) > 1) sd(returns, na.rm = TRUE) * sqrt(252) else 0,
        Sharpe_Ratio = if (length(returns) > 1 && sd(returns, na.rm = TRUE) > 0) {
          mean(returns, na.rm = TRUE) / sd(returns, na.rm = TRUE) * sqrt(252)
        } else 0,
        Max_Drawdown = calculate_max_drawdown(data$sp500$cumulative_returns)
      )
      metrics <- rbind(metrics, metric_row)
    }
  }
  
  if (!is.null(data$bitcoin) && !is.null(data$bitcoin$cumulative_returns)) {
    returns <- diff(log(data$bitcoin$cumulative_returns + 1))
    returns <- returns[is.finite(returns)]
    
    if (length(returns) > 0) {
      metric_row <- data.frame(
        Portfolio = "Bitcoin",
        Total_Return = tail(data$bitcoin$cumulative_returns, 1),
        Volatility = if (length(returns) > 1) sd(returns, na.rm = TRUE) * sqrt(252) else 0,
        Sharpe_Ratio = if (length(returns) > 1 && sd(returns, na.rm = TRUE) > 0) {
          mean(returns, na.rm = TRUE) / sd(returns, na.rm = TRUE) * sqrt(252)
        } else 0,
        Max_Drawdown = calculate_max_drawdown(data$bitcoin$cumulative_returns)
      )
      metrics <- rbind(metrics, metric_row)
    }
  }
  
  return(metrics)
}

# Helper function for max drawdown
calculate_max_drawdown <- function(returns) {
  if (is.null(returns) || length(returns) == 0) return(0)
  
  cumulative <- cumprod(1 + returns)
  running_max <- cummax(cumulative)
  drawdown <- (cumulative - running_max) / running_max
  
  min_drawdown <- min(drawdown, na.rm = TRUE)
  return(if (is.finite(min_drawdown)) min_drawdown else 0)
}