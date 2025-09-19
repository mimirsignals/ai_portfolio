# mod_holdings.R - Fixed Holdings Module with Enhanced Data Validation

#' Holdings Module UI
#' @param id Module namespace ID
holdingsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "Current Portfolio Holdings",
        status = "primary",
        solidHeader = TRUE,
        width = 6,
        plotlyOutput(ns("current_pie_chart"))
      ),
      box(
        title = "Holdings Comparison",
        status = "info", 
        solidHeader = TRUE,
        width = 6,
        conditionalPanel(
          condition = "output.has_historical_data",
          ns = ns,
          plotlyOutput(ns("comparison_chart"))
        ),
        conditionalPanel(
          condition = "!output.has_historical_data",
          ns = ns,
          div(
            style = "text-align: center; padding: 50px;",
            h4("No Historical Data Available", style = "color: gray;"),
            p("Select historical portfolios in the Performance tab to compare holdings.")
          )
        )
      )
    ),
    fluidRow(
      box(
        title = "Individual Stock Performance",
        status = "success",
        solidHeader = TRUE,
        width = 12,
        plotlyOutput(ns("stock_performance_chart"))
      )
    ),
    fluidRow(
      box(
        title = "Holdings Details",
        status = "warning",
        solidHeader = TRUE,
        width = 12,
        DT::dataTableOutput(ns("holdings_table"))
      )
    )
  )
}

#' Helper function to safely validate and convert investment data
#' @param data Data frame containing investment column
#' @return Cleaned data frame with numeric investment column
safe_validate_investment_data <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(data.frame())
  }
  
  tryCatch({
    data %>%
      mutate(
        # Ensure proper data types
        investment = as.numeric(as.character(investment)),
        symbol = as.character(symbol),
        date = as.Date(date)
      ) %>%
      # Filter out invalid data
      filter(
        !is.na(investment),
        is.finite(investment),
        investment > 0,
        !is.na(symbol),
        symbol != "",
        !is.na(date)
      )
  }, error = function(e) {
    warning(paste("Error validating investment data:", e$message))
    return(data.frame())
  })
}

#' Holdings Module Server
#' @param id Module namespace ID
#' @param portfolios_reactive Reactive expression containing portfolio data
#' @param portfolio_calc Portfolio calculator function
#' @param performance_selections Reactive with selected portfolios from performance module
holdingsServer <- function(id, portfolios_reactive, portfolio_calc, performance_selections) {
  moduleServer(id, function(input, output, session) {
    
    # Get portfolio data based on performance module selections
    portfolio_data <- reactive({
      req(performance_selections())
      
      selected_portfolios <- performance_selections()
      
      if (length(selected_portfolios) == 0) {
        return(NULL)
      }
      
      tryCatch({
        portfolio_calc(
          portfolios = portfolios_reactive(),
          selected_portfolios = selected_portfolios,
          show_sp500 = FALSE,  # Don't need benchmarks for holdings analysis
          show_btc = FALSE
        )
      }, error = function(e) {
        warning(paste("Error in holdings module portfolio calculation:", e$message))
        return(NULL)
      })
    })
    
    # Check if we have historical data for comparison
    output$has_historical_data <- reactive({
      data <- portfolio_data()
      !is.null(data) && length(data$portfolios) > 1
    })
    outputOptions(output, "has_historical_data", suspendWhenHidden = FALSE)
    
    # Current portfolio (first/most recent) pie chart
    output$current_pie_chart <- renderPlotly({
      data <- portfolio_data()
      if (is.null(data) || length(data$portfolios) == 0) {
        return(plotly_empty("No current portfolio data"))
      }
      
      tryCatch({
        # Get current (first) portfolio
        current_name <- names(data$portfolios)[1]
        current_portfolio <- data$portfolios[[current_name]]
        
        # Get individual stock data with validation
        if (is.null(current_portfolio$individual_stocks) || 
            nrow(current_portfolio$individual_stocks) == 0) {
          return(plotly_empty("No individual stock data available"))
        }
        
        # Safe data validation and processing
        holdings_data <- safe_validate_investment_data(current_portfolio$individual_stocks)
        
        if (nrow(holdings_data) == 0) {
          return(plotly_empty("No valid holdings data after validation"))
        }
        
        # Get final values for each stock
        holdings_summary <- holdings_data %>%
          group_by(symbol) %>%
          arrange(date) %>%
          slice_tail(n = 1) %>%
          ungroup() %>%
          select(symbol, investment) %>%
          arrange(desc(investment))
        
        if (nrow(holdings_summary) == 0) {
          return(plotly_empty("No valid holdings data"))
        }
        
        # Final validation for plotly
        holdings_summary <- holdings_summary %>%
          mutate(
            symbol = as.character(symbol),
            investment = as.numeric(investment)
          ) %>%
          filter(
            !is.na(symbol),
            symbol != "",
            !is.na(investment),
            is.finite(investment),
            investment > 0
          )
        
        if (nrow(holdings_summary) == 0) {
          return(plotly_empty("No valid data for pie chart"))
        }
        
        # Create pie chart
        plot_ly(
          holdings_summary,
          labels = ~symbol,
          values = ~investment,
          type = 'pie',
          hovertemplate = "<b>%{label}</b><br>" +
                         "Value: $%{value:,.0f}<br>" +
                         "Percentage: %{percent}<br>" +
                         "<extra></extra>",
          marker = list(colors = RColorBrewer::brewer.pal(n = min(nrow(holdings_summary), 11), name = "Spectral"))
        ) %>%
          layout(
            title = paste("Current Portfolio Holdings:", current_name),
            showlegend = TRUE,
            legend = list(orientation = "v", x = 1.02, y = 0.5)
          )
      }, error = function(e) {
        plotly_empty(paste("Pie chart error:", e$message))
      })
    })
    
    # Holdings comparison chart (if multiple portfolios selected)
    output$comparison_chart <- renderPlotly({
      data <- portfolio_data()
      if (is.null(data) || length(data$portfolios) <= 1) {
        return(plotly_empty("Select historical portfolios to compare"))
      }
      
      tryCatch({
        # Prepare comparison data
        comparison_data <- data.frame()
        
        for (portfolio_name in names(data$portfolios)) {
          portfolio <- data$portfolios[[portfolio_name]]
          
          if (!is.null(portfolio$individual_stocks) && 
              nrow(portfolio$individual_stocks) > 0) {
            
            # Safe data validation
            validated_data <- safe_validate_investment_data(portfolio$individual_stocks)
            
            if (nrow(validated_data) > 0) {
              # Get final weights for each symbol
              final_holdings <- validated_data %>%
                group_by(symbol) %>%
                arrange(date) %>%
                slice_tail(n = 1) %>%
                ungroup()
              
              if (nrow(final_holdings) > 0) {
                total_value <- sum(final_holdings$investment, na.rm = TRUE)
                if (total_value > 0) {
                  temp_data <- final_holdings %>%
                    mutate(
                      weight = (investment / total_value) * 100,
                      portfolio = as.character(portfolio_name)
                    ) %>%
                    select(portfolio, symbol, weight) %>%
                    filter(
                      !is.na(weight),
                      is.finite(weight),
                      weight > 0
                    )
                  
                  if (nrow(temp_data) > 0) {
                    comparison_data <- rbind(comparison_data, temp_data)
                  }
                }
              }
            }
          }
        }
        
        if (nrow(comparison_data) == 0) {
          return(plotly_empty("No comparison data available"))
        }
        
        # Final data type validation for plotly
        comparison_data <- comparison_data %>%
          mutate(
            portfolio = as.character(portfolio),
            symbol = as.character(symbol),
            weight = as.numeric(weight)
          ) %>%
          filter(
            !is.na(portfolio),
            portfolio != "",
            !is.na(symbol),
            symbol != "",
            !is.na(weight),
            is.finite(weight),
            weight > 0
          )
        
        if (nrow(comparison_data) == 0) {
          return(plotly_empty("No valid comparison data"))
        }
        
        # Create grouped bar chart
        plot_ly(
          comparison_data,
          x = ~symbol,
          y = ~weight,
          color = ~portfolio,
          type = 'bar',
          hovertemplate = "<b>%{fullData.name}</b><br>" +
                         "Symbol: %{x}<br>" +
                         "Weight: %{y:.2f}%<br>" +
                         "<extra></extra>"
        ) %>%
          layout(
            title = "Portfolio Holdings Comparison",
            xaxis = list(title = "Symbols"),
            yaxis = list(title = "Weight (%)"),
            barmode = 'group',
            legend = list(orientation = "h", x = 0, y = -0.2)
          )
      }, error = function(e) {
        plotly_empty(paste("Comparison chart error:", e$message))
      })
    })
    
    # Individual stock performance chart
    output$stock_performance_chart <- renderPlotly({
      data <- portfolio_data()
      if (is.null(data) || length(data$portfolios) == 0) {
        return(plotly_empty("No portfolio data available"))
      }
      
      tryCatch({
        # Use current portfolio for individual stock performance
        current_name <- names(data$portfolios)[1]
        current_portfolio <- data$portfolios[[current_name]]
        
        if (is.null(current_portfolio$individual_stocks) || 
            nrow(current_portfolio$individual_stocks) == 0) {
          return(plotly_empty("No individual stock data available"))
        }
        
        # Safe data validation and processing
        validated_data <- safe_validate_investment_data(current_portfolio$individual_stocks)
        
        if (nrow(validated_data) == 0) {
          return(plotly_empty("No valid stock data after validation"))
        }
        
        # Prepare stock performance data
        stock_data <- validated_data %>%
          arrange(symbol, date) %>%
          group_by(symbol) %>%
          mutate(
            initial_investment = first(investment),
            return_pct = if (!is.na(first(investment)) && first(investment) > 0) {
              (investment / first(investment) - 1) * 100
            } else {
              NA_real_
            }
          ) %>%
          ungroup() %>%
          filter(
            !is.na(return_pct),
            is.finite(return_pct)
          )
        
        if (nrow(stock_data) == 0) {
          return(plotly_empty("No valid stock performance data"))
        }
        
        # Final data type validation for plotly
        stock_data <- stock_data %>%
          mutate(
            date = as.Date(date),
            return_pct = as.numeric(return_pct),
            symbol = as.character(symbol)
          ) %>%
          filter(
            !is.na(date),
            !is.na(return_pct),
            is.finite(return_pct),
            !is.na(symbol),
            symbol != ""
          )
        
        if (nrow(stock_data) == 0) {
          return(plotly_empty("No valid data for performance chart"))
        }
        
        # Create color palette
        n_stocks <- length(unique(stock_data$symbol))
        colors <- RColorBrewer::brewer.pal(n = min(max(n_stocks, 3), 11), name = "Set3")
        
        plot_ly(
          stock_data,
          x = ~date,
          y = ~return_pct,
          color = ~symbol,
          colors = colors,
          type = 'scatter',
          mode = 'lines',
          hovertemplate = "<b>%{fullData.name}</b><br>" +
                         "Date: %{x}<br>" +
                         "Return: %{y:.2f}%<br>" +
                         "<extra></extra>"
        ) %>%
          layout(
            title = "Individual Stock Performance",
            xaxis = list(title = "Date"),
            yaxis = list(title = "Return (%)"),
            hovermode = 'x unified',
            legend = list(orientation = "h", x = 0, y = -0.2)
          )
      }, error = function(e) {
        plotly_empty(paste("Stock performance error:", e$message))
      })
    })
    
    # Holdings details table
    output$holdings_table <- renderDT({
      data <- portfolio_data()
      if (is.null(data) || length(data$portfolios) == 0) {
        return(data.frame(Message = "No portfolio data available"))
      }
      
      tryCatch({
        # Create comprehensive holdings table for all selected portfolios
        holdings_summary <- data.frame()
        
        for (portfolio_name in names(data$portfolios)) {
          portfolio <- data$portfolios[[portfolio_name]]
          
          if (!is.null(portfolio$individual_stocks) && 
              nrow(portfolio$individual_stocks) > 0) {
            
            # Safe data validation
            validated_data <- safe_validate_investment_data(portfolio$individual_stocks)
            
            if (nrow(validated_data) > 0) {
              # Get final values for each stock
              final_holdings <- validated_data %>%
                group_by(symbol) %>%
                arrange(date) %>%
                summarise(
                  initial_investment = first(investment),
                  final_investment = last(investment),
                  .groups = 'drop'
                ) %>%
                filter(
                  !is.na(initial_investment),
                  !is.na(final_investment),
                  initial_investment > 0,
                  final_investment > 0
                ) %>%
                mutate(
                  total_return = (final_investment / initial_investment - 1) * 100,
                  portfolio = as.character(portfolio_name)
                )
              
              if (nrow(final_holdings) > 0) {
                holdings_summary <- rbind(holdings_summary, final_holdings)
              }
            }
          }
        }
        
        if (nrow(holdings_summary) == 0) {
          return(data.frame(Message = "No holdings data to display"))
        }
        
        # Calculate total portfolio values and weights
        holdings_summary <- holdings_summary %>%
          group_by(portfolio) %>%
          mutate(
            portfolio_total = sum(final_investment, na.rm = TRUE),
            weight = if (portfolio_total > 0) {
              (final_investment / portfolio_total) * 100
            } else {
              0
            }
          ) %>%
          ungroup() %>%
          select(portfolio, symbol, weight, initial_investment, final_investment, total_return) %>%
          filter(
            !is.na(weight),
            is.finite(weight),
            weight >= 0
          ) %>%
          arrange(portfolio, desc(weight))
        
        # Create the data table
        DT::datatable(
          holdings_summary,
          options = list(
            dom = 'tp',
            pageLength = 15,
            scrollX = TRUE,
            columnDefs = list(
              list(className = 'dt-center', targets = "_all")
            )
          ),
          rownames = FALSE,
          colnames = c(
            "Portfolio", "Symbol", "Weight (%)", 
            "Initial ($)", "Current ($)", "Return (%)"
          )
        ) %>%
          formatRound(c('weight', 'total_return'), 2) %>%
          formatCurrency(c('initial_investment', 'final_investment'), digits = 0) %>%
          formatStyle(
            'total_return',
            backgroundColor = styleInterval(0, c('#ffcccc', '#ccffcc')),
            color = styleInterval(0, c('#d9534f', '#5cb85c'))
          )
      }, error = function(e) {
        data.frame(Error = paste("Holdings table error:", e$message))
      })
    })
  })
}