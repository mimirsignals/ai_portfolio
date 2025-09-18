# mod_holdings.R - Fixed Holdings Module

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

#' Holdings Module Server
#' @param id Module namespace ID
#' @param portfolios_reactive Reactive expression containing portfolio data
#' @param portfolio_calc Portfolio calculator function
#' @param performance_selections Reactive with selected portfolios from performance module
holdingsServer <- function(id, portfolios_reactive, portfolio_calc, performance_selections) {
  moduleServer(id, function(input, output, session) {
    
    # Get portfolio data based on performance module selections
    portfolio_data <- reactive({
      req(performance_selections)
      
      selections <- performance_selections
      if (is.null(selections) || !is.list(selections)) {
        return(NULL)
      }
      
      selected_portfolios <- if (is.function(selections$selected)) {
        selections$selected()
      } else {
        selections$selected
      }
      
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
        
        # Get individual stock data
        if (is.null(current_portfolio$individual_stocks) || 
            nrow(current_portfolio$individual_stocks) == 0) {
          return(plotly_empty("No individual stock data available"))
        }
        
        # Get final values for each stock with proper data validation
        holdings_data <- current_portfolio$individual_stocks %>%
          mutate(
            investment = as.numeric(investment),
            symbol = as.character(symbol)
          ) %>%
          filter(is.finite(investment), investment > 0) %>%
          group_by(symbol) %>%
          slice_tail(n = 1) %>%
          ungroup() %>%
          select(symbol, investment) %>%
          arrange(desc(investment))
        
        if (nrow(holdings_data) == 0) {
          return(plotly_empty("No valid holdings data"))
        }
        
        # Ensure data types for plotly
        holdings_data$symbol <- as.character(holdings_data$symbol)
        holdings_data$investment <- as.numeric(holdings_data$investment)
        
        plot_ly(
          holdings_data,
          labels = ~symbol,
          values = ~investment,
          type = 'pie',
          hovertemplate = "<b>%{label}</b><br>" +
                         "Value: $%{value:,.0f}<br>" +
                         "Percentage: %{percent}<br>" +
                         "<extra></extra>",
          marker = list(colors = RColorBrewer::brewer.pal(n = min(nrow(holdings_data), 11), name = "Spectral"))
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
            
            # Get final weights for each symbol
            final_holdings <- portfolio$individual_stocks %>%
              mutate(
                investment = as.numeric(investment),
                symbol = as.character(symbol)
              ) %>%
              filter(is.finite(investment), investment > 0) %>%
              group_by(symbol) %>%
              slice_tail(n = 1) %>%
              ungroup()
            
            if (nrow(final_holdings) > 0) {
              total_value <- sum(final_holdings$investment, na.rm = TRUE)
              if (total_value > 0) {
                final_holdings$weight <- final_holdings$investment / total_value
                
                temp_data <- data.frame(
                  portfolio = as.character(portfolio_name),
                  symbol = as.character(final_holdings$symbol),
                  weight = as.numeric(final_holdings$weight) * 100,  # Convert to percentage
                  stringsAsFactors = FALSE
                )
                
                comparison_data <- rbind(comparison_data, temp_data)
              }
            }
          }
        }
        
        if (nrow(comparison_data) == 0) {
          return(plotly_empty("No comparison data available"))
        }
        
        # Ensure proper data types
        comparison_data$portfolio <- as.character(comparison_data$portfolio)
        comparison_data$symbol <- as.character(comparison_data$symbol)
        comparison_data$weight <- as.numeric(comparison_data$weight)
        
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
        
        # Prepare stock performance data with proper validation
        stock_data <- current_portfolio$individual_stocks %>%
          mutate(
            date = as.Date(date),
            investment = as.numeric(investment),
            symbol = as.character(symbol)
          ) %>%
          filter(is.finite(investment), investment > 0, !is.na(date)) %>%
          group_by(symbol) %>%
          arrange(date) %>%
          mutate(
            initial_investment = first(investment),
            return_pct = if (first(investment) > 0) {
              (investment / first(investment) - 1) * 100
            } else {
              0
            }
          ) %>%
          ungroup() %>%
          filter(is.finite(return_pct))
        
        if (nrow(stock_data) == 0) {
          return(plotly_empty("No valid stock performance data"))
        }
        
        # Ensure proper data types for plotly
        stock_data$date <- as.Date(stock_data$date)
        stock_data$return_pct <- as.numeric(stock_data$return_pct)
        stock_data$symbol <- as.character(stock_data$symbol)
        
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
            
            # Get final values for each stock
            final_holdings <- portfolio$individual_stocks %>%
              filter(is.finite(investment), investment > 0) %>%
              group_by(symbol) %>%
              summarise(
                initial_investment = first(investment),
                final_investment = last(investment),
                .groups = 'drop'
              ) %>%
              filter(initial_investment > 0) %>%
              mutate(
                total_return = (final_investment / initial_investment - 1) * 100,
                portfolio = portfolio_name
              )
            
            if (nrow(final_holdings) > 0) {
              holdings_summary <- rbind(holdings_summary, final_holdings)
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
          arrange(portfolio, desc(weight))
        
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