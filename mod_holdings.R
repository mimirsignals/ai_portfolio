# mod_holdings.R - Updated Holdings Module for Excel-based Portfolios

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
      req(performance_selections())
      
      selections <- performance_selections()
      
      if (length(selections$selected()) == 0) {
        return(NULL)
      }
      
      portfolio_calc(
        portfolios = portfolios_reactive(),
        selected_portfolios = selections$selected(),
        show_sp500 = FALSE,  # Don't need benchmarks for holdings analysis
        show_btc = FALSE
      )
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
      
      # Get current (first) portfolio
      current_name <- names(data$portfolios)[1]
      current_portfolio <- data$portfolios[[current_name]]
      
      # Get final values for each stock
      holdings_data <- current_portfolio$individual_stocks %>%
        group_by(symbol) %>%
        slice_tail(n = 1) %>%
        ungroup() %>%
        select(symbol, investment) %>%
        arrange(desc(investment))
      
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
    })
    
    # Holdings comparison chart (if multiple portfolios selected)
    output$comparison_chart <- renderPlotly({
      data <- portfolio_data()
      if (is.null(data) || length(data$portfolios) <= 1) {
        return(plotly_empty("Select historical portfolios to compare"))
      }
      
      # Prepare comparison data
      comparison_data <- data.frame()
      
      for (portfolio_name in names(data$portfolios)) {
        portfolio <- data$portfolios[[portfolio_name]]
        
        # Get final weights for each symbol
        final_holdings <- portfolio$individual_stocks %>%
          group_by(symbol) %>%
          slice_tail(n = 1) %>%
          ungroup()
        
        total_value <- sum(final_holdings$investment)
        final_holdings$weight <- final_holdings$investment / total_value
        
        temp_data <- data.frame(
          portfolio = portfolio_name,
          symbol = final_holdings$symbol,
          weight = final_holdings$weight * 100  # Convert to percentage
        )
        
        comparison_data <- rbind(comparison_data, temp_data)
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
    })
    
    # Individual stock performance chart
    output$stock_performance_chart <- renderPlotly({
      data <- portfolio_data()
      if (is.null(data) || length(data$portfolios) == 0) {
        return(plotly_empty("No portfolio data available"))
      }
      
      # Use current portfolio for individual stock performance
      current_name <- names(data$portfolios)[1]
      current_portfolio <- data$portfolios[[current_name]]
      
      # Prepare stock performance data
      stock_data <- current_portfolio$individual_stocks %>%
        group_by(symbol) %>%
        mutate(
          initial_investment = first(investment),
          return_pct = (investment / initial_investment - 1) * 100
        ) %>%
        ungroup()
      
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
    })
    
    # Holdings details table
    output$holdings_table <- renderDT({
      data <- portfolio_data()
      if (is.null(data) || length(data$portfolios) == 0) {
        return(data.frame(Message = "No portfolio data available"))
      }
      
      # Create comprehensive holdings table for all selected portfolios
      holdings_summary <- data.frame()
      
      for (portfolio_name in names(data$portfolios)) {
        portfolio <- data$portfolios[[portfolio_name]]
        
        # Get final values for each stock
        final_holdings <- portfolio$individual_stocks %>%
          group_by(symbol) %>%
          summarise(
            initial_investment = first(investment),
            final_investment = last(investment),
            .groups = 'drop'
          ) %>%
          mutate(
            total_return = (final_investment / initial_investment - 1) * 100,
            portfolio = portfolio_name
          )
        
        holdings_summary <- rbind(holdings_summary, final_holdings)
      }
      
      # Calculate total portfolio values and weights
      holdings_summary <- holdings_summary %>%
        group_by(portfolio) %>%
        mutate(
          portfolio_total = sum(final_investment),
          weight = (final_investment / portfolio_total) * 100
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
    })
  })
}