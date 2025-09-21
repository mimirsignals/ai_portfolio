# mod_holdings.R - Updated version with stock performance plot

holdingsUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(12,
      box(
        title = "Portfolio Holdings", 
        status = "primary", 
        solidHeader = TRUE, 
        width = NULL,
        selectInput(ns("portfolio_select"), "Select Portfolio:", choices = NULL),
        h4("Portfolio Holdings"),
        DT::dataTableOutput(ns("combined_holdings_table")),
        br(),
        h4("Stock Performance Since Rebalancing"),
        plotly::plotlyOutput(ns("stock_performance_plot"), height = "400px")
      )
    )
  )
}

holdingsServer <- function(id, portfolios_reactive, portfolio_calc) {
  moduleServer(id, function(input, output, session) {
    
    # Update portfolio choices
    observe({
      portfolios <- portfolios_reactive()
      updateSelectInput(session, "portfolio_select", 
                       choices = names(portfolios),
                       selected = names(portfolios)[1])
    })
    
    # COMBINED: Show target and actual weights in one table
    output$combined_holdings_table <- DT::renderDataTable({
      req(input$portfolio_select)
      
      portfolios <- portfolios_reactive()
      selected_portfolio <- portfolios[[input$portfolio_select]]
      
      if (is.null(selected_portfolio)) {
        return(data.frame(Message = "No portfolio selected"))
      }
      
      # Get target weights
      target_weights_df <- data.frame(
        Symbol = selected_portfolio$symbols,
        Target_Weight = paste0(round(selected_portfolio$weights * 100, 1), "%"),
        stringsAsFactors = FALSE
      )
      
      # Get actual weights
      current_symbols <- selected_portfolio$symbols
      
      portfolio_data <- portfolio_calc(
        portfolios = portfolios_reactive(),
        selected_portfolios = input$portfolio_select,
        show_sp500 = FALSE,
        show_btc = FALSE
      )
      
      if (is.null(portfolio_data) || 
          is.null(portfolio_data$portfolios[[input$portfolio_select]]) ||
          is.null(portfolio_data$portfolios[[input$portfolio_select]]$individual_stocks)) {
        
        # If no performance data, just show target weights
        target_weights_df$Actual_Weight <- "N/A"
        
      } else {
        
        # Calculate actual weights for current portfolio symbols
        latest_values <- portfolio_data$portfolios[[input$portfolio_select]]$individual_stocks %>%
          filter(symbol %in% current_symbols) %>%
          group_by(symbol) %>%
          slice_tail(n = 1) %>%
          ungroup() %>%
          mutate(investment = as.numeric(investment)) %>%
          mutate(
            actual_weight_pct = (investment / sum(investment)) * 100
          ) %>%
          select(symbol, actual_weight_pct)
        
        # Join target and actual weights
        target_weights_df <- target_weights_df %>%
          left_join(latest_values, by = c("Symbol" = "symbol")) %>%
          mutate(
            Actual_Weight = ifelse(is.na(actual_weight_pct), "N/A", 
                                 paste0(round(actual_weight_pct, 1), "%"))
          ) %>%
          select(Symbol, Target_Weight, Actual_Weight)
      }
      
      DT::datatable(target_weights_df, 
                   options = list(pageLength = 20, dom = 't'),
                   rownames = FALSE,
                   colnames = c('Symbol', 'Target Weight', 'Actual Weight'))
    })
    
    # NEW: Stock Performance Plot Since Rebalancing
    output$stock_performance_plot <- plotly::renderPlotly({
      req(input$portfolio_select)
      
      portfolios <- portfolios_reactive()
      selected_portfolio <- portfolios[[input$portfolio_select]]
      
      if (is.null(selected_portfolio)) {
        return(plotly::plotly_empty())
      }
      
      # Get rebalancing date and stock symbols from selected portfolio
      rebalance_date <- as.Date(selected_portfolio$start_date)
      symbols <- selected_portfolio$symbols
      
      # Fetch raw stock data for these symbols from rebalancing date forward
      stock_data <- fetch_stock_data(symbols, rebalance_date)
      
      if (is.null(stock_data) || nrow(stock_data) == 0) {
        return(plotly::plotly_empty())
      }
      
      # Calculate cumulative returns for each stock since rebalancing date
      stock_performance <- stock_data %>%
        group_by(symbol) %>%
        arrange(date) %>%
        mutate(
          # First price on/after rebalancing date is the baseline
          baseline_price = first(adjusted_price),
          # Calculate cumulative return since rebalancing
          cumulative_return = ((adjusted_price / baseline_price) - 1) * 100
        ) %>%
        ungroup() %>%
        filter(!is.na(cumulative_return), is.finite(cumulative_return))
      
      # Create the plot
      if (nrow(stock_performance) == 0) {
        return(plotly::plotly_empty())
      }
      
      # Create plotly line chart showing cumulative returns
      p <- stock_performance %>%
        plotly::plot_ly(
          x = ~date, 
          y = ~cumulative_return, 
          color = ~symbol,
          type = 'scatter', 
          mode = 'lines',
          line = list(width = 2),
          hovertemplate = paste(
            "<b>%{fullData.name}</b><br>",
            "Date: %{x}<br>",
            "Cumulative Return: %{y:.1f}%<br>",
            "<extra></extra>"
          )
        ) %>%
        plotly::layout(
          title = list(
            text = paste("Cumulative Stock Returns Since Rebalancing on", format(rebalance_date, "%Y-%m-%d")),
            font = list(size = 16)
          ),
          xaxis = list(
            title = "Date",
            type = "date"
          ),
          yaxis = list(
            title = "Cumulative Return Since Rebalancing (%)",
            tickformat = ".1f",
            zeroline = TRUE,
            zerolinecolor = "rgba(0,0,0,0.3)",
            zerolinewidth = 1
          ),
          hovermode = 'x unified',
          legend = list(
            title = list(text = "Stock Symbol"),
            orientation = "v",
            x = 1,
            y = 1
          ),
          margin = list(l = 50, r = 50, t = 50, b = 50)
        ) %>%
        plotly::config(displayModeBar = TRUE, displaylogo = FALSE)
      
      return(p)
    })
  })
}