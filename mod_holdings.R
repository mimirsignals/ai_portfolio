# mod_holdings.R - Simple version

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
        h4("Target Weights"),
        DT::dataTableOutput(ns("holdings_table")),
        h4("Current Actual Weights"),
        DT::dataTableOutput(ns("actual_weights_table"))
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
    
    # Show holdings table
    output$holdings_table <- DT::renderDataTable({
      req(input$portfolio_select)
      portfolios <- portfolios_reactive()
      selected_portfolio <- portfolios[[input$portfolio_select]]
      
      if (is.null(selected_portfolio)) {
        return(data.frame(Message = "No portfolio selected"))
      }
      
      # Create simple table of symbols and weights
      holdings_df <- data.frame(
        Symbol = selected_portfolio$symbols,
        Weight = paste0(round(selected_portfolio$weights * 100, 1), "%"),
        stringsAsFactors = FALSE
      )
      
      DT::datatable(holdings_df, 
                   options = list(pageLength = 20, dom = 't'),
                   rownames = FALSE)
    })
    
    # Show actual current weights
    output$actual_weights_table <- DT::renderDataTable({
      req(input$portfolio_select)
      
      portfolio_data <- portfolio_calc(
        portfolios = portfolios_reactive(),
        selected_portfolios = input$portfolio_select,
        show_sp500 = FALSE,
        show_btc = FALSE
      )
      
      if (is.null(portfolio_data) || is.null(portfolio_data$portfolios[[input$portfolio_select]]$individual_stocks)) {
        return(data.frame(Message = "No performance data available"))
      }
      
      # Get latest values for each stock
      latest_values <- portfolio_data$portfolios[[input$portfolio_select]]$individual_stocks %>%
        group_by(symbol) %>%
        slice_tail(n = 1) %>%
        ungroup() %>%
        mutate(investment = as.numeric(investment)) %>%
        mutate(
          actual_weight = (investment / sum(investment)) * 100
        ) %>%
        select(symbol, actual_weight) %>%
        arrange(desc(actual_weight))
      
      actual_weights_df <- data.frame(
        Symbol = latest_values$symbol,
        Actual.Weight = paste0(round(latest_values$actual_weight, 1), "%"),
        stringsAsFactors = FALSE
      )
      
      DT::datatable(actual_weights_df, 
                   options = list(pageLength = 20, dom = 't'),
                   rownames = FALSE,
                   colnames = c('Symbol', 'Actual Weight'))
    })
  })
}