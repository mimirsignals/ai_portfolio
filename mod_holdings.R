# mod_holdings.R - Fixed version

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
        DT::dataTableOutput(ns("combined_holdings_table"))
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
  })
}