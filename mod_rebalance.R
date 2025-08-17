# R/mod_rebalance.R - Portfolio Rebalancing Module

#' Rebalancing Module UI
#' @param id Module namespace ID
rebalanceUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "Portfolio Selection",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        selectInput(
          ns("selected_portfolio"),
          "Select Portfolio to Rebalance:",
          choices = NULL,
          selected = NULL
        )
      )
    ),
    
    fluidRow(
      box(
        title = "Current vs Target Allocation",
        status = "info",
        solidHeader = TRUE,
        width = 8,
        DT::dataTableOutput(ns("allocation_table"))
      ),
      box(
        title = "Rebalancing Actions",
        status = "warning",
        solidHeader = TRUE,
        width = 4,
        h4("Add New Stock"),
        textInput(ns("new_symbol"), "Stock Symbol:", placeholder = "e.g., AAPL"),
        numericInput(ns("new_weight"), "Target Weight (%):", value = 0, min = 0, max = 100, step = 0.1),
        actionButton(ns("add_stock"), "Add Stock", class = "btn-primary"),
        hr(),
        h4("Rebalancing Notes"),
        textAreaInput(ns("rebalance_notes"), "Notes:", placeholder = "Reason for rebalancing..."),
        hr(),
        actionButton(ns("save_rebalance"), "Save Rebalancing", class = "btn-success btn-lg"),
        br(), br(),
        actionButton(ns("reset_changes"), "Reset Changes", class = "btn-warning")
      )
    ),
    
    fluidRow(
      box(
        title = "Rebalancing History",
        status = "success",
        solidHeader = TRUE,
        width = 6,
        selectInput(
          ns("historical_date"),
          "Compare with Historical Allocation:",
          choices = NULL,
          selected = NULL
        ),
        DT::dataTableOutput(ns("history_table"))
      ),
      box(
        title = "Performance Comparison",
        status = "primary",
        solidHeader = TRUE,
        width = 6,
        plotlyOutput(ns("comparison_plot"))
      )
    ),
    
    fluidRow(
      box(
        title = "Rebalancing Impact",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        plotlyOutput(ns("impact_plot"))
      )
    )
  )
}

#' Rebalancing Module Server
#' @param id Module namespace ID
#' @param data_manager DataManager R6 object
#' @param portfolios_reactive Reactive expression containing portfolio data
#' @param portfolio_calc Portfolio calculator function
rebalanceServer <- function(id, data_manager, portfolios_reactive, portfolio_calc) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values for tracking changes
    values <- reactiveValues(
      current_weights = NULL,
      modified_weights = NULL,
      new_stocks = list(),
      removed_stocks = character()
    )
    
    # Update portfolio choices
    observe({
      choices <- names(portfolios_reactive())
      updateSelectInput(session, "selected_portfolio", choices = choices)
    })
    
    # Load historical dates for selected portfolio
    observe({
      req(input$selected_portfolio)
      
      history <- data_manager$get_rebalancing_history(input$selected_portfolio)
      if (nrow(history) > 0) {
        choices <- setNames(history$rebalance_date, 
                           paste(history$rebalance_date, "-", history$action_type))
        updateSelectInput(session, "historical_date", choices = choices)
      } else {
        updateSelectInput(session, "historical_date", choices = NULL)
      }
    })
    
    # Calculate current portfolio state
    current_portfolio_state <- reactive({
      req(input$selected_portfolio)
      
      portfolio <- portfolios_reactive()[[input$selected_portfolio]]
      if (is.null(portfolio)) return(NULL)
      
      # Get current market data
      portfolio_data <- portfolio_calc(
        portfolios = portfolios_reactive(),
        selected_portfolios = input$selected_portfolio,
        show_sp500 = FALSE,
        show_btc = FALSE
      )
      
      if (is.null(portfolio_data) || length(portfolio_data$portfolios) == 0) return(NULL)
      
      portfolio_info <- portfolio_data$portfolios[[input$selected_portfolio]]
      
      # Calculate current weights based on market values
      current_values <- portfolio_info$individual_stocks %>%
        group_by(symbol) %>%
        slice_tail(n = 1) %>%
        ungroup() %>%
        select(symbol, investment)
      
      total_value <- sum(current_values$investment)
      
      current_weights <- current_values %>%
        mutate(
          current_weight = investment / total_value,
          target_weight = portfolio_info$weights[match(symbol, portfolio_info$successful_symbols)],
          drift = current_weight - target_weight,
          current_value = investment
        )
      
      list(
        weights = current_weights,
        total_value = total_value,
        portfolio_info = portfolio_info
      )
    })
    
    # Initialize weights when portfolio changes
    observe({
      state <- current_portfolio_state()
      if (!is.null(state)) {
        values$current_weights <- state$weights
        values$modified_weights <- state$weights
        values$new_stocks <- list()
        values$removed_stocks <- character()
      }
    })
    
    # Render allocation table
    output$allocation_table <- DT::renderDataTable({
      tryCatch({
        state <- current_portfolio_state()
        if (is.null(state)) return(data.frame())
        
        # Combine current and any new stocks
        display_data <- values$modified_weights
        
        # Add new stocks if any
        if (length(values$new_stocks) > 0) {
          # Make sure new stocks have all the same columns as existing data
          existing_cols <- names(display_data)
          
          new_stocks_df <- data.frame(
            symbol = names(values$new_stocks),
            stringsAsFactors = FALSE
          )
          
          # Add all required columns with appropriate defaults
          for (col in existing_cols) {
            if (col == "symbol") next  # Already added
            
            if (col == "target_weight") {
              new_stocks_df[[col]] <- unlist(values$new_stocks) / 100
            } else if (col == "drift") {
              new_stocks_df[[col]] <- unlist(values$new_stocks) / 100  # Since current_weight = 0
            } else if (col %in% c("current_weight", "current_value", "investment")) {
              new_stocks_df[[col]] <- 0
            } else {
              new_stocks_df[[col]] <- NA
            }
          }
          
          # Reorder columns to match existing data
          new_stocks_df <- new_stocks_df[existing_cols]
          
          display_data <- rbind(display_data, new_stocks_df)
        }
        
        # Remove stocks marked for removal
        if (length(values$removed_stocks) > 0) {
          display_data <- display_data[!display_data$symbol %in% values$removed_stocks, ]
        }
        
        # Format for display
        display_data <- display_data %>%
          mutate(
            Current_Weight = round(current_weight * 100, 2),
            Target_Weight = round(target_weight * 100, 2),
            Drift = round(drift * 100, 2),
            Current_Value = round(current_value, 0),
            Action = ""
          ) %>%
          select(Symbol = symbol, 
                 `Current %` = Current_Weight,
                 `Target %` = Target_Weight,
                 `Drift %` = Drift,
                 `Value $` = Current_Value,
                 Action)
        
        DT::datatable(
          display_data,
          editable = list(target = "cell", disable = list(columns = c(0, 1, 3, 4, 5))),
          options = list(
            dom = 't',
            pageLength = 20,
            scrollX = TRUE
          ),
          rownames = FALSE
        ) %>%
          DT::formatCurrency(columns = "Value $", currency = "$", digits = 0) %>%
          DT::formatStyle(
            "Drift %",
            backgroundColor = DT::styleInterval(c(-1, 1), c("lightcoral", "white", "lightgreen"))
          )
        
      }, error = function(e) {
        # Return error message instead of crashing
        data.frame(
          Error = paste("Error rendering table:", e$message),
          Solution = "Try refreshing the page or selecting a different portfolio"
        )
      })
    })
    
    # Handle table edits
    observeEvent(input$allocation_table_cell_edit, {
      info <- input$allocation_table_cell_edit
      if (info$col == 2) {  # Target % column
        row_idx <- info$row
        new_value <- as.numeric(info$value) / 100
        
        # Update the modified weights
        if (row_idx <= nrow(values$modified_weights)) {
          values$modified_weights[row_idx, "target_weight"] <- new_value
          values$modified_weights[row_idx, "drift"] <- 
            values$modified_weights[row_idx, "current_weight"] - new_value
        }
      }
    })
    
    # Add new stock
    observeEvent(input$add_stock, {
      req(input$new_symbol, input$new_weight)
      
      symbol <- toupper(trimws(input$new_symbol))
      weight <- input$new_weight
      
      # Validation
      if (symbol == "") {
        showNotification("Please enter a stock symbol", type = "warning")
        return()
      }
      
      if (weight <= 0) {
        showNotification("Weight must be greater than 0", type = "warning")
        return()
      }
      
      if (weight > 50) {
        showNotification("Weight cannot exceed 50% for safety", type = "warning")
        return()
      }
      
      # Check if symbol already exists
      current_symbols <- values$modified_weights$symbol
      if (symbol %in% current_symbols || symbol %in% names(values$new_stocks)) {
        showNotification("Stock already exists in portfolio", type = "warning")
        return()
      }
      
      # Add to new stocks
      values$new_stocks[[symbol]] <- weight
      
      # Clear inputs
      updateTextInput(session, "new_symbol", value = "")
      updateNumericInput(session, "new_weight", value = 0)
      
      showNotification(paste("Added", symbol, "with", weight, "% target weight"), type = "message")
    })
    
    # Save rebalancing
    observeEvent(input$save_rebalance, {
      tryCatch({
        req(input$selected_portfolio)
        
        # Show progress
        showNotification("Saving rebalancing...", type = "message", duration = 2)
        
        # Validate we have data to save
        if (is.null(values$modified_weights)) {
          showNotification("No portfolio data to save", type = "warning")
          return()
        }
        
        # Prepare new portfolio data
        all_weights <- values$modified_weights
        
        # Add new stocks if any
        if (length(values$new_stocks) > 0) {
          new_symbols <- names(values$new_stocks)
          new_weights <- unlist(values$new_stocks) / 100
          
          # Create new rows with proper structure
          for (i in seq_along(new_symbols)) {
            # Get the column structure from existing data
            new_row <- all_weights[1, ]  # Copy structure
            new_row$symbol <- new_symbols[i]
            new_row$current_weight <- 0
            new_row$target_weight <- new_weights[i]
            new_row$drift <- new_weights[i]
            new_row$current_value <- 0
            
            all_weights <- rbind(all_weights, new_row)
          }
        }
        
        # Remove stocks marked for removal
        if (length(values$removed_stocks) > 0) {
          all_weights <- all_weights[!all_weights$symbol %in% values$removed_stocks, ]
        }
        
        # Validate we still have stocks
        if (nrow(all_weights) == 0) {
          showNotification("Cannot save empty portfolio", type = "error")
          return()
        }
        
        # Normalize weights to sum to 1
        total_weight <- sum(all_weights$target_weight, na.rm = TRUE)
        if (total_weight > 0) {
          all_weights$target_weight <- all_weights$target_weight / total_weight
        } else {
          showNotification("Invalid weights - cannot normalize", type = "error")
          return()
        }
        
        # Prepare simple data for saving
        final_symbols <- all_weights$symbol
        final_weights <- all_weights$target_weight
        notes_text <- ifelse(is.null(input$rebalance_notes) || input$rebalance_notes == "", 
                            "Manual rebalancing", input$rebalance_notes)
        
        # Try to save rebalancing history (use simpler approach if method doesn't exist)
        history_saved <- FALSE
        if (exists("save_rebalancing_action", where = data_manager)) {
          history_saved <- data_manager$save_rebalancing_action(
            portfolio_name = input$selected_portfolio,
            new_symbols = final_symbols,
            new_weights = final_weights,
            action_type = "manual_rebalance",
            notes = notes_text
          )
        } else {
          # Fallback: just note that history tracking isn't available
          message("Rebalancing history tracking not available")
          history_saved <- TRUE  # Proceed anyway
        }
        
        # Update the actual portfolio
        portfolio_updated <- data_manager$update_portfolio(
          name = input$selected_portfolio,
          symbols = final_symbols,
          weights = final_weights
        )
        
        if (portfolio_updated) {
          showNotification("Portfolio rebalanced successfully!", type = "message", duration = 5)
          
          # Clear notes and reset values
          updateTextAreaInput(session, "rebalance_notes", value = "")
          values$new_stocks <- list()
          values$removed_stocks <- character()
          
          # Force refresh of the current state
          invalidateLater(1000, session)
          
        } else {
          showNotification("Error updating portfolio", type = "error")
        }
        
      }, error = function(e) {
        # Detailed error handling
        error_msg <- paste("Error saving rebalancing:", e$message)
        warning(error_msg)
        showNotification(error_msg, type = "error", duration = 10)
      })
    })
    
    # Reset changes
    observeEvent(input$reset_changes, {
      state <- current_portfolio_state()
      if (!is.null(state)) {
        values$modified_weights <- state$weights
        values$new_stocks <- list()
        values$removed_stocks <- character()
        showNotification("Changes reset", type = "message")
      }
    })
    
    # Render history table
    output$history_table <- DT::renderDataTable({
      req(input$selected_portfolio)
      
      history <- data_manager$get_rebalancing_history(input$selected_portfolio)
      
      if (nrow(history) == 0) {
        return(data.frame(Message = "No rebalancing history"))
      }
      
      history_display <- history %>%
        select(Date = rebalance_date, Type = action_type, Notes = notes) %>%
        arrange(desc(Date))
      
      DT::datatable(
        history_display,
        options = list(dom = 't', pageLength = 10),
        rownames = FALSE
      )
    })
    
    # Render comparison plot
    output$comparison_plot <- renderPlotly({
      req(input$selected_portfolio, input$historical_date)
      
      # Get historical portfolio data
      historical_data <- data_manager$get_historical_portfolio_state(
        input$selected_portfolio, 
        input$historical_date
      )
      
      if (is.null(historical_data)) {
        return(plotly_empty() %>% layout(title = "No historical data available"))
      }
      
      # Calculate performance comparison
      comparison_data <- calculate_historical_comparison(
        portfolio_name = input$selected_portfolio,
        historical_symbols = historical_data$symbols,
        historical_weights = historical_data$weights,
        comparison_date = input$historical_date,
        portfolio_calc = portfolio_calc,
        portfolios = portfolios_reactive()
      )
      
      if (is.null(comparison_data)) {
        return(plotly_empty() %>% layout(title = "Could not calculate comparison"))
      }
      
      plot_ly(comparison_data, x = ~date, y = ~return_pct, color = ~type,
              type = "scatter", mode = "lines") %>%
        layout(
          title = "Historical vs Current Portfolio Performance",
          xaxis = list(title = "Date"),
          yaxis = list(title = "Return (%)")
        )
    })
    
    # Render impact plot
    output$impact_plot <- renderPlotly({
      state <- current_portfolio_state()
      if (is.null(state)) return(plotly_empty())
      
      # Calculate rebalancing impact
      weights_data <- values$modified_weights %>%
        mutate(
          rebalance_amount = (target_weight - current_weight) * state$total_value
        )
      
      plot_ly(weights_data, x = ~symbol, y = ~rebalance_amount,
              type = "bar",
              marker = list(
                color = ~ifelse(rebalance_amount > 0, "green", "red")
              )) %>%
        layout(
          title = "Rebalancing Impact (Buy/Sell Amounts)",
          xaxis = list(title = "Stock Symbol"),
          yaxis = list(title = "Amount ($)")
        )
    })
  })
}

# Helper function for historical comparison
calculate_historical_comparison <- function(portfolio_name, historical_symbols, 
                                          historical_weights, comparison_date,
                                          portfolio_calc, portfolios) {
  
  # Create temporary historical portfolio
  current_portfolio <- portfolios[[portfolio_name]]
  temp_portfolio <- list()
  temp_portfolio[[paste0(portfolio_name, "_historical")]] <- list(
    symbols = historical_symbols,
    weights = historical_weights,
    start_date = as.Date(comparison_date),
    total_investment = current_portfolio$total_investment
  )
  
  # Calculate both portfolios from comparison date
  tryCatch({
    historical_data <- portfolio_calc(
      portfolios = temp_portfolio,
      selected_portfolios = paste0(portfolio_name, "_historical"),
      show_sp500 = FALSE,
      show_btc = FALSE
    )
    
    current_data <- portfolio_calc(
      portfolios = portfolios,
      selected_portfolios = portfolio_name,
      show_sp500 = FALSE,
      show_btc = FALSE
    )
    
    if (is.null(historical_data) || is.null(current_data)) return(NULL)
    
    # Prepare comparison data
    hist_performance <- historical_data$portfolios[[paste0(portfolio_name, "_historical")]]$portfolio_tbl %>%
      filter(date >= as.Date(comparison_date)) %>%
      mutate(
        return_pct = (investment / current_portfolio$total_investment - 1) * 100,
        type = "Historical Portfolio"
      ) %>%
      select(date, return_pct, type)
    
    current_performance <- current_data$portfolios[[portfolio_name]]$portfolio_tbl %>%
      filter(date >= as.Date(comparison_date)) %>%
      mutate(
        return_pct = (investment / current_portfolio$total_investment - 1) * 100,
        type = "Current Portfolio"
      ) %>%
      select(date, return_pct, type)
    
    bind_rows(hist_performance, current_performance)
    
  }, error = function(e) {
    warning(paste("Error calculating historical comparison:", e$message))
    return(NULL)
  })
}