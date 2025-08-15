# R/mod_portfolio_manage.R - Portfolio Management Module

#' Portfolio Manage Module UI
#' @param id Module namespace ID
portfolioManageUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "Manage Existing Portfolios",
        status = "warning",
        solidHeader = TRUE,
        width = 12,
        
        # Action buttons at the top
        fluidRow(
          column(3,
            actionButton(ns("refresh_table"), 
                        "Refresh", 
                        class = "btn-default",
                        icon = icon("refresh"))
          ),
          column(3,
            actionButton(ns("edit_selected"), 
                        "Edit Selected", 
                        class = "btn-primary",
                        icon = icon("edit"))
          ),
          column(3,
            actionButton(ns("duplicate_selected"), 
                        "Duplicate", 
                        class = "btn-info",
                        icon = icon("copy"))
          ),
          column(3,
            actionButton(ns("delete_selected"), 
                        "Delete Selected", 
                        class = "btn-danger",
                        icon = icon("trash"))
          )
        ),
        
        hr(),
        
        # Portfolios table
        DT::dataTableOutput(ns("portfolios_table")),
        
        br(),
        
        # Status messages
        uiOutput(ns("management_status"))
      )
    ),
    
    # Edit Portfolio Box (initially hidden)
    conditionalPanel(
      condition = paste0("input['", ns("show_edit"), "'] == true"),
      fluidRow(
        box(
          title = "Edit Portfolio",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          
          uiOutput(ns("edit_form"))
        )
      )
    ),
    
    # Portfolio Statistics
    fluidRow(
      box(
        title = "Portfolio Statistics",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        collapsible = TRUE,
        collapsed = TRUE,
        
        uiOutput(ns("portfolio_stats"))
      )
    )
  )
}

#' Portfolio Manage Module Server
#' @param id Module namespace ID
#' @param data_manager DataManager R6 object
#' @param portfolios_reactive Reactive expression with portfolio data
portfolioManageServer <- function(id, data_manager, portfolios_reactive) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values
    show_edit <- reactiveVal(FALSE)
    selected_portfolio <- reactiveVal(NULL)
    
    # Portfolios table
    output$portfolios_table <- DT::renderDataTable({
      portfolios <- portfolios_reactive()
      
      if (length(portfolios) == 0) {
        return(data.frame(Message = "No portfolios available"))
      }
      
      # Create summary table
      portfolio_summary <- data_manager$get_portfolio_summary()
      
      # Add additional columns
      portfolio_summary$Created <- sapply(names(portfolios), function(name) {
        as.character(portfolios[[name]]$start_date)
      })
      
      portfolio_summary$Stocks_Count <- sapply(names(portfolios), function(name) {
        length(portfolios[[name]]$symbols)
      })
      
      # Add status indicator
      portfolio_summary$Status <- ifelse(
        portfolio_summary$Portfolio == "Default Portfolio",
        "Protected",
        "Active"
      )
      
      DT::datatable(
        portfolio_summary,
        selection = "single",
        options = list(
          pageLength = 10,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel'),
          columnDefs = list(
            list(className = 'dt-center', targets = '_all')
          )
        ),
        rownames = FALSE
      ) %>%
        DT::formatStyle(
          "Status",
          backgroundColor = DT::styleEqual(
            c("Protected", "Active"),
            c("#fff3cd", "#d4edda")
          )
        )
    })
    
    # Refresh table
    observeEvent(input$refresh_table, {
      data_manager$load_portfolios()
      showNotification("Portfolio list refreshed", type = "info", duration = 2)
    })
    
    # Delete selected portfolio
    observeEvent(input$delete_selected, {
      selected_row <- input$portfolios_table_rows_selected
      
      if (length(selected_row) == 0) {
        showNotification("Please select a portfolio to delete", 
                        type = "warning", duration = 3)
        return()
      }
      
      portfolio_names <- names(portfolios_reactive())
      portfolio_to_delete <- portfolio_names[selected_row]
      
      # Confirm deletion with modal
      showModal(modalDialog(
        title = "Confirm Deletion",
        paste("Are you sure you want to delete portfolio:", portfolio_to_delete, "?"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("confirm_delete"), "Delete", class = "btn-danger")
        )
      ))
      
      # Store portfolio to delete
      selected_portfolio(portfolio_to_delete)
    })
    
    # Confirm deletion
    observeEvent(input$confirm_delete, {
      portfolio_to_delete <- selected_portfolio()
      
      success <- data_manager$remove_portfolio(portfolio_to_delete)
      
      if (success) {
        output$management_status <- renderUI({
          div(class = "alert alert-success",
              icon("check-circle"),
              paste("Portfolio '", portfolio_to_delete, "' deleted successfully!"))
        })
      } else {
        output$management_status <- renderUI({
          div(class = "alert alert-warning",
              icon("exclamation-triangle"),
              "Cannot delete the Default Portfolio or portfolio not found!")
        })
      }
      
      removeModal()
      selected_portfolio(NULL)
    })
    
    # Duplicate selected portfolio
    observeEvent(input$duplicate_selected, {
      selected_row <- input$portfolios_table_rows_selected
      
      if (length(selected_row) == 0) {
        showNotification("Please select a portfolio to duplicate", 
                        type = "warning", duration = 3)
        return()
      }
      
      portfolio_names <- names(portfolios_reactive())
      portfolio_to_duplicate <- portfolio_names[selected_row]
      original_portfolio <- portfolios_reactive()[[portfolio_to_duplicate]]
      
      # Generate new name
      new_name <- paste0(portfolio_to_duplicate, " (Copy)")
      counter <- 1
      while (data_manager$portfolio_exists(new_name)) {
        counter <- counter + 1
        new_name <- paste0(portfolio_to_duplicate, " (Copy ", counter, ")")
      }
      
      # Create duplicate
      success <- data_manager$add_portfolio(
        name = new_name,
        symbols = original_portfolio$symbols,
        start_date = original_portfolio$start_date,
        total_investment = original_portfolio$total_investment,
        weights = original_portfolio$weights
      )
      
      if (success) {
        output$management_status <- renderUI({
          div(class = "alert alert-success",
              icon("check-circle"),
              paste("Portfolio duplicated as '", new_name, "'"))
        })
      }
    })
    
    # Edit selected portfolio
    observeEvent(input$edit_selected, {
      selected_row <- input$portfolios_table_rows_selected
      
      if (length(selected_row) == 0) {
        showNotification("Please select a portfolio to edit", 
                        type = "warning", duration = 3)
        return()
      }
      
      portfolio_names <- names(portfolios_reactive())
      portfolio_to_edit <- portfolio_names[selected_row]
      
      if (portfolio_to_edit == "Default Portfolio") {
        showNotification("Cannot edit the Default Portfolio", 
                        type = "warning", duration = 3)
        return()
      }
      
      selected_portfolio(portfolio_to_edit)
      show_edit(TRUE)
      
      # Render edit form
      output$edit_form <- renderUI({
        portfolio <- portfolios_reactive()[[portfolio_to_edit]]
        
        tagList(
          h4("Editing: ", portfolio_to_edit),
          
          fluidRow(
            column(6,
              dateInput(session$ns("edit_start_date"),
                       "Start Date:",
                       value = portfolio$start_date,
                       max = Sys.Date())
            ),
            column(6,
              numericInput(session$ns("edit_investment"),
                          "Total Investment ($):",
                          value = portfolio$total_investment,
                          min = 100,
                          step = 100)
            )
          ),
          
          h5("Stock Symbols (comma-separated):"),
          textAreaInput(session$ns("edit_symbols"),
                       label = NULL,
                       value = paste(portfolio$symbols, collapse = ", "),
                       rows = 3),
          
          h5("Weights (comma-separated percentages):"),
          textAreaInput(session$ns("edit_weights"),
                       label = NULL,
                       value = paste(round(portfolio$weights * 100, 1), collapse = ", "),
                       rows = 3),
          
          br(),
          
          fluidRow(
            column(6,
              actionButton(session$ns("save_edits"),
                          "Save Changes",
                          class = "btn-success",
                          icon = icon("save"))
            ),
            column(6,
              actionButton(session$ns("cancel_edit"),
                          "Cancel",
                          class = "btn-default",
                          icon = icon("times"))
            )
          )
        )
      })
    })
    
    # Save edits
    observeEvent(input$save_edits, {
      portfolio_name <- selected_portfolio()
      
      # Parse symbols
      symbols <- toupper(trimws(unlist(strsplit(input$edit_symbols, ","))))
      symbols <- symbols[symbols != ""]
      
      # Parse weights
      weights_text <- trimws(unlist(strsplit(input$edit_weights, ",")))
      weights <- as.numeric(gsub("%", "", weights_text)) / 100
      
      # Validate
      if (length(symbols) == 0) {
        showNotification("Please provide at least one stock symbol", 
                        type = "error", duration = 3)
        return()
      }
      
      if (length(weights) != length(symbols)) {
        showNotification("Number of weights must match number of symbols", 
                        type = "error", duration = 3)
        return()
      }
      
      # Normalize weights
      weights <- weights / sum(weights)
      
      # Update portfolio
      success <- data_manager$update_portfolio(
        name = portfolio_name,
        symbols = symbols,
        start_date = input$edit_start_date,
        total_investment = input$edit_investment,
        weights = weights
      )
      
      if (success) {
        output$management_status <- renderUI({
          div(class = "alert alert-success",
              icon("check-circle"),
              paste("Portfolio '", portfolio_name, "' updated successfully!"))
        })
        
        show_edit(FALSE)
        selected_portfolio(NULL)
      }
    })
    
    # Cancel edit
    observeEvent(input$cancel_edit, {
      show_edit(FALSE)
      selected_portfolio(NULL)
    })
    
    # Portfolio statistics
    output$portfolio_stats <- renderUI({
      portfolios <- portfolios_reactive()
      
      if (length(portfolios) == 0) {
        return(p("No portfolios available"))
      }
      
      # Calculate statistics
      total_portfolios <- length(portfolios)
      total_investment <- sum(sapply(portfolios, function(p) p$total_investment))
      unique_stocks <- unique(unlist(lapply(portfolios, function(p) p$symbols)))
      avg_stocks_per_portfolio <- mean(sapply(portfolios, function(p) length(p$symbols)))
      
      # Most common stocks
      all_stocks <- unlist(lapply(portfolios, function(p) p$symbols))
      stock_freq <- sort(table(all_stocks), decreasing = TRUE)
      top_stocks <- head(stock_freq, 5)
      
      tagList(
        fluidRow(
          column(3,
            wellPanel(
              h4("Total Portfolios"),
              h3(total_portfolios),
              style = "text-align: center;"
            )
          ),
          column(3,
            wellPanel(
              h4("Total Investment"),
              h3(paste0("$", formatC(total_investment, format = "f", 
                                    digits = 0, big.mark = ","))),
              style = "text-align: center;"
            )
          ),
          column(3,
            wellPanel(
              h4("Unique Stocks"),
              h3(length(unique_stocks)),
              style = "text-align: center;"
            )
          ),
          column(3,
            wellPanel(
              h4("Avg Stocks/Portfolio"),
              h3(round(avg_stocks_per_portfolio, 1)),
              style = "text-align: center;"
            )
          )
        ),
        
        hr(),
        
        h4("Most Common Stocks Across Portfolios:"),
        renderTable({
          data.frame(
            Stock = names(top_stocks),
            Frequency = as.vector(top_stocks),
            Percentage = paste0(round(as.vector(top_stocks) / total_portfolios * 100, 1), "%")
          )
        }, striped = TRUE)
      )
    })
    
    # Return reactive for show_edit (for conditional panel)
    reactive({
      list(show_edit = show_edit())
    })
  })
}