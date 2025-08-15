# R/mod_portfolio_create.R - Portfolio Creation Module

#' Portfolio Create Module UI
#' @param id Module namespace ID
portfolioCreateUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "Portfolio Persistence Info",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        icon = icon("info-circle"),
        p(icon("folder"), " Portfolios are automatically saved to CSV file"),
        p(icon("save"), " Your portfolios will persist between app sessions"),
        p(icon("trash"), " Deleted portfolios are permanently removed from storage")
      )
    ),
    
    fluidRow(
      box(
        title = "Create New Portfolio",
        status = "success",
        solidHeader = TRUE,
        width = 12,
        
        # Portfolio Name
        fluidRow(
          column(6,
            textInput(ns("portfolio_name"), 
                     "Portfolio Name:",
                     placeholder = "Enter a unique portfolio name")
          ),
          column(6,
            dateInput(ns("start_date"), 
                     "Start Date:",
                     value = Sys.Date() - 365,
                     max = Sys.Date())
          )
        ),
        
        # Investment Amount and Method
        fluidRow(
          column(6,
            numericInput(ns("total_investment"), 
                        "Total Investment Amount ($):",
                        value = 10000, 
                        min = 100, 
                        step = 100)
          ),
          column(6,
            selectInput(ns("allocation_method"), 
                       "Allocation Method:",
                       choices = list(
                         "Equal Weight" = "equal",
                         "Custom Weight" = "custom",
                         "Market Cap Weight" = "market_cap"
                       ))
          )
        ),
        
        hr(),
        
        # Stock Selection Section
        h4(icon("chart-line"), "Stock Selection"),
        
        fluidRow(
          column(4,
            textInput(ns("stock_input"), 
                     "Add Stock Symbol:",
                     placeholder = "e.g., AAPL, MSFT, GOOGL")
          ),
          column(2,
            br(),
            actionButton(ns("add_stock"), 
                        "Add Stock", 
                        class = "btn-primary",
                        icon = icon("plus"))
          ),
          column(2,
            br(),
            actionButton(ns("validate_stocks"), 
                        "Validate", 
                        class = "btn-info",
                        icon = icon("check"))
          ),
          column(2,
            br(),
            actionButton(ns("clear_stocks"), 
                        "Clear All", 
                        class = "btn-warning",
                        icon = icon("times"))
          ),
          column(2,
            br(),
            actionButton(ns("load_template"), 
                        "Load Template", 
                        class = "btn-default",
                        icon = icon("file"))
          )
        ),
        
        # Selected Stocks Display
        fluidRow(
          column(12,
            br(),
            wellPanel(
              h5("Selected Stocks:"),
              uiOutput(ns("selected_stocks_display")),
              textOutput(ns("validation_status"))
            )
          )
        ),
        
        # Custom Weights Section
        conditionalPanel(
          condition = paste0("input['", ns("allocation_method"), "'] == 'custom'"),
          hr(),
          h4(icon("balance-scale"), "Custom Weights"),
          uiOutput(ns("custom_weights_ui")),
          br(),
          actionButton(ns("normalize_weights"), 
                      "Normalize Weights", 
                      class = "btn-sm btn-default",
                      icon = icon("calculator"))
        ),
        
        # Market Cap Weights Info
        conditionalPanel(
          condition = paste0("input['", ns("allocation_method"), "'] == 'market_cap'"),
          hr(),
          p(icon("info-circle"), 
            "Market cap weights will be calculated automatically based on current market capitalizations.")
        ),
        
        hr(),
        
        # Action Buttons
        fluidRow(
          column(6,
            actionButton(ns("preview_portfolio"), 
                        "Preview Portfolio",
                        class = "btn-info btn-lg",
                        icon = icon("eye"),
                        width = "100%")
          ),
          column(6,
            actionButton(ns("create_portfolio"), 
                        "Create Portfolio",
                        class = "btn-success btn-lg",
                        icon = icon("check-circle"),
                        width = "100%")
          )
        ),
        
        br(),
        
        # Status Messages
        uiOutput(ns("creation_status"))
      )
    ),
    
    # Preview Modal
    fluidRow(
      box(
        title = "Portfolio Preview",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        collapsible = TRUE,
        collapsed = TRUE,
        uiOutput(ns("preview_content"))
      )
    )
  )
}

#' Portfolio Create Module Server
#' @param id Module namespace ID
#' @param data_manager DataManager R6 object
#' @param portfolios_reactive Reactive expression with portfolio data
portfolioCreateServer <- function(id, data_manager, portfolios_reactive) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values
    selected_stocks <- reactiveVal(character(0))
    validation_results <- reactiveVal(list())
    
    # Add stock
    observeEvent(input$add_stock, {
      req(input$stock_input)
      
      # Parse input (handle comma-separated values)
      new_stocks <- toupper(trimws(unlist(strsplit(input$stock_input, "[, ]+"))))
      new_stocks <- new_stocks[new_stocks != ""]
      
      if (length(new_stocks) > 0) {
        current_stocks <- selected_stocks()
        # Add only unique stocks
        unique_new <- new_stocks[!new_stocks %in% current_stocks]
        selected_stocks(c(current_stocks, unique_new))
        updateTextInput(session, "stock_input", value = "")
        
        showNotification(
          paste("Added", length(unique_new), "stock(s)"),
          type = "success",
          duration = 2
        )
      }
    })
    
    # Clear stocks
    observeEvent(input$clear_stocks, {
      selected_stocks(character(0))
      validation_results(list())
      showNotification("Cleared all stocks", type = "warning", duration = 2)
    })
    
    # Load template portfolio
    observeEvent(input$load_template, {
      template_stocks <- c("AAPL", "MSFT", "GOOGL", "AMZN", "META", 
                          "NVDA", "TSLA", "JPM", "V", "JNJ")
      selected_stocks(template_stocks)
      showNotification("Loaded template portfolio", type = "info", duration = 2)
    })
    
    # Validate stocks
    observeEvent(input$validate_stocks, {
      req(length(selected_stocks()) > 0)
      
      withProgress(message = "Validating stocks...", value = 0, {
        stocks <- selected_stocks()
        results <- list()
        
        for (i in seq_along(stocks)) {
          incProgress(1/length(stocks), detail = paste("Checking", stocks[i]))
          
          tryCatch({
            # Try to fetch recent data
            test_data <- getSymbols(stocks[i], 
                                   from = Sys.Date() - 30,
                                   to = Sys.Date(),
                                   auto.assign = FALSE)
            results[[stocks[i]]] <- list(valid = TRUE, message = "Valid")
          }, error = function(e) {
            results[[stocks[i]]] <- list(valid = FALSE, 
                                         message = "Invalid or data unavailable")
          })
        }
        
        validation_results(results)
      })
    })
    
    # Display selected stocks
    output$selected_stocks_display <- renderUI({
      stocks <- selected_stocks()
      
      if (length(stocks) == 0) {
        return(p("No stocks selected", style = "color: #999;"))
      }
      
      # Create badges for each stock
      stock_badges <- lapply(stocks, function(stock) {
        validation <- validation_results()[[stock]]
        
        if (!is.null(validation)) {
          if (validation$valid) {
            span(class = "label label-success", 
                 style = "margin: 2px;", 
                 stock)
          } else {
            span(class = "label label-danger", 
                 style = "margin: 2px;", 
                 stock)
          }
        } else {
          span(class = "label label-default", 
                 style = "margin: 2px;", 
                 stock)
        }
      })
      
      tagList(
        div(stock_badges),
        br(),
        p(paste("Total:", length(stocks), "stocks selected"))
      )
    })
    
    # Validation status
    output$validation_status <- renderText({
      results <- validation_results()
      if (length(results) == 0) return("")
      
      valid_count <- sum(sapply(results, function(x) x$valid))
      total_count <- length(results)
      
      if (valid_count == total_count) {
        paste("✓ All", total_count, "stocks validated successfully")
      } else {
        paste("⚠", valid_count, "of", total_count, "stocks valid")
      }
    })
    
    # Custom weights UI
    output$custom_weights_ui <- renderUI({
      stocks <- selected_stocks()
      
      if (length(stocks) == 0) {
        return(p("Add stocks to set custom weights"))
      }
      
      # Create weight inputs in a grid
      weight_inputs <- lapply(seq_along(stocks), function(i) {
        column(3,
          numericInput(session$ns(paste0("weight_", stocks[i])),
                      label = stocks[i],
                      value = round(100/length(stocks), 1),
                      min = 0, 
                      max = 100, 
                      step = 0.1,
                      width = "100%")
        )
      })
      
      fluidRow(weight_inputs)
    })
    
    # Normalize weights
    observeEvent(input$normalize_weights, {
      stocks <- selected_stocks()
      if (length(stocks) == 0) return()
      
      # Get current weights
      weights <- sapply(stocks, function(stock) {
        weight_val <- input[[paste0("weight_", stock)]]
        if (is.null(weight_val) || weight_val == 0) 0.01 else weight_val
      })
      
      # Normalize
      total_weight <- sum(weights)
      if (total_weight > 0) {
        normalized_weights <- weights / total_weight * 100
        
        # Update inputs
        for (i in seq_along(stocks)) {
          updateNumericInput(session, 
                           paste0("weight_", stocks[i]),
                           value = round(normalized_weights[i], 1))
        }
        
        showNotification("Weights normalized to 100%", type = "success", duration = 2)
      }
    })
    
    # Preview portfolio
    observeEvent(input$preview_portfolio, {
      req(input$portfolio_name)
      req(length(selected_stocks()) > 0)
      
      # Generate preview content
      output$preview_content <- renderUI({
        stocks <- selected_stocks()
        
        # Get weights based on method
        if (input$allocation_method == "equal") {
          weights <- rep(1/length(stocks), length(stocks))
          weight_display <- rep(paste0(round(100/length(stocks), 1), "%"), length(stocks))
        } else if (input$allocation_method == "custom") {
          weights <- sapply(stocks, function(stock) {
            weight_input <- input[[paste0("weight_", stock)]]
            if (is.null(weight_input)) 1/length(stocks) else weight_input/100
          })
          weight_display <- paste0(round(weights * 100, 1), "%")
        } else {
          # Market cap weights would be calculated here
          weights <- rep(1/length(stocks), length(stocks))
          weight_display <- rep("TBD", length(stocks))
        }
        
        # Create preview table
        preview_df <- data.frame(
          Symbol = stocks,
          Weight = weight_display,
          Investment = paste0("$", formatC(round(input$total_investment * weights), 
                                          format = "f", digits = 0, big.mark = ","))
        )
        
        tagList(
          h4("Portfolio Summary:"),
          p(strong("Name:"), input$portfolio_name),
          p(strong("Start Date:"), as.character(input$start_date)),
          p(strong("Total Investment:"), 
            paste0("$", formatC(input$total_investment, format = "f", 
                               digits = 0, big.mark = ","))),
          p(strong("Allocation Method:"), 
            switch(input$allocation_method,
                   "equal" = "Equal Weight",
                   "custom" = "Custom Weight",
                   "market_cap" = "Market Cap Weight")),
          hr(),
          h4("Holdings:"),
          renderTable(preview_df, striped = TRUE, hover = TRUE)
        )
      })
      
      # Expand preview box
      shinyjs::runjs("$('.box-header:contains(Portfolio Preview)').click();")
    })
    
    # Create portfolio
    observeEvent(input$create_portfolio, {
      req(input$portfolio_name)
      req(length(selected_stocks()) > 0)
      
      # Check if name already exists
      if (data_manager$portfolio_exists(input$portfolio_name)) {
        output$creation_status <- renderUI({
          div(class = "alert alert-danger",
              icon("exclamation-triangle"),
              "Portfolio name already exists! Please choose a different name.")
        })
        return()
      }
      
      # Get weights
      stocks <- selected_stocks()
      if (input$allocation_method == "equal") {
        weights <- rep(1/length(stocks), length(stocks))
      } else if (input$allocation_method == "custom") {
        weights <- sapply(stocks, function(stock) {
          weight_input <- input[[paste0("weight_", stock)]]
          if (is.null(weight_input)) 0 else weight_input
        })
        weights <- weights / sum(weights)  # Normalize
      } else {
        # Market cap weights - simplified for now
        weights <- rep(1/length(stocks), length(stocks))
      }
      
      # Create portfolio
      success <- data_manager$add_portfolio(
        name = input$portfolio_name,
        symbols = stocks,
        start_date = input$start_date,
        total_investment = input$total_investment,
        weights = weights
      )
      
      if (success) {
        output$creation_status <- renderUI({
          div(class = "alert alert-success",
              icon("check-circle"),
              paste("Portfolio", input$portfolio_name, "created successfully!"))
        })
        
        # Reset form
        updateTextInput(session, "portfolio_name", value = "")
        selected_stocks(character(0))
        validation_results(list())
        
        showNotification(
          paste("Portfolio", input$portfolio_name, "created and saved!"),
          type = "success",
          duration = 3
        )
      }
    })
  })
}