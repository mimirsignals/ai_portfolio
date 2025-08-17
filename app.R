# app.R - Main application file with Rebalancing

# Source global setup
source("global.R")

# UI Definition
ui <- dashboardPage(
  dashboardHeader(title = "Portfolio Performance Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Performance Overview", tabName = "performance", icon = icon("chart-line")),
      menuItem("Risk Metrics", tabName = "risk", icon = icon("exclamation-triangle")),
      menuItem("Holdings", tabName = "holdings", icon = icon("pie-chart")),
      menuItem("Rebalancing", tabName = "rebalance", icon = icon("balance-scale")),
      menuItem("Portfolio Management", tabName = "manage", icon = icon("cogs"))
    )
  ),
  
  dashboardBody(
    # Add shinyjs if available
    if (exists("useShinyjs")) { useShinyjs() },
    
    # Custom CSS for better styling
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          border-radius: 5px;
        }
        .table-editable .form-control {
          border: none;
          box-shadow: none;
          background: transparent;
        }
        .drift-positive {
          background-color: #d4edda !important;
        }
        .drift-negative {
          background-color: #f8d7da !important;
        }
      "))
    ),
    
    tabItems(
      tabItem(tabName = "performance", performanceUI("performance")),
      tabItem(tabName = "risk", riskUI("risk")),
      tabItem(tabName = "holdings", holdingsUI("holdings")),
      tabItem(tabName = "rebalance", rebalanceUI("rebalance")),
      tabItem(
        tabName = "manage",
        fluidRow(
          box(
            title = "Portfolio Management",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            h3("Portfolio Summary"),
            DT::dataTableOutput("portfolio_summary"),
            br(),
            h4("Quick Actions"),
            fluidRow(
              column(6,
                actionButton("refresh_data", "Refresh All Data", 
                           class = "btn-primary", icon = icon("refresh"))
              ),
              column(6,
                downloadButton("export_data", "Export Portfolio Data", 
                             class = "btn-success", icon = icon("download"))
              )
            )
          )
        )
      )
    )
  )
)

# Server Definition
server <- function(input, output, session) {
  
  # Initialize data manager with error handling
  data_manager <- tryCatch({
    DataManager$new(
      csv_path = APP_CONFIG$csv_path,
      history_path = "rebalancing_history.csv"
    )
  }, error = function(e) {
    showNotification(
      paste("Error initializing data manager:", e$message),
      type = "error",
      duration = 10
    )
    return(NULL)
  })
  
  # Reactive wrapper for portfolio data
  portfolios_reactive <- reactive({
    if (is.null(data_manager)) {
      return(list())
    }
    data_manager$get_portfolios()
  })
  
  # Shared reactive for portfolio calculations
  portfolio_calc <- portfolioCalculator()
  
  # Initialize modules with returned values for inter-module communication
  tryCatch({
    # Performance module returns selected portfolios and benchmark preferences
    performance_selections <- performanceServer("performance", 
                                               portfolios_reactive, 
                                               portfolio_calc)
    
    # Risk module uses selections from performance module
    riskServer("risk", 
               portfolios_reactive, 
               portfolio_calc,
               performance_selections)
    
    # Holdings module uses selections from performance module
    holdingsServer("holdings", 
                   portfolios_reactive,
                   portfolio_calc,
                   performance_selections)
    
    # NEW: Rebalancing module
    rebalanceServer("rebalance",
                   data_manager,
                   portfolios_reactive,
                   portfolio_calc)
    
  }, error = function(e) {
    showNotification(
      paste("Error initializing modules:", e$message),
      type = "error",
      duration = 10
    )
  })
  
  # Portfolio Management Tab Outputs
  output$portfolio_summary <- DT::renderDataTable({
    if (is.null(data_manager)) {
      return(data.frame(Message = "Data manager not available"))
    }
    
    summary <- data_manager$get_portfolio_summary()
    
    DT::datatable(
      summary,
      options = list(
        dom = 'tp',
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
  
  # Refresh data action
  observeEvent(input$refresh_data, {
    if (!is.null(data_manager)) {
      # Reload portfolios and history
      data_manager$load_portfolios()
      data_manager$load_rebalancing_history()
      
      showNotification("Data refreshed successfully!", type = "message")
    } else {
      showNotification("Data manager not available", type = "error")
    }
  })
  
  # Export data handler
  output$export_data <- downloadHandler(
    filename = function() {
      paste0("portfolio_export_", Sys.Date(), ".zip")
    },
    content = function(file) {
      # Create temporary directory
      temp_dir <- tempdir()
      
      # Copy portfolio and history files
      portfolio_file <- file.path(temp_dir, "portfolios_data.csv")
      history_file <- file.path(temp_dir, "rebalancing_history.csv")
      
      if (file.exists(APP_CONFIG$csv_path)) {
        file.copy(APP_CONFIG$csv_path, portfolio_file)
      }
      
      if (file.exists("rebalancing_history.csv")) {
        file.copy("rebalancing_history.csv", history_file)
      }
      
      # Create summary report
      if (!is.null(data_manager)) {
        summary_data <- data_manager$get_portfolio_summary()
        summary_file <- file.path(temp_dir, "portfolio_summary.csv")
        write.csv(summary_data, summary_file, row.names = FALSE)
      }
      
      # Create zip file
      files_to_zip <- list.files(temp_dir, pattern = "\\.(csv)$", full.names = TRUE)
      zip(file, files_to_zip, flags = "-j")
    },
    contentType = "application/zip"
  )
  
  # Add session cleanup
  session$onSessionEnded(function() {
    if (!is.null(data_manager)) {
      message("Session ended - cleaning up resources")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)