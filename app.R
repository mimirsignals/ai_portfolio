# app.R - Simplified Portfolio Monitoring App

# Source global setup
source("global.R")

# UI Definition
ui <- dashboardPage(
  dashboardHeader(title = "Portfolio Performance Monitor"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Performance Overview", tabName = "performance", icon = icon("chart-line")),
      menuItem("Risk Metrics", tabName = "risk", icon = icon("exclamation-triangle")),
      menuItem("Holdings", tabName = "holdings", icon = icon("pie-chart"))
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
        .current-portfolio {
          border: 2px solid #3c8dbc;
        }
        .historical-portfolio {
          border: 2px solid #f39c12;
        }
        .refresh-button {
          position: fixed;
          bottom: 20px;
          right: 20px;
          z-index: 1000;
        }
      "))
    ),
    
    # Floating refresh button
    div(
      class = "refresh-button",
      actionButton("refresh_data", "Refresh Data", 
                   class = "btn-primary", icon = icon("refresh"))
    ),
    
    tabItems(
      tabItem(tabName = "performance", performanceUI("performance")),
      tabItem(tabName = "risk", riskUI("risk")),
      tabItem(tabName = "holdings", holdingsUI("holdings"))
    )
  )
)

# Server Definition
server <- function(input, output, session) {
  
  # Initialize data manager for Excel-based portfolios
  data_manager <- tryCatch({
    ExcelPortfolioManager$new(excel_path = "portfolio.xlsx")
  }, error = function(e) {
    showNotification(
      paste("Error initializing portfolio manager:", e$message),
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
    data_manager$get_all_portfolios()
  })
  
  # Shared reactive for portfolio calculations
  portfolio_calc <- portfolioCalculator()
  
  # Initialize modules
  tryCatch({
    # Performance module - returns selection info for other modules
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
    
  }, error = function(e) {
    showNotification(
      paste("Error initializing modules:", e$message),
      type = "error",
      duration = 10
    )
  })
  
  # Refresh data action
  observeEvent(input$refresh_data, {
    if (!is.null(data_manager)) {
      data_manager$reload_portfolio_data()
      showNotification("Portfolio data refreshed successfully!", type = "message")
    } else {
      showNotification("Data manager not available", type = "error")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)