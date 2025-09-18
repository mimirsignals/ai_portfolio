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
    
    # Custom CSS
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
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
  
  # Simple reactive for portfolio data - just read Excel file
  portfolios_reactive <- reactive({
    tryCatch({
      portfolios <- read_portfolio_excel("portfolio.xlsx")
      
      if (length(portfolios) == 0) {
        showNotification(
          "No portfolio data found. Please check your portfolio.xlsx file.",
          type = "warning"
        )
      }
      
      return(portfolios)
    }, error = function(e) {
      showNotification(
        paste("Error reading portfolio.xlsx:", e$message),
        type = "error"
      )
      return(list())
    })
  })
  
  # Use simple portfolio calculator
  portfolio_calc <- simple_portfolio_calculator
  
  # Initialize modules
  tryCatch({
    # Performance module
    performance_selections <- performanceServer("performance", 
                                               portfolios_reactive, 
                                               portfolio_calc)
    
    # Risk module
    riskServer("risk", 
               portfolios_reactive, 
               portfolio_calc,
               performance_selections)
    
    # Holdings module
    holdingsServer("holdings", 
                   portfolios_reactive,
                   portfolio_calc,
                   performance_selections)
    
  }, error = function(e) {
    showNotification(
      paste("Error initializing modules:", e$message),
      type = "error"
    )
  })
  
  # Refresh data - just invalidate the reactive
  observeEvent(input$refresh_data, {
    # Force re-read of Excel file
    portfolios_reactive <- reactive({
      tryCatch({
        portfolios <- read_portfolio_excel("portfolio.xlsx")
        showNotification("Portfolio data refreshed!", type = "message")
        return(portfolios)
      }, error = function(e) {
        showNotification(paste("Error refreshing:", e$message), type = "error")
        return(list())
      })
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)