# app.R - Main application file

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
      menuItem("Create Portfolio", tabName = "create", icon = icon("plus-circle")),
      menuItem("Manage Portfolios", tabName = "manage", icon = icon("cogs"))
    )
  ),
  
  dashboardBody(
    # Add shinyjs if available
    if (exists("useShinyjs")) { useShinyjs() },
    
    tabItems(
      tabItem(tabName = "performance", performanceUI("performance")),
      tabItem(tabName = "risk", riskUI("risk")),
      tabItem(tabName = "holdings", holdingsUI("holdings"))
      # tabItem(tabName = "create", portfolioCreateUI("create")),
      # tabItem(tabName = "manage", portfolioManageUI("manage"))
    )
  )
)

# Server Definition
server <- function(input, output, session) {
  
  # Initialize data manager with error handling
  data_manager <- tryCatch({
    DataManager$new(csv_path = APP_CONFIG$csv_path)
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
    
    # Portfolio creation module
    portfolioCreateServer("create", 
                         data_manager,
                         portfolios_reactive)
    
    # Portfolio management module
    portfolioManageServer("manage", 
                         data_manager,
                         portfolios_reactive)
  }, error = function(e) {
    showNotification(
      paste("Error initializing modules:", e$message),
      type = "error",
      duration = 10
    )
  })
  
  # Add session cleanup
  session$onSessionEnded(function() {
    if (!is.null(data_manager)) {
      message("Session ended - cleaning up resources")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)