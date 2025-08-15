# app.R - Main application file

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
    tabItems(
      tabItem(tabName = "performance", performanceUI("performance")),
      tabItem(tabName = "risk", riskUI("risk")),
      tabItem(tabName = "holdings", holdingsUI("holdings")),
      tabItem(tabName = "create", portfolioCreateUI("create")),
      tabItem(tabName = "manage", portfolioManageUI("manage"))
    )
  )
)

# Server Definition
server <- function(input, output, session) {
  
  # Initialize data manager
  data_manager <- DataManager$new(csv_path = APP_CONFIG$csv_path)
  
  # Reactive wrapper for portfolio data
  portfolios_reactive <- reactive({
    data_manager$get_portfolios()
  })
  
  # Shared reactive for portfolio calculations
  portfolio_calc <- portfolioCalculator()
  
  # Initialize modules with returned values for inter-module communication
  
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
}

# Run the application
shinyApp(ui = ui, server = server)