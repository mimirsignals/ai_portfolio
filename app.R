# app.R

source("global.R")

ui <- dashboardPage(
  dashboardHeader(title = "Portfolio Monitor"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Performance", tabName = "performance", icon = icon("chart-line")),
      menuItem("Risk", tabName = "risk", icon = icon("exclamation-triangle")),
      menuItem("Holdings", tabName = "holdings", icon = icon("pie-chart"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "performance", performanceUI("performance")),
      tabItem(tabName = "risk", riskUI("risk")),
      tabItem(tabName = "holdings", holdingsUI("holdings"))
    )
  )
)

server <- function(input, output, session) {
  portfolios_reactive <- reactive({
    # Make sure you are calling the new function
    load_portfolios_from_excel("portfolio.xlsx")
  })
  
  # Use the corrected wrapper function
  portfolio_calc <- calculate_all_portfolios

  performance_selections <- performanceServer("performance", portfolios_reactive, portfolio_calc)
  riskServer("risk", portfolios_reactive, portfolio_calc, performance_selections)
  holdingsServer("holdings", portfolios_reactive, portfolio_calc, performance_selections)
}

shinyApp(ui = ui, server = server)