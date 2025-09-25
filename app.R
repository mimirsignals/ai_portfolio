# app.R

source("global.R")

ui <- navbarPage(
  title = "Portfolio Monitor",
  header = tags$head(
    tags$style(HTML('
      .panel {
        border-radius: 4px;
        border: 1px solid #ddd;
        box-shadow: 0 1px 1px rgba(0, 0, 0, 0.05);
        margin-bottom: 20px;
      }
      .panel-heading {
        padding: 10px 15px;
        border-bottom: 1px solid transparent;
        border-top-left-radius: 3px;
        border-top-right-radius: 3px;
      }
      .panel-heading-solid {
        color: #fff;
        font-weight: 600;
      }
      .panel-default .panel-heading-solid {
        background-color: #f5f5f5;
        color: #333;
      }
      .panel-primary .panel-heading-solid {
        background-color: #337ab7;
      }
      .panel-info .panel-heading-solid {
        background-color: #5bc0de;
      }
      .panel-success .panel-heading-solid {
        background-color: #5cb85c;
      }
      .panel-warning .panel-heading-solid {
        background-color: #f0ad4e;
      }
      .panel-danger .panel-heading-solid {
        background-color: #d9534f;
      }
      .panel-body {
        background-color: #fff;
        padding: 15px;
      }
      .selection-summary {
        margin-top: 10px;
      }
      .selection-summary p {
        margin: 0 0 6px;
      }
      .selection-summary ul {
        margin: 0;
        padding-left: 18px;
      }
    '))
  ),
  tabPanel(
    title = tagList(icon("chart-line"), "Performance"),
    div(class = "container-fluid", performanceUI("performance"))
  ),
  tabPanel(
    title = tagList(icon("exclamation-triangle"), "Risk"),
    div(class = "container-fluid", riskUI("risk"))
  ),
  tabPanel(
    title = tagList(icon("pie-chart"), "Holdings"),
    div(class = "container-fluid", holdingsUI("holdings"))
  ),
  tabPanel(
    title = tagList(icon("clock-rotate-left"), "History"),
    div(class = "container-fluid", historyUI("history"))
  )
)

server <- function(input, output, session) {
  portfolios_reactive <- reactive({
    load_portfolios_from_excel("portfolio.xlsx")
  })
  
  portfolio_calc <- calculate_all_portfolios_with_inheritance
  
  performance_selections <- performanceServer("performance", portfolios_reactive, portfolio_calc)
  riskServer("risk", portfolios_reactive, portfolio_calc, performance_selections)
  holdingsServer("holdings", portfolios_reactive, portfolio_calc, performance_selections)
  historyServer("history", portfolios_reactive, portfolio_calc)
}

shinyApp(ui = ui, server = server)
