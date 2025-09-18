# global.R

library(shiny)
library(shinydashboard)
library(tidyverse)
library(quantmod)
library(plotly)
library(DT)

# Source new and existing utilities
source("portfolio_loader.R")
source("utils_data.R")
source("utils_portfolio.R")
source("mod_performance.R")
source("mod_risk.R")
source("mod_holdings.R")

# Helper function
plotly_empty <- function(message = "No data available") {
  plot_ly() %>%
    layout(
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE),
      annotations = list(text = message, showarrow = FALSE, font = list(size = 16))
    )
}