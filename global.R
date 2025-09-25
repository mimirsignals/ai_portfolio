# global.R

library(shiny)
library(tidyverse)
library(quantmod)
library(plotly)
library(DT)

# Lightweight replacement for shinydashboard::box
box <- function(..., title = NULL, status = NULL, solidHeader = FALSE, width = 12) {
  if (is.null(width)) {
    width <- 12
  }

  if (!is.numeric(width) || length(width) != 1 || is.na(width)) {
    stop("box width must be a single numeric value")
  }

  width <- as.integer(width)
  width <- max(1, min(12, width))

  status_class <- switch(status,
    primary = "panel-primary",
    info = "panel-info",
    success = "panel-success",
    warning = "panel-warning",
    danger = "panel-danger",
    NULL
  )
  panel_classes <- c("panel", if (!is.null(status_class)) status_class else "panel-default")
  header_class <- "panel-heading"
  if (solidHeader) {
    header_class <- paste(header_class, "panel-heading-solid")
  }

  column(
    width,
    div(
      class = paste(panel_classes, collapse = " "),
      if (!is.null(title)) div(class = header_class, h3(class = "panel-title", title)),
      div(class = "panel-body", ...)
    )
  )
}

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
