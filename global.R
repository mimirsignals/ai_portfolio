# global.R - Load libraries and source modules
library(shiny)
library(shinydashboard)
library(tidyverse)
library(quantmod)
library(plotly)
library(DT)
library(lubridate)
library(shinyWidgets)

# Source all modules and utilities
source("R/mod_performance.R")
source("R/mod_risk.R")
source("R/mod_holdings.R")
source("R/mod_portfolio_create.R")
source("R/mod_portfolio_manage.R")
source("R/utils_data.R")
source("R/utils_portfolio.R")
source("R/utils_persistence.R")

# =====================================
# app.R - Main application file (clean and minimal)
# =====================================

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

server <- function(input, output, session) {
  
  # Initialize portfolio storage
  portfolios <- portfolioStorage()
  
  # Calculate portfolio data (shared reactive)
  portfolio_calculations <- portfolioCalculator(portfolios$data)
  
  # Initialize modules
  performance_selections <- performanceServer("performance", 
                                             portfolios$data, 
                                             portfolio_calculations)
  
  riskServer("risk", 
             portfolios$data, 
             portfolio_calculations,
             performance_selections)
  
  holdingsServer("holdings", 
                 portfolios$data,
                 portfolio_calculations,
                 performance_selections)
  
  portfolioCreateServer("create", 
                       portfolios$add,
                       portfolios$data)
  
  portfolioManageServer("manage", 
                       portfolios$data,
                       portfolios$remove)
}

shinyApp(ui = ui, server = server)

# =====================================
# utils_persistence.R - Portfolio storage utilities
# =====================================

#' Portfolio Storage Manager
#' @return List with reactive data and methods
portfolioStorage <- function() {
  portfolios_csv_path <- "portfolios_data.csv"
  
  # Reactive values to store portfolios
  portfolios <- reactiveValues(
    data = list(),
    loaded = FALSE
  )
  
  # Load portfolios on initialization
  load_portfolios_from_csv <- function() {
    if (file.exists(portfolios_csv_path)) {
      tryCatch({
        portfolio_df <- read.csv(portfolios_csv_path, stringsAsFactors = FALSE)
        
        for (i in 1:nrow(portfolio_df)) {
          row <- portfolio_df[i, ]
          portfolios$data[[row$portfolio_name]] <- list(
            symbols = strsplit(row$symbols, "\\|")[[1]],
            start_date = as.Date(row$start_date),
            total_investment = row$total_investment,
            weights = as.numeric(strsplit(row$weights, "\\|")[[1]])
          )
        }
        cat("Loaded", nrow(portfolio_df), "portfolios from", portfolios_csv_path, "\n")
      }, error = function(e) {
        cat("Error loading portfolios from CSV:", e$message, "\n")
        create_default_portfolio()
      })
    } else {
      cat("No existing portfolio file found, creating default portfolio\n")
      create_default_portfolio()
    }
    portfolios$loaded <- TRUE
  }
  
  # Save portfolios to CSV
  save_portfolios_to_csv <- function() {
    if (length(portfolios$data) > 0) {
      portfolio_df <- data.frame(
        portfolio_name = character(0),
        symbols = character(0),
        start_date = character(0),
        total_investment = numeric(0),
        weights = character(0),
        stringsAsFactors = FALSE
      )
      
      for (name in names(portfolios$data)) {
        portfolio_info <- portfolios$data[[name]]
        new_row <- data.frame(
          portfolio_name = name,
          symbols = paste(portfolio_info$symbols, collapse = "|"),
          start_date = as.character(portfolio_info$start_date),
          total_investment = portfolio_info$total_investment,
          weights = paste(portfolio_info$weights, collapse = "|"),
          stringsAsFactors = FALSE
        )
        portfolio_df <- rbind(portfolio_df, new_row)
      }
      
      write.csv(portfolio_df, portfolios_csv_path, row.names = FALSE)
      cat("Portfolios saved to", portfolios_csv_path, "\n")
    } else {
      if (file.exists(portfolios_csv_path)) {
        file.remove(portfolios_csv_path)
        cat("Empty portfolios - removed", portfolios_csv_path, "\n")
      }
    }
  }
  
  # Create default portfolio
  create_default_portfolio <- function() {
    default_symbols <- c("DDOG", "FSLR", "NET", "IOBT", "BEAM", 
                        "AMPX", "PGEN", "SERV", "QUBT", "INOD")
    portfolios$data[["Default Portfolio"]] <- list(
      symbols = default_symbols,
      start_date = as.Date("2025-05-21"),
      total_investment = 10000,
      weights = rep(1/length(default_symbols), length(default_symbols))
    )
    save_portfolios_to_csv()
  }
  
  # Add portfolio method
  add_portfolio <- function(name, symbols, start_date, total_investment, weights) {
    portfolios$data[[name]] <- list(
      symbols = symbols,
      start_date = start_date,
      total_investment = total_investment,
      weights = weights
    )
    save_portfolios_to_csv()
  }
  
  # Remove portfolio method  
  remove_portfolio <- function(name) {
    if (name != "Default Portfolio") {
      portfolios$data[[name]] <- NULL
      save_portfolios_to_csv()
      TRUE
    } else {
      FALSE
    }
  }
  
  # Load portfolios on startup
  if (!portfolios$loaded) {
    load_portfolios_from_csv()
  }
  
  list(
    data = reactive(portfolios$data),
    add = add_portfolio,
    remove = remove_portfolio
  )
}

# =====================================
# utils_portfolio.R - Portfolio calculation utilities
# =====================================

#' Portfolio Calculator
#' @param portfolios_reactive Reactive expression with portfolio data
#' @return Function that calculates portfolio performance
portfolioCalculator <- function(portfolios_reactive) {
  
  function(selected_portfolios, show_sp500 = FALSE, show_btc = FALSE) {
    portfolios <- portfolios_reactive()
    
    if (length(portfolios) == 0 || length(selected_portfolios) == 0) {
      return(NULL)
    }
    
    all_portfolio_data <- list()
    
    # Process each selected portfolio
    for (portfolio_name in selected_portfolios) {
      if (portfolio_name %in% names(portfolios)) {
        portfolio_info <- portfolios[[portfolio_name]]
        
        # Fetch and process stock data
        stock_data <- fetch_stock_data(
          symbols = portfolio_info$symbols,
          start_date = portfolio_info$start_date,
          weights = portfolio_info$weights,
          total_investment = portfolio_info$total_investment
        )
        
        if (!is.null(stock_data)) {
          # Calculate portfolio metrics
          portfolio_metrics <- calculate_portfolio_metrics(
            stock_data,
            portfolio_info$total_investment,
            portfolio_name
          )
          
          all_portfolio_data[[portfolio_name]] <- portfolio_metrics
        }
      }
    }
    
    # Fetch benchmark data if needed
    benchmark_data <- list()
    if (show_sp500 || show_btc) {
      earliest_date <- min(sapply(selected_portfolios, function(name) {
        portfolios[[name]]$start_date
      }))
      
      if (show_sp500) {
        benchmark_data$sp500 <- fetch_benchmark_data("^GSPC", earliest_date)
      }
      
      if (show_btc) {
        benchmark_data$btc <- fetch_benchmark_data("BTC-USD", earliest_date)
      }
    }
    
    list(
      portfolios = all_portfolio_data,
      benchmarks = benchmark_data
    )
  }
}

#' Fetch stock data for portfolio
fetch_stock_data <- function(symbols, start_date, weights, total_investment) {
  data_ls <- list()
  successful_symbols <- c()
  
  for (i in seq_along(symbols)) {
    tryCatch({
      temp_data <- getSymbols(symbols[[i]],
                             from = start_date,
                             to = (today() + 1),
                             auto.assign = FALSE)
      
      data_ls[[i]] <- temp_data %>%
        as.data.frame() %>%
        rownames_to_column("date") %>%
        as_tibble() %>%
        select(date, contains("Close")) %>%
        set_names("date", "price") %>%
        mutate(symbol = symbols[[i]])
      
      successful_symbols <- c(successful_symbols, symbols[[i]])
      
    }, error = function(e) {
      cat("Failed to download", symbols[[i]], "\n")
    })
  }
  
  if (length(data_ls) == 0) return(NULL)
  
  # Combine and process data
  bind_rows(data_ls) %>%
    arrange(symbol, date) %>%
    group_by(symbol) %>%
    mutate(
      growth = price / lag(price) - 1,
      growth = if_else(date == min(date), 0, growth)
    ) %>%
    ungroup() %>%
    left_join(
      data.frame(
        symbol = successful_symbols,
        weight = weights[match(successful_symbols, symbols)]
      ),
      by = "symbol"
    ) %>%
    group_by(symbol) %>%
    mutate(
      investment = total_investment * weight * cumprod(1 + growth)
    ) %>%
    ungroup()
}

#' Fetch benchmark data
fetch_benchmark_data <- function(symbol, start_date) {
  tryCatch({
    data <- getSymbols(symbol,
                      from = start_date,
                      to = (today() + 1),
                      auto.assign = FALSE)
    
    data %>%
      as.data.frame() %>%
      rownames_to_column("date") %>%
      as_tibble() %>%
      select(date, contains("Close")) %>%
      set_names("date", "price") %>%
      arrange(date) %>%
      mutate(
        growth = price / lag(price) - 1,
        growth = if_else(date == min(date), 0, growth),
        value = 10000 * cumprod(1 + growth)
      ) %>%
      select(date, !!sym(tolower(gsub("[^A-Za-z]", "", symbol))) := value)
  }, error = function(e) {
    cat("Failed to download benchmark", symbol, "\n")
    NULL
  })
}

#' Calculate portfolio metrics
calculate_portfolio_metrics <- function(stock_data, total_investment, portfolio_name) {
  portfolio_tbl <- stock_data %>%
    group_by(date) %>%
    summarise(investment = sum(investment), .groups = 'drop') %>%
    mutate(portfolio_name = portfolio_name)
  
  list(
    portfolio_tbl = portfolio_tbl,
    individual_stocks = stock_data,
    successful_symbols = unique(stock_data$symbol),
    total_investment = total_investment
  )
}