# R/mod_risk.R - Risk Metrics Module

#' Risk Module UI
#' @param id Module namespace ID
riskUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "Daily Returns Distribution",
        status = "primary",
        solidHeader = TRUE,
        width = 6,
        plotlyOutput(ns("returns_distribution"))
      ),
      box(
        title = "Rolling Volatility (30 days)",
        status = "primary",
        solidHeader = TRUE,
        width = 6,
        plotlyOutput(ns("volatility_plot"))
      )
    ),
    fluidRow(
      box(
        title = "Drawdown Analysis",
        status = "warning",
        solidHeader = TRUE,
        width = 12,
        plotlyOutput(ns("drawdown_plot"))
      )
    ),
    fluidRow(
      box(
        title = "Risk Metrics Summary",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        DT::dataTableOutput(ns("risk_metrics_table"))
      )
    )
  )
}

#' Risk Module Server
#' @param id Module namespace ID
#' @param portfolios_reactive Reactive expression containing portfolio data
#' @param portfolio_calc Portfolio calculator function
#' @param performance_selections Reactive with selected portfolios from performance module
riskServer <- function(id, portfolios_reactive, portfolio_calc, performance_selections) {
  moduleServer(id, function(input, output, session) {
    
    # Get portfolio data based on performance module selections
    portfolio_data <- reactive({
      req(performance_selections())
      
      selections <- performance_selections()
      
      portfolio_calc(
        portfolios = portfolios_reactive(),
        selected_portfolios = selections$selected,
        show_sp500 = selections$show_sp500,
        show_btc = selections$show_btc
      )
    })
    
    # Returns distribution plot
    output$returns_distribution <- renderPlotly({
      data <- portfolio_data()
      if (is.null(data)) return(plotly_empty())
      
      all_returns <- calculate_all_returns(data)
      
      if (length(all_returns) == 0) {
        return(plotly_empty() %>% 
                 layout(title = "No data available for returns distribution"))
      }
      
      combined_returns <- bind_rows(all_returns)
      
      plot_ly(combined_returns, x = ~daily_return, color = ~type,
              type = "histogram", alpha = 0.7, nbinsx = 30) %>%
        layout(
          title = "Distribution of Daily Returns",
          xaxis = list(title = "Daily Return (%)"),
          yaxis = list(title = "Frequency"),
          barmode = "overlay"
        )
    })
    
    # Rolling volatility plot
    output$volatility_plot <- renderPlotly({
      data <- portfolio_data()
      if (is.null(data)) return(plotly_empty())
      
      all_volatility <- calculate_rolling_volatility(data)
      
      if (length(all_volatility) == 0) {
        return(plotly_empty() %>% 
                 layout(title = "Not enough data for rolling volatility (need 30+ days)"))
      }
      
      combined_volatility <- bind_rows(all_volatility)
      
      plot_ly(combined_volatility, x = ~date, y = ~volatility, color = ~type,
              type = "scatter", mode = "lines") %>%
        layout(
          title = "30-Day Rolling Volatility",
          xaxis = list(title = "Date"),
          yaxis = list(title = "Annualized Volatility (%)")
        )
    })
    
    # Drawdown plot
    output$drawdown_plot <- renderPlotly({
      data <- portfolio_data()
      if (is.null(data)) return(plotly_empty())
      
      all_drawdown <- calculate_drawdowns(data)
      
      if (length(all_drawdown) == 0) {
        return(plotly_empty() %>% 
                 layout(title = "No data available for drawdown analysis"))
      }
      
      combined_drawdown <- bind_rows(all_drawdown)
      
      plot_ly(combined_drawdown, x = ~date, y = ~drawdown, color = ~type,
              type = "scatter", mode = "lines", fill = "tozeroy") %>%
        layout(
          title = "Drawdown Analysis",
          xaxis = list(title = "Date"),
          yaxis = list(title = "Drawdown (%)"),
          hovermode = "x unified"
        )
    })
    
    # Risk metrics summary table
    output$risk_metrics_table <- DT::renderDataTable({
      data <- portfolio_data()
      if (is.null(data)) return(data.frame())
      
      metrics <- calculate_risk_metrics(data)
      
      DT::datatable(metrics, 
                   options = list(
                     dom = 't',
                     pageLength = 20
                   ),
                   rownames = FALSE) %>%
        DT::formatRound(columns = c("VaR_95", "CVaR_95"), digits = 2) %>%
        DT::formatPercentage(columns = c("Downside_Deviation", "Max_Drawdown"), digits = 2)
    })
  })
}

# Helper functions for risk calculations

#' Calculate daily returns for all portfolios and benchmarks
calculate_all_returns <- function(data) {
  all_returns <- list()
  
  # Portfolio returns
  if (!is.null(data$portfolios)) {
    for (portfolio_name in names(data$portfolios)) {
      portfolio_info <- data$portfolios[[portfolio_name]]
      returns <- portfolio_info$portfolio_tbl %>%
        arrange(date) %>%
        mutate(
          daily_return = (investment / lag(investment) - 1) * 100,
          type = portfolio_name
        ) %>%
        filter(!is.na(daily_return)) %>%
        select(daily_return, type)
      
      if (nrow(returns) > 0) {
        all_returns[[portfolio_name]] <- returns
      }
    }
  }
  
  # S&P 500 returns
  if (!is.null(data$benchmarks$sp500)) {
    sp500_returns <- data$benchmarks$sp500 %>%
      arrange(date) %>%
      mutate(
        daily_return = (sp500 / lag(sp500) - 1) * 100,
        type = "S&P 500"
      ) %>%
      filter(!is.na(daily_return)) %>%
      select(daily_return, type)
    
    if (nrow(sp500_returns) > 0) {
      all_returns[["S&P 500"]] <- sp500_returns
    }
  }
  
  # Bitcoin returns
  if (!is.null(data$benchmarks$btc)) {
    btc_returns <- data$benchmarks$btc %>%
      arrange(date) %>%
      mutate(
        daily_return = (btc / lag(btc) - 1) * 100,
        type = "Bitcoin"
      ) %>%
      filter(!is.na(daily_return)) %>%
      select(daily_return, type)
    
    if (nrow(btc_returns) > 0) {
      all_returns[["Bitcoin"]] <- btc_returns
    }
  }
  
  all_returns
}

#' Calculate rolling volatility
calculate_rolling_volatility <- function(data, window = 30) {
  all_volatility <- list()
  
  # Portfolio volatility
  if (!is.null(data$portfolios)) {
    for (portfolio_name in names(data$portfolios)) {
      portfolio_info <- data$portfolios[[portfolio_name]]
      returns <- portfolio_info$portfolio_tbl %>%
        arrange(date) %>%
        mutate(daily_return = investment / lag(investment) - 1) %>%
        filter(!is.na(daily_return))
      
      if (nrow(returns) >= window) {
        volatility_data <- returns %>%
          mutate(
            volatility = zoo::rollapply(daily_return, width = window, FUN = sd,
                                       fill = NA, align = "right") * sqrt(252) * 100,
            date = ymd(date),
            type = portfolio_name
          ) %>%
          filter(!is.na(volatility)) %>%
          select(date, volatility, type)
        
        if (nrow(volatility_data) > 0) {
          all_volatility[[portfolio_name]] <- volatility_data
        }
      }
    }
  }
  
  # S&P 500 volatility
  if (!is.null(data$benchmarks$sp500)) {
    sp500_returns <- data$benchmarks$sp500 %>%
      arrange(date) %>%
      mutate(daily_return = sp500 / lag(sp500) - 1) %>%
      filter(!is.na(daily_return))
    
    if (nrow(sp500_returns) >= window) {
      sp500_volatility <- sp500_returns %>%
        mutate(
          volatility = zoo::rollapply(daily_return, width = window, FUN = sd,
                                     fill = NA, align = "right") * sqrt(252) * 100,
          date = ymd(date),
          type = "S&P 500"
        ) %>%
        filter(!is.na(volatility)) %>%
        select(date, volatility, type)
      
      if (nrow(sp500_volatility) > 0) {
        all_volatility[["S&P 500"]] <- sp500_volatility
      }
    }
  }
  
  # Bitcoin volatility
  if (!is.null(data$benchmarks$btc)) {
    btc_returns <- data$benchmarks$btc %>%
      arrange(date) %>%
      mutate(daily_return = btc / lag(btc) - 1) %>%
      filter(!is.na(daily_return))
    
    if (nrow(btc_returns) >= window) {
      btc_volatility <- btc_returns %>%
        mutate(
          volatility = zoo::rollapply(daily_return, width = window, FUN = sd,
                                     fill = NA, align = "right") * sqrt(252) * 100,
          date = ymd(date),
          type = "Bitcoin"
        ) %>%
        filter(!is.na(volatility)) %>%
        select(date, volatility, type)
      
      if (nrow(btc_volatility) > 0) {
        all_volatility[["Bitcoin"]] <- btc_volatility
      }
    }
  }
  
  all_volatility
}

#' Calculate drawdowns
calculate_drawdowns <- function(data) {
  all_drawdown <- list()
  
  # Portfolio drawdowns
  if (!is.null(data$portfolios)) {
    for (portfolio_name in names(data$portfolios)) {
      portfolio_info <- data$portfolios[[portfolio_name]]
      returns <- portfolio_info$portfolio_tbl %>%
        arrange(date) %>%
        mutate(
          daily_return = investment / lag(investment) - 1,
          cumulative = cumprod(1 + coalesce(daily_return, 0)),
          drawdown = (cumulative / cummax(cumulative) - 1) * 100,
          date = ymd(date),
          type = portfolio_name
        ) %>%
        select(date, drawdown, type)
      
      if (nrow(returns) > 0) {
        all_drawdown[[portfolio_name]] <- returns
      }
    }
  }
  
  # S&P 500 drawdown
  if (!is.null(data$benchmarks$sp500)) {
    sp500_drawdown <- data$benchmarks$sp500 %>%
      arrange(date) %>%
      mutate(
        daily_return = sp500 / lag(sp500) - 1,
        cumulative = cumprod(1 + coalesce(daily_return, 0)),
        drawdown = (cumulative / cummax(cumulative) - 1) * 100,
        date = ymd(date),
        type = "S&P 500"
      ) %>%
      select(date, drawdown, type)
    
    if (nrow(sp500_drawdown) > 0) {
      all_drawdown[["S&P 500"]] <- sp500_drawdown
    }
  }
  
  # Bitcoin drawdown
  if (!is.null(data$benchmarks$btc)) {
    btc_drawdown <- data$benchmarks$btc %>%
      arrange(date) %>%
      mutate(
        daily_return = btc / lag(btc) - 1,
        cumulative = cumprod(1 + coalesce(daily_return, 0)),
        drawdown = (cumulative / cummax(cumulative) - 1) * 100,
        date = ymd(date),
        type = "Bitcoin"
      ) %>%
      select(date, drawdown, type)
    
    if (nrow(btc_drawdown) > 0) {
      all_drawdown[["Bitcoin"]] <- btc_drawdown
    }
  }
  
  all_drawdown
}

#' Calculate comprehensive risk metrics
calculate_risk_metrics <- function(data) {
  metrics_list <- list()
  
  # Calculate for portfolios
  if (!is.null(data$portfolios)) {
    for (portfolio_name in names(data$portfolios)) {
      portfolio_info <- data$portfolios[[portfolio_name]]
      
      returns <- portfolio_info$portfolio_tbl %>%
        arrange(date) %>%
        mutate(daily_return = (investment / lag(investment) - 1)) %>%
        filter(!is.na(daily_return))
      
      if (nrow(returns) > 5) {
        # Basic metrics
        vol_annual <- sd(returns$daily_return, na.rm = TRUE) * sqrt(252)
        downside_returns <- returns$daily_return[returns$daily_return < 0]
        downside_dev <- if(length(downside_returns) > 0) {
          sd(downside_returns) * sqrt(252)
        } else {
          0
        }
        
        # VaR and CVaR
        var_95 <- quantile(returns$daily_return, 0.05, na.rm = TRUE) * 100
        cvar_95 <- mean(returns$daily_return[returns$daily_return <= quantile(returns$daily_return, 0.05)], 
                       na.rm = TRUE) * 100
        
        # Max drawdown
        cumulative <- cumprod(1 + returns$daily_return)
        max_dd <- min((cumulative / cummax(cumulative) - 1), na.rm = TRUE)
        
        metrics_list[[portfolio_name]] <- data.frame(
          Portfolio = portfolio_name,
          Volatility = vol_annual,
          Downside_Deviation = downside_dev,
          VaR_95 = var_95,
          CVaR_95 = cvar_95,
          Max_Drawdown = max_dd,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  
  if (length(metrics_list) > 0) {
    bind_rows(metrics_list)
  } else {
    data.frame()
  }
}