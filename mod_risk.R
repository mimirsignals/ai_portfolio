# mod_risk.R - Fixed Risk Metrics Module

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
      
      selected_portfolios <- performance_selections()
      
      if (length(selected_portfolios) == 0) {
        return(NULL)
      }
      
      tryCatch({
        portfolio_calc(
          portfolios = portfolios_reactive(),
          selected_portfolios = selected_portfolios,
          show_sp500 = TRUE,
          show_btc = TRUE
        )
      }, error = function(e) {
        warning(paste("Error in risk module portfolio calculation:", e$message))
        return(NULL)
      })
    })
    
    # Returns distribution plot
    output$returns_distribution <- renderPlotly({
      data <- portfolio_data()
      if (is.null(data)) return(plotly_empty("No data available"))
      
      tryCatch({
        all_returns <- calculate_all_returns(data)
        
        if (length(all_returns) == 0 || nrow(bind_rows(all_returns)) == 0) {
          return(plotly_empty("No returns data available"))
        }
        
        combined_returns <- bind_rows(all_returns)
        
        plot_ly(combined_returns, x = ~daily_return, color = ~type, type = "histogram",
                alpha = 0.7, histnorm = "probability density") %>%
          layout(
            title = "Daily Returns Distribution",
            xaxis = list(title = "Daily Return (%)"),
            yaxis = list(title = "Density"),
            barmode = "overlay"
          )
      }, error = function(e) {
        plotly_empty(paste("Returns distribution error:", e$message))
      })
    })
    
    # Rolling volatility plot
    output$volatility_plot <- renderPlotly({
      data <- portfolio_data()
      if (is.null(data)) return(plotly_empty("No data available"))
      
      tryCatch({
        all_volatility <- calculate_volatility(data)
        
        if (length(all_volatility) == 0 || nrow(bind_rows(all_volatility)) == 0) {
          return(plotly_empty("No volatility data available"))
        }
        
        combined_volatility <- bind_rows(all_volatility)
        
        plot_ly(combined_volatility, x = ~date, y = ~volatility, color = ~type,
                type = 'scatter', mode = 'lines') %>%
          layout(
            title = "30-Day Rolling Volatility",
            xaxis = list(title = "Date"),
            yaxis = list(title = "Volatility (%)")
          )
      }, error = function(e) {
        plotly_empty(paste("Volatility plot error:", e$message))
      })
    })
    
    # Drawdown analysis plot
    output$drawdown_plot <- renderPlotly({
      data <- portfolio_data()
      if (is.null(data)) return(plotly_empty("No data available"))
      
      tryCatch({
        all_drawdowns <- calculate_drawdowns(data)
        
        if (length(all_drawdowns) == 0 || nrow(bind_rows(all_drawdowns)) == 0) {
          return(plotly_empty("No drawdown data available"))
        }
        
        combined_drawdowns <- bind_rows(all_drawdowns)
        
        plot_ly(combined_drawdowns, x = ~date, y = ~drawdown, color = ~type,
                type = 'scatter', mode = 'lines', fill = 'tozeroy') %>%
          layout(
            title = "Portfolio Drawdown Analysis",
            xaxis = list(title = "Date"),
            yaxis = list(title = "Drawdown (%)")
          )
      }, error = function(e) {
        plotly_empty(paste("Drawdown plot error:", e$message))
      })
    })
    
    # Risk metrics summary table
    output$risk_metrics_table <- renderDT({
      data <- portfolio_data()
      if (is.null(data)) return(data.frame(Message = "No data available"))
      
      tryCatch({
        metrics <- calculate_risk_metrics(data)
        
        if (nrow(metrics) == 0) {
          return(data.frame(Message = "No risk metrics available"))
        }
        
        DT::datatable(metrics, 
                     options = list(
                       dom = 't',
                       pageLength = 20
                     ),
                     rownames = FALSE) %>%
          DT::formatRound(columns = c("VaR_95", "CVaR_95"), digits = 3) %>%
          DT::formatPercentage(columns = c("Volatility", "Downside_Deviation", "Max_Drawdown"), digits = 2)
      }, error = function(e) {
        data.frame(Error = paste("Risk metrics error:", e$message))
      })
    })
  })
}

# Helper functions for risk calculations

#' Calculate daily returns for all portfolios and benchmarks
calculate_all_returns <- function(data) {
  all_returns <- list()
  
  process_returns <- function(series, name) {
    if (is.null(series) || length(series$cumulative_returns) < 2) return(NULL)
    
    # Calculate daily returns from cumulative returns using log differences
    daily_returns <- c(0, diff(log1p(series$cumulative_returns))) * 100
    
    returns_df <- data.frame(
      date = as.Date(series$dates),
      daily_return = daily_returns,
      type = name,
      stringsAsFactors = FALSE
    ) %>%
      filter(is.finite(daily_return), !is.na(daily_return))
      
    if (nrow(returns_df) > 0) {
      return(returns_df)
    }
    return(NULL)
  }

  # Portfolio returns
  if (!is.null(data$portfolios) && length(data$portfolios) > 0) {
    for (portfolio_name in names(data$portfolios)) {
      all_returns[[portfolio_name]] <- process_returns(data$portfolios[[portfolio_name]], portfolio_name)
    }
  }
  
  # S&P 500 returns
  if (!is.null(data$sp500)) {
    all_returns[["S&P 500"]] <- process_returns(data$sp500, "S&P 500")
  }
  
  # Bitcoin returns
  if (!is.null(data$bitcoin)) {
    all_returns[["Bitcoin"]] <- process_returns(data$bitcoin, "Bitcoin")
  }
  
  return(all_returns)
}


#' Calculate rolling volatility
calculate_volatility <- function(data, window = 30) {
  all_volatility <- list()
  
  # Portfolio volatility
  if (!is.null(data$portfolios) && length(data$portfolios) > 0) {
    for (portfolio_name in names(data$portfolios)) {
      portfolio_info <- data$portfolios[[portfolio_name]]
      
      if (!is.null(portfolio_info$cumulative_returns) && 
          length(portfolio_info$cumulative_returns) > window) {
        
        cumulative_returns <- as.numeric(portfolio_info$cumulative_returns)
        dates <- as.Date(portfolio_info$dates)
        
        valid_indices <- is.finite(cumulative_returns) & !is.na(dates)
        cumulative_returns <- cumulative_returns[valid_indices]
        dates <- dates[valid_indices]
        
        if (length(cumulative_returns) > window) {
          daily_returns <- c(0, diff(log(cumulative_returns + 1)))
          
          volatility_data <- data.frame(
            date = dates,
            daily_return = daily_returns
          ) %>%
            filter(is.finite(daily_return)) %>%
            arrange(date)
          
          if (nrow(volatility_data) >= window) {
            volatility_data <- volatility_data %>%
              mutate(
                volatility = zoo::rollapply(daily_return, width = window, FUN = sd,
                                           fill = NA, align = "right") * sqrt(252) * 100,
                type = portfolio_name
              ) %>%
              filter(!is.na(volatility), is.finite(volatility)) %>%
              select(date, volatility, type)
            
            if (nrow(volatility_data) > 0) {
              all_volatility[[portfolio_name]] <- volatility_data
            }
          }
        }
      }
    }
  }
  
  return(all_volatility)
}

#' Calculate drawdowns
calculate_drawdowns <- function(data) {
  all_drawdown <- list()
  
  # Portfolio drawdowns
  if (!is.null(data$portfolios) && length(data$portfolios) > 0) {
    for (portfolio_name in names(data$portfolios)) {
      portfolio_info <- data$portfolios[[portfolio_name]]
      
      if (!is.null(portfolio_info$cumulative_returns) && 
          length(portfolio_info$cumulative_returns) > 1) {
        
        cumulative_returns <- as.numeric(portfolio_info$cumulative_returns) + 1  # Convert to price series
        dates <- as.Date(portfolio_info$dates)
        
        valid_indices <- is.finite(cumulative_returns) & !is.na(dates)
        cumulative_returns <- cumulative_returns[valid_indices]
        dates <- dates[valid_indices]
        
        if (length(cumulative_returns) > 1) {
          running_max <- cummax(cumulative_returns)
          drawdown <- (cumulative_returns / running_max - 1) * 100
          
          drawdown_data <- data.frame(
            date = dates,
            drawdown = drawdown,
            type = portfolio_name,
            stringsAsFactors = FALSE
          ) %>%
            filter(is.finite(drawdown))
          
          if (nrow(drawdown_data) > 0) {
            all_drawdown[[portfolio_name]] <- drawdown_data
          }
        }
      }
    }
  }
  
  return(all_drawdown)
}

#' Calculate comprehensive risk metrics
calculate_risk_metrics <- function(data) {
  metrics_list <- list()
  
  # Calculate for portfolios
  if (!is.null(data$portfolios) && length(data$portfolios) > 0) {
    for (portfolio_name in names(data$portfolios)) {
      portfolio_info <- data$portfolios[[portfolio_name]]
      
      if (!is.null(portfolio_info$cumulative_returns) && 
          length(portfolio_info$cumulative_returns) > 5) {
        
        cumulative_returns <- as.numeric(portfolio_info$cumulative_returns)
        
        # Calculate daily returns
        daily_returns <- c(0, diff(log(cumulative_returns + 1)))
        daily_returns <- daily_returns[is.finite(daily_returns) & !is.na(daily_returns)]
        
        if (length(daily_returns) > 5) {
          # Basic metrics
          vol_annual <- sd(daily_returns, na.rm = TRUE) * sqrt(252)
          downside_returns <- daily_returns[daily_returns < 0]
          downside_dev <- if(length(downside_returns) > 0) {
            sd(downside_returns) * sqrt(252)
          } else {
            0
          }
          
          # VaR and CVaR
          var_95 <- quantile(daily_returns, 0.05, na.rm = TRUE)
          cvar_95 <- mean(daily_returns[daily_returns <= var_95], na.rm = TRUE)
          
          # Max drawdown
          cumulative_prices <- cumprod(1 + daily_returns)
          running_max <- cummax(cumulative_prices)
          drawdown <- (cumulative_prices / running_max - 1)
          max_dd <- min(drawdown, na.rm = TRUE)
          
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
  }
  
  # Add benchmark metrics
  if (!is.null(data$sp500) && !is.null(data$sp500$cumulative_returns)) {
    cumulative_returns <- as.numeric(data$sp500$cumulative_returns)
    daily_returns <- c(0, diff(log(cumulative_returns + 1)))
    daily_returns <- daily_returns[is.finite(daily_returns) & !is.na(daily_returns)]
    
    if (length(daily_returns) > 5) {
      vol_annual <- sd(daily_returns, na.rm = TRUE) * sqrt(252)
      downside_returns <- daily_returns[daily_returns < 0]
      downside_dev <- if(length(downside_returns) > 0) {
        sd(downside_returns) * sqrt(252)
      } else {
        0
      }
      
      var_95 <- quantile(daily_returns, 0.05, na.rm = TRUE)
      cvar_95 <- mean(daily_returns[daily_returns <= var_95], na.rm = TRUE)
      
      cumulative_prices <- cumprod(1 + daily_returns)
      running_max <- cummax(cumulative_prices)
      drawdown <- (cumulative_prices / running_max - 1)
      max_dd <- min(drawdown, na.rm = TRUE)
      
      metrics_list[["S&P 500"]] <- data.frame(
        Portfolio = "S&P 500",
        Volatility = vol_annual,
        Downside_Deviation = downside_dev,
        VaR_95 = var_95,
        CVaR_95 = cvar_95,
        Max_Drawdown = max_dd,
        stringsAsFactors = FALSE
      )
    }
  }
  
  if (!is.null(data$bitcoin) && !is.null(data$bitcoin$cumulative_returns)) {
    cumulative_returns <- as.numeric(data$bitcoin$cumulative_returns)
    daily_returns <- c(0, diff(log(cumulative_returns + 1)))
    daily_returns <- daily_returns[is.finite(daily_returns) & !is.na(daily_returns)]
    
    if (length(daily_returns) > 5) {
      vol_annual <- sd(daily_returns, na.rm = TRUE) * sqrt(252)
      downside_returns <- daily_returns[daily_returns < 0]
      downside_dev <- if(length(downside_returns) > 0) {
        sd(downside_returns) * sqrt(252)
      } else {
        0
      }
      
      var_95 <- quantile(daily_returns, 0.05, na.rm = TRUE)
      cvar_95 <- mean(daily_returns[daily_returns <= var_95], na.rm = TRUE)
      
      cumulative_prices <- cumprod(1 + daily_returns)
      running_max <- cummax(cumulative_prices)
      drawdown <- (cumulative_prices / running_max - 1)
      max_dd <- min(drawdown, na.rm = TRUE)
      
      metrics_list[["Bitcoin"]] <- data.frame(
        Portfolio = "Bitcoin",
        Volatility = vol_annual,
        Downside_Deviation = downside_dev,
        VaR_95 = var_95,
        CVaR_95 = cvar_95,
        Max_Drawdown = max_dd,
        stringsAsFactors = FALSE
      )
    }
  }
  
  if (length(metrics_list) > 0) {
    return(bind_rows(metrics_list))
  } else {
    return(data.frame())
  }
}