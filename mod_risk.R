# mod_risk.R - Fixed Risk Metrics Module

#' Risk Module UI
#' @param id Module namespace ID
riskUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "Selection Summary",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        uiOutput(ns("selection_summary"))
      )
    ),
    fluidRow(
      box(
        title = "Risk Alerts",
        status = "warning",
        solidHeader = TRUE,
        width = 12,
        uiOutput(ns("risk_alerts"))
      )
    ),
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
#' @param selection_state Reactive selection state shared by the app
#' @param portfolio_data Reactive providing shared portfolio calculations

riskServer <- function(id, selection_state, portfolio_data) {
  moduleServer(id, function(input, output, session) {
    current_selection <- reactive({
      sel <- selection_state()
      if (is.null(sel)) {
        list(
          selected_ids = character(),
          selected_groups = character(),
          selected_group = NULL,
          metadata = tibble(),
          show_sp500 = TRUE,
          show_btc = TRUE,
          date_range = NULL
        )
      } else {
        sel
      }
    })

    output$selection_summary <- renderUI({
      sel <- current_selection()
      ids <- sel$selected_ids
      metadata <- sel$metadata

      if (is.null(metadata) || nrow(metadata) == 0) {
        return(tags$p("No portfolios loaded."))
      }

      if (is.null(ids) || length(ids) == 0) {
        return(tags$p("Select at least one portfolio in the sidebar."))
      }

      selected_meta <- metadata %>%
        dplyr::filter(key %in% ids) %>%
        dplyr::arrange(dplyr::desc(start_date))

      if (nrow(selected_meta) == 0) {
        return(tags$p("Selection unavailable; choose a different portfolio."))
      }

      date_range <- sel$date_range
      range_text <- NULL
      if (!is.null(date_range) && length(date_range) == 2 && all(!is.na(date_range))) {
        range_text <- sprintf("Window: %s to %s", format(date_range[1]), format(date_range[2]))
      }

      group_labels <- selected_meta %>%
        dplyr::distinct(portfolio_name) %>%
        dplyr::pull(portfolio_name)
      group_text <- if (length(group_labels) > 0) sprintf("Portfolios: %s", paste(group_labels, collapse = ", ")) else NULL

      version_items <- lapply(seq_len(nrow(selected_meta)), function(i) {
        tags$li(sprintf("%s - %s", selected_meta$portfolio_name[i], selected_meta$version_label[i]))
      })

      tags$div(
        class = "selection-summary",
        if (!is.null(group_text)) tags$p(group_text) else NULL,
        if (!is.null(range_text)) tags$p(range_text) else NULL,
        tags$ul(version_items)
      )
    })

    active_data <- reactive({
      data <- tryCatch(portfolio_data(), error = function(e) NULL)
      sel <- current_selection()
      metadata <- sel$metadata
      ids <- sel$selected_ids

      if (is.null(data) || is.null(ids) || length(ids) == 0) {
        return(NULL)
      }

      if (is.null(metadata) || nrow(metadata) == 0) {
        trimmed <- data
      } else {
        trimmed <- tryCatch(trim_portfolio_results(data, metadata), error = function(e) data)
      }

      apply_date_window(trimmed, sel$date_range)
    })

    risk_metrics <- reactive({
      data <- active_data()
      if (is.null(data)) return(data.frame())

      tryCatch({
        metrics <- calculate_risk_metrics(data)
        if (is.null(metrics) || nrow(metrics) == 0) data.frame() else metrics
      }, error = function(e) {
        warning(paste("Risk metrics calculation error:", e$message))
        data.frame()
      })
    })

    output$risk_alerts <- renderUI({
      metrics <- risk_metrics()
      sel <- current_selection()
      focus <- sel$selected_ids

      if (is.null(metrics) || nrow(metrics) == 0) {
        return(tags$p("No risk metrics available yet."))
      }

      focus_metrics <- metrics %>% dplyr::filter(Portfolio %in% focus)
      if (nrow(focus_metrics) == 0) {
        focus_metrics <- metrics %>% dplyr::filter(!(Portfolio %in% c("S&P 500", "Bitcoin")))
      }

      if (nrow(focus_metrics) == 0) {
        focus_metrics <- metrics
      }

      alerts <- list()

      for (i in seq_len(nrow(focus_metrics))) {
        row <- focus_metrics[i, ]
        label <- row$Portfolio

        if (!is.na(row$Max_Drawdown) && row$Max_Drawdown <= -0.2) {
          alerts[[length(alerts) + 1]] <- tags$li(sprintf("%s max drawdown is %.1f%% (monitor recovery plans).", label, row$Max_Drawdown * 100))
        }
        if (!is.na(row$VaR_95) && row$VaR_95 <= -0.03) {
          alerts[[length(alerts) + 1]] <- tags$li(sprintf("%s 95%% VaR is %.1f%% (tail risk elevated).", label, row$VaR_95 * 100))
        }
        if (!is.na(row$Volatility) && row$Volatility >= 0.25) {
          alerts[[length(alerts) + 1]] <- tags$li(sprintf("%s annualised volatility is %.1f%% (high volatility regime).", label, row$Volatility * 100))
        }
        if (!is.na(row$Downside_Deviation) && row$Downside_Deviation >= 0.18) {
          alerts[[length(alerts) + 1]] <- tags$li(sprintf("%s downside deviation is %.1f%% (drawdowns becoming more persistent).", label, row$Downside_Deviation * 100))
        }
      }

      if (length(alerts) == 0) {
        tags$p("No alerts triggered for the selected portfolios.")
      } else {
        tags$ul(alerts)
      }
    })

    output$returns_distribution <- renderPlotly({
      data <- active_data()
      if (is.null(data)) return(plotly_empty("No data available"))

      tryCatch({
        all_returns <- calculate_all_returns(data)

        if (length(all_returns) == 0 || nrow(dplyr::bind_rows(all_returns)) == 0) {
          return(plotly_empty("No returns data available"))
        }

        combined_returns <- dplyr::bind_rows(all_returns)

        plotly::plot_ly(combined_returns, x = ~daily_return, color = ~type, type = "histogram",
          alpha = 0.7, histnorm = "probability density") %>%
          plotly::layout(
            title = "Daily Returns Distribution",
            xaxis = list(title = "Daily Return (%)"),
            yaxis = list(title = "Density"),
            barmode = "overlay"
          )
      }, error = function(e) {
        plotly_empty(paste("Returns distribution error:", e$message))
      })
    })

    output$volatility_plot <- renderPlotly({
      data <- active_data()
      if (is.null(data)) return(plotly_empty("No data available"))

      tryCatch({
        all_volatility <- calculate_volatility(data)

        if (length(all_volatility) == 0 || nrow(dplyr::bind_rows(all_volatility)) == 0) {
          return(plotly_empty("No volatility data available"))
        }

        combined_volatility <- dplyr::bind_rows(all_volatility)

        plotly::plot_ly(combined_volatility, x = ~date, y = ~volatility, color = ~type,
          type = "scatter", mode = "lines") %>%
          plotly::layout(
            title = "Rolling Volatility (30 days)",
            xaxis = list(title = "Date"),
            yaxis = list(title = "Annualised Volatility"),
            hovermode = "x unified"
          )
      }, error = function(e) {
        plotly_empty(paste("Volatility plot error:", e$message))
      })
    })

    output$drawdown_plot <- renderPlotly({
      data <- active_data()
      if (is.null(data)) return(plotly_empty("No data available"))

      tryCatch({
        drawdowns <- calculate_drawdowns(data)

        if (length(drawdowns) == 0 || nrow(dplyr::bind_rows(drawdowns)) == 0) {
          return(plotly_empty("No drawdown data available"))
        }

        combined_drawdowns <- dplyr::bind_rows(drawdowns)

        plotly::plot_ly(combined_drawdowns, x = ~date, y = ~drawdown, color = ~type,
          type = "scatter", mode = "lines") %>%
          plotly::layout(
            title = "Drawdown Analysis",
            xaxis = list(title = "Date"),
            yaxis = list(title = "Drawdown (%)"),
            hovermode = "x unified"
          )
      }, error = function(e) {
        plotly_empty(paste("Drawdown plot error:", e$message))
      })
    })

    output$risk_metrics_table <- DT::renderDataTable({
      metrics <- risk_metrics()
      if (is.null(metrics) || nrow(metrics) == 0) {
        return(DT::datatable(
          data.frame(Message = "No metrics available"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      DT::datatable(metrics, options = list(pageLength = 25, dom = 't'), rownames = FALSE) %>%
        DT::formatPercentage(c("Volatility", "Downside_Deviation", "VaR_95", "CVaR_95", "Max_Drawdown"), 2)
    })
  })
}


# Helper functions for risk calculations

compute_period_returns <- function(cumulative_returns) {
  if (is.null(cumulative_returns)) return(numeric(0))
  wealth <- 1 + as.numeric(cumulative_returns)
  if (length(wealth) < 2) return(numeric(0))
  wealth[-1] / wealth[-length(wealth)] - 1
}

trim_portfolio_results <- function(result, metadata) {
  if (is.null(result)) return(result)
  if (is.null(metadata) || nrow(metadata) == 0) return(result)

  metadata <- metadata %>%
    dplyr::filter(key %in% names(result$portfolios)) %>%
    dplyr::mutate(start_date = as.Date(start_date)) %>%
    dplyr::filter(!is.na(start_date))

  if (nrow(metadata) == 0) return(result)

  earliest_start <- min(metadata$start_date, na.rm = TRUE)

  # Trim portfolios
  for (portfolio_name in names(result$portfolios)) {
    portfolio_info <- result$portfolios[[portfolio_name]]
    start_row <- metadata %>% dplyr::filter(key == portfolio_name)
    if (nrow(start_row) == 0) next
    start_date <- as.Date(start_row$start_date[1])
    if (is.na(start_date)) next

    dates <- as.Date(portfolio_info$dates)
    keep <- dates >= start_date
    if (!any(keep, na.rm = TRUE)) next

    result$portfolios[[portfolio_name]]$dates <- dates[keep]
    result$portfolios[[portfolio_name]]$cumulative_returns <- portfolio_info$cumulative_returns[keep]

    if (!is.null(portfolio_info$individual_stocks)) {
      result$portfolios[[portfolio_name]]$individual_stocks <- portfolio_info$individual_stocks %>%
        dplyr::mutate(date = as.Date(date)) %>%
        dplyr::filter(date >= start_date)
    }
  }

  # Trim benchmarks to earliest selected start date
  trim_benchmark <- function(bench) {
    if (is.null(bench)) return(NULL)
    dates <- as.Date(bench$dates)
    keep <- dates >= earliest_start
    if (!any(keep, na.rm = TRUE)) return(NULL)
    bench$dates <- dates[keep]
    bench$cumulative_returns <- bench$cumulative_returns[keep]
    bench
  }

  result$sp500 <- trim_benchmark(result$sp500)
  result$bitcoin <- trim_benchmark(result$bitcoin)

  result
}

apply_date_window <- function(result, date_range) {
  if (is.null(result) || is.null(date_range) || length(date_range) != 2 || any(is.na(date_range))) {
    return(result)
  }

  start_date <- as.Date(date_range[1])
  end_date <- as.Date(date_range[2])

  filter_series <- function(series) {
    if (is.null(series) || is.null(series$dates)) return(NULL)
    dates <- as.Date(series$dates)
    keep <- dates >= start_date & dates <= end_date
    if (!any(keep)) return(NULL)

    series$dates <- dates[keep]
    if (!is.null(series$cumulative_returns)) {
      series$cumulative_returns <- as.numeric(series$cumulative_returns[keep])
    }
    if (!is.null(series$portfolio_tbl)) {
      series$portfolio_tbl <- series$portfolio_tbl %>% dplyr::filter(date >= start_date, date <= end_date)
    }
    if (!is.null(series$individual_stocks)) {
      series$individual_stocks <- series$individual_stocks %>% dplyr::filter(date >= start_date, date <= end_date)
    }
    series
  }

  if (!is.null(result$portfolios)) {
    result$portfolios <- lapply(result$portfolios, filter_series)
    result$portfolios <- Filter(Negate(is.null), result$portfolios)
  }

  result$sp500 <- filter_series(result$sp500)
  result$bitcoin <- filter_series(result$bitcoin)

  result
}


#' Calculate daily returns for all portfolios and benchmarks
calculate_all_returns <- function(data) {
  all_returns <- list()

  process_returns <- function(series, name) {
    if (is.null(series) || length(series$cumulative_returns) < 2) return(NULL)

    period_returns <- compute_period_returns(series$cumulative_returns)
    if (length(period_returns) == 0) return(NULL)

    dates <- as.Date(series$dates)
    dates <- dates[-1]

    returns_df <- tibble::tibble(
      date = dates,
      daily_return = period_returns * 100,
      type = name
    ) %>%
      dplyr::filter(is.finite(daily_return), !is.na(daily_return))

    if (nrow(returns_df) > 0) returns_df else NULL
  }

  if (!is.null(data$portfolios) && length(data$portfolios) > 0) {
    for (portfolio_name in names(data$portfolios)) {
      all_returns[[portfolio_name]] <- process_returns(data$portfolios[[portfolio_name]], portfolio_name)
    }
  }

  if (!is.null(data$sp500)) {
    all_returns[["S&P 500"]] <- process_returns(data$sp500, "S&P 500")
  }

  if (!is.null(data$bitcoin)) {
    all_returns[["Bitcoin"]] <- process_returns(data$bitcoin, "Bitcoin")
  }

  all_returns
}


#' Calculate rolling volatility


#' Calculate rolling volatility
calculate_volatility <- function(data, window = 30) {
  build_volatility <- function(series, name) {
    if (is.null(series) || length(series$cumulative_returns) <= window) return(NULL)

    period_returns <- compute_period_returns(series$cumulative_returns)
    if (length(period_returns) < window) return(NULL)

    dates <- as.Date(series$dates)
    dates <- dates[-1]

    vol_df <- tibble::tibble(
      date = dates,
      daily_return = period_returns
    ) %>%
      dplyr::filter(is.finite(daily_return)) %>%
      dplyr::arrange(date)

    if (nrow(vol_df) < window) return(NULL)

    vol_df <- vol_df %>%
      dplyr::mutate(
        volatility = zoo::rollapply(daily_return, width = window, FUN = sd, fill = NA, align = "right") * sqrt(252) * 100,
        type = name
      ) %>%
      dplyr::filter(!is.na(volatility), is.finite(volatility)) %>%
      dplyr::select(date, volatility, type)

    if (nrow(vol_df) > 0) vol_df else NULL
  }

  all_volatility <- list()

  if (!is.null(data$portfolios) && length(data$portfolios) > 0) {
    for (portfolio_name in names(data$portfolios)) {
      vol_df <- build_volatility(data$portfolios[[portfolio_name]], portfolio_name)
      if (!is.null(vol_df)) {
        all_volatility[[portfolio_name]] <- vol_df
      }
    }
  }

  if (!is.null(data$sp500)) {
    vol_df <- build_volatility(data$sp500, "S&P 500")
    if (!is.null(vol_df)) {
      all_volatility[["S&P 500"]] <- vol_df
    }
  }

  if (!is.null(data$bitcoin)) {
    vol_df <- build_volatility(data$bitcoin, "Bitcoin")
    if (!is.null(vol_df)) {
      all_volatility[["Bitcoin"]] <- vol_df
    }
  }

  all_volatility
}

#' Calculate drawdowns

#' Calculate drawdowns
calculate_drawdowns <- function(data) {
  build_drawdown <- function(series, name) {
    if (is.null(series) || length(series$cumulative_returns) < 2) return(NULL)

    wealth <- 1 + as.numeric(series$cumulative_returns)
    dates <- as.Date(series$dates)

    valid <- is.finite(wealth) & !is.na(dates)
    wealth <- wealth[valid]
    dates <- dates[valid]

    if (length(wealth) < 2) return(NULL)

    running_max <- cummax(wealth)
    drawdown <- (wealth / running_max - 1) * 100

    drawdown_df <- tibble::tibble(
      date = dates,
      drawdown = drawdown,
      type = name
    ) %>%
      dplyr::filter(is.finite(drawdown))

    if (nrow(drawdown_df) > 0) drawdown_df else NULL
  }

  all_drawdown <- list()

  if (!is.null(data$portfolios) && length(data$portfolios) > 0) {
    for (portfolio_name in names(data$portfolios)) {
      dd_df <- build_drawdown(data$portfolios[[portfolio_name]], portfolio_name)
      if (!is.null(dd_df)) {
        all_drawdown[[portfolio_name]] <- dd_df
      }
    }
  }

  if (!is.null(data$sp500)) {
    dd_df <- build_drawdown(data$sp500, "S&P 500")
    if (!is.null(dd_df)) {
      all_drawdown[["S&P 500"]] <- dd_df
    }
  }

  if (!is.null(data$bitcoin)) {
    dd_df <- build_drawdown(data$bitcoin, "Bitcoin")
    if (!is.null(dd_df)) {
      all_drawdown[["Bitcoin"]] <- dd_df
    }
  }

  all_drawdown
}

#' Calculate comprehensive risk metrics

#' Calculate comprehensive risk metrics
calculate_risk_metrics <- function(data) {
  metrics_list <- list()

  build_metrics <- function(series, label) {
    if (is.null(series) || length(series$cumulative_returns) < 2) return(NULL)

    period_returns <- compute_period_returns(series$cumulative_returns)
    period_returns <- period_returns[is.finite(period_returns) & !is.na(period_returns)]

    if (length(period_returns) <= 5) return(NULL)

    vol_annual <- stats::sd(period_returns, na.rm = TRUE) * sqrt(252)
    downside_returns <- period_returns[period_returns < 0]
    downside_dev <- if (length(downside_returns) > 0) {
      stats::sd(downside_returns, na.rm = TRUE) * sqrt(252)
    } else {
      0
    }

    var_95 <- stats::quantile(period_returns, 0.05, na.rm = TRUE, type = 7)
    cvar_95 <- mean(period_returns[period_returns <= var_95], na.rm = TRUE)

    cumulative_prices <- cumprod(1 + period_returns)
    running_max <- cummax(cumulative_prices)
    drawdown <- (cumulative_prices / running_max) - 1
    max_dd <- min(drawdown, na.rm = TRUE)

    dplyr::tibble(
      Portfolio = label,
      Volatility = vol_annual,
      Downside_Deviation = downside_dev,
      VaR_95 = var_95,
      CVaR_95 = cvar_95,
      Max_Drawdown = max_dd
    )
  }

  if (!is.null(data$portfolios) && length(data$portfolios) > 0) {
    for (portfolio_name in names(data$portfolios)) {
      metrics_list[[portfolio_name]] <- build_metrics(data$portfolios[[portfolio_name]], portfolio_name)
    }
  }

  if (!is.null(data$sp500)) {
    metrics_list[["S&P 500"]] <- build_metrics(data$sp500, "S&P 500")
  }

  if (!is.null(data$bitcoin)) {
    metrics_list[["Bitcoin"]] <- build_metrics(data$bitcoin, "Bitcoin")
  }

  metrics_list <- metrics_list[!vapply(metrics_list, is.null, logical(1))]

  if (length(metrics_list) > 0) {
    dplyr::bind_rows(metrics_list)
  } else {
    dplyr::tibble()
  }
}



