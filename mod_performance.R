
performanceUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = "Selection Overview", status = "primary", solidHeader = TRUE, width = 12,
        uiOutput(ns("selection_summary"))
      )
    ),
    fluidRow(
      box(
        title = "Portfolio Performance vs Benchmarks", status = "primary", solidHeader = TRUE, width = 12,
        plotlyOutput(ns("performance_plot"), height = "500px")
      )
    ),
    fluidRow(
      box(
        title = "Performance Metrics Comparison", status = "info", solidHeader = TRUE, width = 12,
        DT::dataTableOutput(ns("metrics_table"))
      )
    )
  )
}

performanceServer <- function(id, selection_state, portfolio_data) {
  moduleServer(id, function(input, output, session) {

    selection_metadata <- reactive({
      sel <- selection_state()
      meta <- sel$metadata
      if (is.null(meta)) tibble() else meta
    })

    output$selection_summary <- renderUI({
      sel <- selection_state()
      meta <- selection_metadata()
      if (nrow(meta) == 0) {
        return(tags$p("No portfolios loaded."))
      }

      group <- sel$selected_group
      versions <- sel$selected_ids

      if (is.null(group) || !group %in% meta$portfolio_name) {
        return(tags$p("Select a portfolio in the sidebar."))
      }

      group_meta <- meta %>%
        dplyr::filter(portfolio_name == group) %>%
        dplyr::arrange(dplyr::desc(start_date))

      if (nrow(group_meta) == 0) {
        return(tags$p("No versions available for the selected portfolio."))
      }

      if (is.null(versions) || length(versions) == 0) {
        latest <- group_meta$version_label[1]
        return(tags$p(sprintf("Select one or more versions to compare. Latest available: %s", latest)))
      }

      selected_meta <- group_meta %>%
        dplyr::filter(key %in% versions) %>%
        dplyr::arrange(dplyr::desc(start_date))

      data <- tryCatch(portfolio_data(), error = function(e) NULL)

      version_items <- lapply(seq_len(nrow(selected_meta)), function(i) {
        key <- selected_meta$key[i]
        label <- selected_meta$version_label[i]
        return_pct <- NA_real_

        if (!is.null(data) && !is.null(data$portfolios[[key]])) {
          returns <- data$portfolios[[key]]$cumulative_returns
          if (!is.null(returns) && length(returns) > 0) {
            return_pct <- tail(returns, 1) * 100
          }
        }

        value_text <- ifelse(
          is.na(return_pct),
          "Latest cumulative return unavailable",
          sprintf("Latest cumulative return: %.2f%%", return_pct)
        )
        tags$li(sprintf("%s - %s", label, value_text))
      })

      tags$div(
        class = "selection-summary",
        tags$p(sprintf("Selected portfolio: %s", group)),
        tags$p(sprintf("Versions selected: %d", length(versions))),
        tags$ul(version_items)
      )
    })

    active_data <- reactive({
      tryCatch({
        data <- portfolio_data()
        if (is.null(data) || length(data$portfolios) == 0) NULL else data
      }, error = function(e) NULL)
    })

    output$performance_plot <- renderPlotly({
      data <- active_data()
      if (is.null(data)) return(plotly_empty())

      sel <- selection_state()
      meta <- selection_metadata()

      selected_keys <- names(data$portfolios)
      if (length(selected_keys) == 0) return(plotly_empty())

      selected_meta <- meta %>% dplyr::filter(key %in% selected_keys)

      portfolio_traces <- purrr::map_dfr(selected_keys, function(name) {
        series <- data$portfolios[[name]]
        if (is.null(series) || length(series$dates) == 0) {
          return(tibble::tibble(date = as.Date(character()), return_pct = numeric(), portfolio = character()))
        }
        start_date <- selected_meta$start_date[selected_meta$key == name]
        keep_idx <- if (length(start_date) == 0 || is.na(start_date[1])) {
          rep(TRUE, length(series$dates))
        } else {
          series$dates >= start_date[1]
        }
        tibble::tibble(
          date = series$dates[keep_idx],
          return_pct = as.numeric(series$cumulative_returns[keep_idx]) * 100,
          portfolio = name
        )
      })

      if (nrow(portfolio_traces) == 0) return(plotly_empty())

      earliest_start <- suppressWarnings(min(selected_meta$start_date, na.rm = TRUE))
      if (!is.finite(earliest_start)) {
        earliest_start <- min(portfolio_traces$date, na.rm = TRUE)
      }

      # Helper function to recalculate benchmark returns from earliest_start
      recalc_benchmark <- function(benchmark_data, name) {
        if (is.null(benchmark_data)) return(NULL)

        keep_idx <- benchmark_data$dates >= earliest_start
        if (!any(keep_idx)) return(NULL)

        filtered_dates <- benchmark_data$dates[keep_idx]
        filtered_returns <- benchmark_data$cumulative_returns[keep_idx]

        # Recalculate returns to start at 0% from earliest_start
        # Convert cumulative returns back to prices, then recalculate from first filtered date
        if (length(filtered_returns) > 0) {
          prices <- 1 + filtered_returns
          baseline_price <- prices[1]
          recalc_returns <- (prices / baseline_price) - 1

          tibble::tibble(
            date = filtered_dates,
            return_pct = as.numeric(recalc_returns) * 100,
            portfolio = name
          )
        } else {
          NULL
        }
      }

      benchmark_traces <- dplyr::bind_rows(
        recalc_benchmark(data$sp500, "S&P 500"),
        recalc_benchmark(data$bitcoin, "Bitcoin")
      )

      plot_df <- dplyr::bind_rows(portfolio_traces, benchmark_traces)

      plotly::plot_ly(plot_df, x = ~date, y = ~return_pct, color = ~portfolio, type = "scatter", mode = "lines") %>%
        plotly::layout(
          xaxis = list(title = "Date"),
          yaxis = list(title = "Cumulative Return (%)"),
          hovermode = "x unified",
          legend = list(orientation = "h", x = 0, y = 1.02)
        )
    })

    output$metrics_table <- DT::renderDataTable({
      data <- active_data()
      if (is.null(data)) {
        return(DT::datatable(
          data.frame(Message = "No data available"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      metrics <- calculate_performance_metrics(data)
      if (is.null(metrics) || nrow(metrics) == 0) {
        return(DT::datatable(
          data.frame(Message = "No metrics available"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      DT::datatable(metrics, options = list(dom = 't'), rownames = FALSE) %>%
        DT::formatPercentage(c("Total_Return", "Volatility", "Max_Drawdown"), 2) %>%
        DT::formatRound("Sharpe_Ratio", 2)
    })
  })
}

calculate_max_drawdown <- function(returns) {
  if (is.null(returns) || length(returns) < 2) return(0)
  prices <- 1 + returns
  running_max <- cummax(prices)
  drawdown <- (prices / running_max - 1)
  min(drawdown, na.rm = TRUE)
}

calculate_performance_metrics <- function(data) {
  metrics_list <- list()

  process_series <- function(name, series) {
    if (is.null(series) || length(series$cumulative_returns) < 2) return(NULL)
    total_ret <- tail(series$cumulative_returns, 1)

    daily_ret <- diff(log1p(series$cumulative_returns))
    daily_ret <- daily_ret[!is.na(daily_ret)]

    if (length(daily_ret) < 2) return(NULL)

    vol <- sd(daily_ret, na.rm = TRUE) * sqrt(252)
    sharpe <- if (vol > 0) (mean(daily_ret, na.rm = TRUE) * 252) / vol else 0
    max_dd <- calculate_max_drawdown(series$cumulative_returns)

    tibble(
      Portfolio = name,
      Total_Return = total_ret,
      Volatility = vol,
      Sharpe_Ratio = sharpe,
      Max_Drawdown = max_dd
    )
  }

  for (name in names(data$portfolios)) {
    metrics_list[[name]] <- process_series(name, data$portfolios[[name]])
  }

  if (!is.null(data$sp500)) {
    metrics_list[["S&P 500"]] <- process_series("S&P 500", data$sp500)
  }

  if (!is.null(data$bitcoin)) {
    metrics_list[["Bitcoin"]] <- process_series("Bitcoin", data$bitcoin)
  }

  bind_rows(metrics_list)
}


