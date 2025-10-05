performanceUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = "Selection Overview",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        uiOutput(ns("selection_summary")),
        uiOutput(ns("comparison_controls"))
      )
    ),
    fluidRow(
      box(
        title = "Portfolio Performance vs Benchmarks",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        plotlyOutput(ns("performance_plot"), height = "520px")
      )
    ),
    fluidRow(
      box(
        title = "Performance Metrics",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        DT::dataTableOutput(ns("metrics_table"))
      )
    ),
    fluidRow(
      box(
        title = "Rebalance Impact",
        status = "warning",
        solidHeader = TRUE,
        width = 12,
        DT::dataTableOutput(ns("rebalance_table"))
      )
    )
  )
}

performanceServer <- function(id, selection_state, portfolio_data, portfolio_metadata, comparison_state, set_comparisons) {
  moduleServer(id, function(input, output, session) {

    selection_metadata <- reactive({
      sel <- selection_state()
      meta <- sel$metadata
      if (is.null(meta)) tibble() else meta
    })

    active_groups <- reactive({
      sel <- selection_state()
      groups <- sel$selected_groups
      if (is.null(groups)) character() else groups
    })

    output$selection_summary <- renderUI({
      sel <- selection_state()
      meta <- selection_metadata()
      groups <- active_groups()

      if (nrow(meta) == 0) {
        return(tags$p("No portfolios loaded."))
      }

      if (length(groups) == 0) {
        return(tags$p("Select one or more portfolios in the sidebar."))
      }

      date_range <- sel$date_range
      range_text <- NULL
      if (!is.null(date_range) && length(date_range) == 2 && all(!is.na(date_range))) {
        range_text <- sprintf("Showing %s to %s", format(date_range[1]), format(date_range[2]))
      }

      # Check if is_stitched column exists, if not assume all are not stitched
      if ("is_stitched" %in% names(meta)) {
        latest_labels <- meta %>%
          dplyr::filter(portfolio_name %in% groups, is_stitched == TRUE) %>%
          dplyr::arrange(portfolio_name) %>%
          dplyr::transmute(label = sprintf("%s (All versions combined)", portfolio_name)) %>%
          dplyr::pull(label)
      } else {
        latest_labels <- meta %>%
          dplyr::filter(portfolio_name %in% groups) %>%
          dplyr::arrange(portfolio_name, dplyr::desc(start_date)) %>%
          dplyr::group_by(portfolio_name) %>%
          dplyr::slice_head(n = 1) %>%
          dplyr::ungroup() %>%
          dplyr::transmute(label = sprintf("%s - Latest", portfolio_name)) %>%
          dplyr::pull(label)
      }

      tags$div(
        class = "selection-summary",
        tags$p(sprintf("Selected portfolios: %s", paste(groups, collapse = ", "))),
        if (!is.null(range_text)) tags$p(range_text),
        tags$ul(lapply(latest_labels, tags$li))
      )
    })

    available_snapshots <- reactive({
      meta <- selection_metadata()
      groups <- active_groups()
      if (nrow(meta) == 0 || length(groups) == 0) {
        return(tibble())
      }
      # Only show individual versions (not stitched) for comparison
      if ("is_stitched" %in% names(meta)) {
        meta %>%
          dplyr::filter(portfolio_name %in% groups, is_stitched == FALSE) %>%
          dplyr::arrange(portfolio_name, dplyr::desc(start_date))
      } else {
        meta %>%
          dplyr::filter(portfolio_name %in% groups) %>%
          dplyr::arrange(portfolio_name, dplyr::desc(start_date))
      }
    })

    output$comparison_controls <- renderUI({
      snapshots <- available_snapshots()
      sel <- selection_state()

      if (nrow(snapshots) == 0) {
        return(tags$div(class = "comparison-controls", tags$p("No historical versions available for what-if comparison.")))
      }

      choices <- stats::setNames(
        snapshots$key,
        sprintf("%s  -  %s (what-if)", snapshots$portfolio_name, snapshots$version_label)
      )

      tags$div(
        class = "comparison-controls",
        selectizeInput(
          session$ns("comparison_versions"),
          label = "Add individual versions for what-if comparison",
          choices = choices,
          selected = sel$comparison_ids,
          multiple = TRUE,
          options = list(placeholder = "Choose versions to compare", plugins = list("remove_button"))
        ),
        actionLink(session$ns("clear_comparisons"), "Clear comparisons")
      )
    })

    observeEvent(input$comparison_versions, {
      set_comparisons(input$comparison_versions)
    }, ignoreNULL = FALSE)

    observeEvent(input$clear_comparisons, {
      set_comparisons(character())
      updateSelectizeInput(session, "comparison_versions", selected = character(), server = TRUE)
    })

    observe({
      sel <- selection_state()
      if (is.null(sel$comparison_ids)) {
        updateSelectizeInput(session, "comparison_versions", selected = character(), server = TRUE)
      } else {
        updateSelectizeInput(session, "comparison_versions", selected = sel$comparison_ids, server = TRUE)
      }
    })

    filter_series_by_range <- function(series, date_range) {
      if (is.null(series) || is.null(date_range) || length(date_range) != 2 || any(is.na(date_range))) {
        return(series)
      }
      start_date <- as.Date(date_range[1])
      end_date <- as.Date(date_range[2])
      keep <- series$dates >= start_date & series$dates <= end_date
      if (!any(keep)) {
        return(NULL)
      }
      out <- series
      out$dates <- as.Date(series$dates[keep])
      out$cumulative_returns <- as.numeric(series$cumulative_returns[keep])
      if (!is.null(series$portfolio_tbl)) {
        out$portfolio_tbl <- series$portfolio_tbl %>% dplyr::filter(date >= start_date, date <= end_date)
      }
      if (!is.null(series$individual_stocks)) {
        out$individual_stocks <- series$individual_stocks %>% dplyr::filter(date >= start_date, date <= end_date)
      }
      out
    }

    filtered_data <- reactive({
      data <- portfolio_data()
      if (is.null(data)) {
        return(NULL)
      }
      sel <- selection_state()
      date_range <- sel$date_range
      if (is.null(date_range) || length(date_range) != 2 || any(is.na(date_range))) {
        return(data)
      }
      filtered <- data
      filtered$portfolios <- lapply(data$portfolios, filter_series_by_range, date_range = date_range)
      filtered$portfolios <- Filter(Negate(is.null), filtered$portfolios)
      if (!is.null(data$sp500)) filtered$sp500 <- filter_series_by_range(data$sp500, date_range) else filtered$sp500 <- NULL
      if (!is.null(data$bitcoin)) filtered$bitcoin <- filter_series_by_range(data$bitcoin, date_range) else filtered$bitcoin <- NULL
      filtered
    })

    build_trace_df <- function(data, keys, meta, type_label) {
      if (length(keys) == 0 || is.null(data)) return(tibble())
      purrr::map_dfr(keys, function(key) {
        series <- data$portfolios[[key]]
        if (is.null(series) || length(series$dates) == 0) return(tibble())
        label_row <- meta %>% dplyr::filter(key == !!key) %>% dplyr::slice_head(n = 1)
        portfolio_label <- if (nrow(label_row) > 0) label_row$portfolio_name[1] else key
        version_label <- if (nrow(label_row) > 0) label_row$version_label[1] else "Latest"
        is_stitched <- if (nrow(label_row) > 0 && "is_stitched" %in% names(label_row)) isTRUE(label_row$is_stitched[1]) else FALSE

        # For stitched portfolios, show just the portfolio name
        # For individual versions, show portfolio name + version
        display <- if (is_stitched) {
          portfolio_label
        } else if (type_label == "current") {
          sprintf("%s  -  Current", portfolio_label)
        } else {
          sprintf("%s  -  %s", portfolio_label, version_label)
        }

        tibble::tibble(
          date = as.Date(series$dates),
          return_pct = as.numeric(series$cumulative_returns) * 100,
          portfolio = display,
          series_type = type_label,
          is_stitched = is_stitched
        )
      })
    }

    output$performance_plot <- renderPlotly({
      data_filtered <- filtered_data()
      data_full <- portfolio_data()
      if (is.null(data_filtered) || is.null(data_full)) return(plotly_empty("No data available"))

      sel <- selection_state()
      meta <- selection_metadata()
      if (nrow(meta) == 0) return(plotly_empty("No data available"))

      # Debug output
      cat("Active IDs for plot:", paste(sel$active_ids, collapse=", "), "\n")
      cat("Available portfolio keys in data:", paste(names(data_filtered$portfolios), collapse=", "), "\n")

      current_df <- build_trace_df(data_filtered, sel$active_ids, meta, "current")
      snapshot_df <- build_trace_df(data_filtered, sel$comparison_ids, meta, "snapshot")

      cat("Current df rows:", nrow(current_df), "\n")
      cat("Snapshot df rows:", nrow(snapshot_df), "\n")

      benchmark_traces <- tibble::tibble()
      if (!is.null(data_filtered$sp500)) {
        benchmark_traces <- dplyr::bind_rows(benchmark_traces, tibble::tibble(
          date = as.Date(data_filtered$sp500$dates),
          return_pct = as.numeric(data_filtered$sp500$cumulative_returns) * 100,
          portfolio = "S&P 500",
          series_type = "benchmark"
        ))
      }
      if (!is.null(data_filtered$bitcoin)) {
        benchmark_traces <- dplyr::bind_rows(benchmark_traces, tibble::tibble(
          date = as.Date(data_filtered$bitcoin$dates),
          return_pct = as.numeric(data_filtered$bitcoin$cumulative_returns) * 100,
          portfolio = "Bitcoin",
          series_type = "benchmark"
        ))
      }

      plot_df <- dplyr::bind_rows(current_df, snapshot_df, benchmark_traces)
      if (nrow(plot_df) == 0) return(plotly_empty("No data available"))

      p <- plotly::plot_ly()

      if (nrow(current_df) > 0) {
        split_current <- split(current_df, current_df$portfolio)
        for (name in names(split_current)) {
          series <- split_current[[name]]
          # Stitched portfolios get solid line, individual versions get dash
          line_style <- if (isTRUE(series$is_stitched[1])) {
            list(width = 3)
          } else {
            list(width = 2, dash = "dash")
          }
          p <- plotly::add_lines(
            p,
            data = series,
            x = ~date,
            y = ~return_pct,
            name = name,
            line = line_style
          )
        }
      }

      if (nrow(snapshot_df) > 0) {
        split_snapshots <- split(snapshot_df, snapshot_df$portfolio)
        for (name in names(split_snapshots)) {
          series <- split_snapshots[[name]]
          # Snapshots always get dashed line for comparison
          p <- plotly::add_lines(
            p,
            data = series,
            x = ~date,
            y = ~return_pct,
            name = name,
            line = list(width = 2, dash = "dash")
          )
        }
      }

      if (nrow(benchmark_traces) > 0) {
        split_bench <- split(benchmark_traces, benchmark_traces$portfolio)
        for (name in names(split_bench)) {
          series <- split_bench[[name]]
          p <- plotly::add_lines(
            p,
            data = series,
            x = ~date,
            y = ~return_pct,
            name = name,
            line = list(width = 1.5, dash = "dot")
          )
        }
      }

      p %>%
        plotly::layout(
          xaxis = list(title = "Date"),
          yaxis = list(title = "Cumulative Return (%)"),
          hovermode = "x unified",
          legend = list(orientation = "h", x = 0, y = 1.05)
        )
    })

    active_data <- reactive({
      tryCatch({
        data <- filtered_data()
        if (is.null(data) || length(data$portfolios) == 0) NULL else data
      }, error = function(e) NULL)
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

    compute_horizon_return <- function(series, start_date, horizon_days) {
      if (is.null(series) || length(series$dates) == 0) return(NA_real_)
      dates <- as.Date(series$dates)
      cumulative <- as.numeric(series$cumulative_returns)

      # Filter to dates from start_date onwards
      valid_idx <- dates >= start_date
      if (!any(valid_idx)) return(NA_real_)

      dates_filtered <- dates[valid_idx]
      cumulative_filtered <- cumulative[valid_idx]

      # Check if we have enough data for the horizon
      if (max(dates_filtered, na.rm = TRUE) < start_date + horizon_days) return(NA_real_)

      # Get cumulative return at start and at horizon end
      # Normalize so start_date has return = 0
      start_return <- cumulative_filtered[1]  # Return at start date
      prices <- 1 + (cumulative_filtered - start_return)  # Normalize to start at 1

      # Get price at start and end of horizon
      start_price <- 1.0  # By normalization
      end_date_numeric <- as.numeric(start_date + horizon_days)
      end_price <- approx(as.numeric(dates_filtered), prices, end_date_numeric, method = "linear", rule = 2)$y

      if (is.na(end_price)) return(NA_real_)
      end_price / start_price - 1
    }

    rebalance_summary <- reactive({
      meta <- selection_metadata()
      groups <- active_groups()
      data <- portfolio_data()
      if (nrow(meta) == 0 || length(groups) == 0 || is.null(data)) {
        return(tibble())
      }

      horizons <- c(`1M` = 30, `3M` = 90, `6M` = 180, `12M` = 365)
      summaries <- list()

      for (group in groups) {
        group_meta <- meta %>%
          dplyr::filter(portfolio_name == group, is_stitched == FALSE) %>%
          dplyr::arrange(start_date)
        if (nrow(group_meta) == 0) next

        previous_metrics <- NULL

        for (i in seq_len(nrow(group_meta))) {
          row <- group_meta[i, ]
          series <- data$portfolios[[row$key]]
          if (is.null(series)) next

          horizon_returns <- vapply(horizons, function(days) compute_horizon_return(series, row$start_date, days), numeric(1))
          delta_3m <- if (!is.null(previous_metrics)) horizon_returns[["3M"]] - previous_metrics[["3M"]] else NA_real_
          delta_6m <- if (!is.null(previous_metrics)) horizon_returns[["6M"]] - previous_metrics[["6M"]] else NA_real_

          summaries[[length(summaries) + 1]] <- tibble::tibble(
            Portfolio = group,
            Version = row$version_label,
            Rebalance = format(row$start_date, "%Y-%m-%d"),
            `1M Return (%)` = horizon_returns[["1M"]] * 100,
            `3M Return (%)` = horizon_returns[["3M"]] * 100,
            `? vs Prev 3M (%)` = delta_3m * 100,
            `6M Return (%)` = horizon_returns[["6M"]] * 100,
            `? vs Prev 6M (%)` = delta_6m * 100,
            `12M Return (%)` = horizon_returns[["12M"]] * 100
          )

          previous_metrics <- horizon_returns
        }
      }

      if (length(summaries) == 0) {
        tibble()
      } else {
        dplyr::bind_rows(summaries)
      }
    })

    output$rebalance_table <- DT::renderDataTable({
      summary_df <- rebalance_summary()
      if (nrow(summary_df) == 0) {
        return(DT::datatable(
          data.frame(Message = "No rebalance statistics available"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      DT::datatable(
        summary_df,
        options = list(pageLength = 10, order = list(list(0, 'asc'), list(2, 'desc'))),
        rownames = FALSE
      ) %>%
        DT::formatPercentage(c('1M Return (%)', '3M Return (%)', '? vs Prev 3M (%)', '6M Return (%)', '? vs Prev 6M (%)', '12M Return (%)'), 2)
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

