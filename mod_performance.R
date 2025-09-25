# mod_performance.R

performanceUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = "Portfolio Selection & Summary", status = "primary", solidHeader = TRUE, width = 12,
        fluidRow(
          column(4, selectInput(ns("portfolio_group"), "Portfolio:", choices = NULL)),
          column(8, checkboxGroupInput(ns("selected_versions"), "Versions to compare:", choices = NULL, inline = TRUE))
        ),
        uiOutput(ns("selection_summary"))
      )
    ),
    fluidRow(
      box(
        title = "Portfolio Performance vs BTC & S&P 500", status = "primary", solidHeader = TRUE, width = 12,
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

performanceServer <- function(id, portfolios_reactive, portfolio_calc) {
  moduleServer(id, function(input, output, session) {

    portfolio_metadata <- reactive({
      portfolios <- portfolios_reactive()
      if (length(portfolios) == 0) {
        return(tibble())
      }

      purrr::imap_dfr(portfolios, function(def, key) {
        tibble(
          key = key,
          portfolio_name = if (!is.null(def$portfolio_name)) def$portfolio_name else key,
          version_label = if (!is.null(def$version_label)) def$version_label else format(as.Date(def$start_date), "%Y-%m-%d"),
          start_date = as.Date(def$start_date)
        )
      }) %>%
        arrange(portfolio_name, start_date)
    })

    observe({
      meta <- portfolio_metadata()
      choices <- unique(meta$portfolio_name)

      if (length(choices) == 0) {
        updateSelectInput(session, "portfolio_group", choices = choices, selected = character(0))
        return()
      }

      selected <- input$portfolio_group
      if (is.null(selected) || !selected %in% choices) {
        preferred <- choices[grepl('^high risk$', choices, ignore.case = TRUE)]
        if (length(preferred) > 0) {
          selected <- preferred[1]
        } else {
          selected <- choices[1]
        }
      }

      updateSelectInput(session, "portfolio_group", choices = choices, selected = selected)
    })

    observeEvent(list(input$portfolio_group, portfolio_metadata()), {
      meta <- portfolio_metadata()
      group <- input$portfolio_group

      if (is.null(group) || nrow(meta) == 0 || !group %in% meta$portfolio_name) {
        updateCheckboxGroupInput(session, "selected_versions", choices = list(), selected = character(0))
        return()
      }

      group_meta <- meta %>%
        filter(portfolio_name == group) %>%
        arrange(desc(start_date))

      choices <- stats::setNames(group_meta$key, group_meta$version_label)
      defaults <- input$selected_versions
      if (is.null(defaults) || !all(defaults %in% group_meta$key)) {
        defaults <- head(group_meta$key, n = min(2, nrow(group_meta)))
      }

      updateCheckboxGroupInput(session, "selected_versions", choices = choices, selected = defaults)
    }, ignoreNULL = FALSE)

    selected_versions <- reactive({
      req(input$portfolio_group)
      req(input$selected_versions)
      input$selected_versions
    })

    portfolio_data <- reactive({
      selections <- selected_versions()
      req(length(selections) > 0)

      portfolio_calc(
        portfolios = portfolios_reactive(),
        selected_portfolios = selections,
        show_sp500 = TRUE,
        show_btc = TRUE
      )
    })

    output$performance_plot <- renderPlotly({
      data <- portfolio_data()
      if (is.null(data)) return(plotly_empty())

      meta <- portfolio_metadata()
      selected_keys <- names(data$portfolios)
      selected_meta <- meta %>%
        dplyr::filter(key %in% selected_keys)

      portfolio_traces <- purrr::map_dfr(selected_keys, function(name) {
        series <- data$portfolios[[name]]
        if (is.null(series) || length(series$dates) == 0) {
          return(tibble::tibble(date = as.Date(character()), return_pct = numeric(), portfolio = character()))
        }
        start_date <- selected_meta$start_date[selected_meta$key == name]
        if (length(start_date) == 0 || is.na(start_date)[1]) {
          keep_idx <- rep(TRUE, length(series$dates))
        } else {
          keep_idx <- series$dates >= start_date[1]
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

      benchmark_traces <- dplyr::bind_rows(
        if (!is.null(data$sp500)) tibble::tibble(
          date = data$sp500$dates[data$sp500$dates >= earliest_start],
          return_pct = as.numeric(data$sp500$cumulative_returns[data$sp500$dates >= earliest_start]) * 100,
          portfolio = "S&P 500"
        ),
        if (!is.null(data$bitcoin)) tibble::tibble(
          date = data$bitcoin$dates[data$bitcoin$dates >= earliest_start],
          return_pct = as.numeric(data$bitcoin$cumulative_returns[data$bitcoin$dates >= earliest_start]) * 100,
          portfolio = "Bitcoin"
        )
      )

      plot_df <- dplyr::bind_rows(portfolio_traces, benchmark_traces)

      if (nrow(plot_df) == 0) return(plotly_empty())

      plot_ly(plot_df, x = ~date, y = ~return_pct, color = ~portfolio, type = 'scatter', mode = 'lines') %>%
        layout(title = "Performance Comparison", yaxis = list(title = "Cumulative Return (%)"), hovermode = 'x unified')
    })

    output$metrics_table <- renderDT({
      data <- portfolio_data()
      if (is.null(data)) return(data.frame(Message = "No data available"))

      metrics <- calculate_performance_metrics(data)

      if (is.null(metrics) || nrow(metrics) == 0) {
        return(data.frame(Message = "No performance metrics available"))
      }

      DT::datatable(metrics, options = list(dom = 't'), rownames = FALSE) %>%
        DT::formatPercentage(c("Total_Return", "Volatility", "Max_Drawdown"), 2) %>%
        DT::formatRound("Sharpe_Ratio", 2)
    })

    output$selection_summary <- renderUI({
      meta <- portfolio_metadata()
      if (nrow(meta) == 0) {
        return(tags$p("No portfolios loaded."))
      }

      group <- input$portfolio_group
      if (is.null(group) || !group %in% meta$portfolio_name) {
        return(tags$p("Select a portfolio to begin."))
      }

      group_meta <- meta %>% filter(portfolio_name == group)
      if (nrow(group_meta) == 0) {
        return(tags$p("No versions available for the selected portfolio."))
      }

      versions <- input$selected_versions
      if (is.null(versions) || length(versions) == 0) {
        latest <- tail(group_meta$version_label, 1)
        return(tags$p(sprintf("Select one or more versions to compare. Latest available: %s", latest)))
      }

      selected_meta <- group_meta %>%
        filter(key %in% versions) %>%
        arrange(desc(start_date))

      if (nrow(selected_meta) == 0) {
        return(tags$p("Select at least one valid version."))
      }

      data <- portfolio_data()

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

        value_text <- if (is.na(return_pct)) "Latest return unavailable" else sprintf("Latest cumulative return: %.2f%%", return_pct)
        tags$li(sprintf("%s - %s", label, value_text))
      })

      previous_meta <- group_meta %>%
        filter(!key %in% versions) %>%
        arrange(desc(start_date)) %>%
        slice_head(n = 1)

      tags$div(
        class = "selection-summary",
        tags$p(sprintf("Selected portfolio: %s", group)),
        tags$p(sprintf("Total versions available: %d", nrow(group_meta))),
        tags$ul(version_items),
        if (nrow(previous_meta) > 0) tags$p(sprintf("Previous version available: %s", previous_meta$version_label))
      )
    })

    reactive({
      list(
        selected_ids = selected_versions(),
        selected_group = input$portfolio_group,
        metadata = portfolio_metadata()
      )
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


