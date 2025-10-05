workspaceUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(
      width = 4,
      box(
        title = "Setup",
        status = "info",
        solidHeader = TRUE,
        width = NULL,
        uiOutput(ns("input_panel"))
      ),
      box(
        title = "Summary",
        status = "success",
        solidHeader = TRUE,
        width = NULL,
        uiOutput(ns("summary_panel"))
      )
    ),
    column(
      width = 8,
      box(
        title = "Projection",
        status = "primary",
        solidHeader = TRUE,
        width = NULL,
        plotly::plotlyOutput(ns("projection_plot"), height = "420px")
      ),
      box(
        title = "Contribution Log",
        status = "info",
        solidHeader = TRUE,
        width = NULL,
        DT::dataTableOutput(ns("contribution_table"))
      )
    )
  )
}

workspaceServer <- function(id, portfolios_reactive, selection_state, portfolio_calc) {
  moduleServer(id, function(input, output, session) {

    format_currency <- function(x) {
      sprintf("$%s", formatC(x, format = "f", big.mark = ",", digits = 2))
    }

    metadata <- reactive({
      sel <- selection_state()
      meta <- sel$metadata
      if (is.null(meta)) tibble::tibble() else meta
    })

    available_groups <- reactive({
      meta <- metadata()
      if (nrow(meta) == 0) return(character())
      unique(meta$portfolio_name)
    })

    latest_version_key <- function(meta, group) {
      latest <- meta %>%
        dplyr::filter(portfolio_name == group) %>%
        dplyr::arrange(dplyr::desc(start_date)) %>%
        dplyr::slice_head(n = 1)
      if (nrow(latest) == 0) return(NULL)
      latest$key[1]
    }

    output$input_panel <- renderUI({
      meta <- metadata()
      groups <- available_groups()
      if (length(groups) == 0) {
        return(tags$p("No portfolios available. Load data to begin."))
      }

      selected_group <- input$portfolio
      if (is.null(selected_group) || !selected_group %in% groups) {
        selected_group <- groups[1]
      }

      group_meta <- meta %>%
        dplyr::filter(portfolio_name == selected_group) %>%
        dplyr::arrange(start_date)

      earliest <- suppressWarnings(min(group_meta$start_date, na.rm = TRUE))
      if (!is.finite(earliest)) {
        earliest <- Sys.Date() - 365
      }
      latest <- Sys.Date()

      start_value <- input$start_date
      if (is.null(start_value) || start_value < earliest || start_value > latest) {
        start_value <- earliest
      }

      tagList(
        selectInput(
          session$ns("portfolio"),
          label = "Model portfolio",
          choices = stats::setNames(groups, groups),
          selected = selected_group
        ),
        dateInput(
          session$ns("start_date"),
          label = "Start investing",
          value = start_value,
          min = earliest,
          max = latest
        ),
        numericInput(
          session$ns("initial"),
          label = "Initial investment",
          value = ifelse(is.null(input$initial), 10000, input$initial),
          min = 0,
          step = 500
        ),
        numericInput(
          session$ns("recurring"),
          label = "Recurring contribution",
          value = ifelse(is.null(input$recurring), 500, input$recurring),
          min = 0,
          step = 50
        ),
        selectInput(
          session$ns("frequency"),
          label = "Contribution frequency",
          choices = c("Monthly", "Quarterly", "Yearly"),
          selected = ifelse(is.null(input$frequency), "Monthly", input$frequency)
        )
      )
    })

    selected_group <- reactive({
      group <- input$portfolio
      groups <- available_groups()
      if (is.null(group) || !group %in% groups) {
        if (length(groups) > 0) groups[1] else NULL
      } else {
        group
      }
    })

    simulation <- reactive({
      meta <- metadata()
      group <- selected_group()
      if (is.null(group) || nrow(meta) == 0) {
        return(NULL)
      }

      key <- latest_version_key(meta, group)
      if (is.null(key)) return(NULL)

      start_date <- as.Date(input$start_date)
      if (is.na(start_date)) {
        start_date <- suppressWarnings(min(meta$start_date[meta$portfolio_name == group], na.rm = TRUE))
      }
      initial <- max(0, as.numeric(input$initial))
      recurring <- max(0, as.numeric(input$recurring))
      frequency <- input$frequency
      if (is.null(frequency)) frequency <- "Monthly"

      calc <- tryCatch({
        portfolio_calc(
          portfolios = portfolios_reactive(),
          selected_portfolios = key,
          show_sp500 = FALSE,
          show_btc = FALSE
        )
      }, error = function(e) NULL)

      if (is.null(calc) || is.null(calc$portfolios[[key]])) {
        return(NULL)
      }

      series <- calc$portfolios[[key]]
      tbl <- series$portfolio_tbl
      if (is.null(tbl) || nrow(tbl) == 0) return(NULL)

      tbl <- tbl %>%
        dplyr::mutate(date = as.Date(date)) %>%
        dplyr::arrange(date) %>%
        dplyr::filter(date >= start_date)

      if (nrow(tbl) == 0) return(NULL)

      sim <- simulate_contributions(tbl, start_date, initial, recurring, frequency)
      if (is.null(sim)) return(NULL)

      metrics <- list(
        group = group,
        version = meta %>% dplyr::filter(key == !!key) %>% dplyr::slice_head(n = 1) %>% dplyr::pull(version_label),
        total_contributed = tail(sim$path$total_contributed, 1),
        current_value = tail(sim$path$portfolio_value, 1),
        gain = tail(sim$path$portfolio_value, 1) - tail(sim$path$total_contributed, 1),
        irr = sim$irr,
        twr = if (!is.null(series$cumulative_returns)) tail(series$cumulative_returns, 1) else NA_real_,
        start_date = start_date,
        latest_date = max(sim$path$date)
      )

      list(path = sim$path, contributions = sim$contributions, metrics = metrics)
    })

    simulate_contributions <- function(tbl, start_date, initial, recurring, frequency) {
      tbl <- tbl %>% dplyr::arrange(date)
      dates <- as.Date(tbl$date)
      daily_returns <- as.numeric(tbl$daily_return)

      freq_by <- switch(frequency,
        Monthly = "month",
        Quarterly = "quarter",
        Yearly = "year",
        "month"
      )

      schedule <- if (recurring > 0) {
        seq_dates <- seq.Date(start_date, max(dates), by = freq_by)
        if (length(seq_dates) > 1) seq_dates[-1] else as.Date(character())
      } else {
        as.Date(character())
      }

      remaining <- schedule
      value <- initial
      total_contrib <- initial
      values <- numeric(length(dates))
      contributions <- numeric(length(dates))

      contribution_log <- list()
      contribution_log[[length(contribution_log) + 1]] <- tibble::tibble(
        date = start_date,
        contribution = initial,
        total_contributed = initial
      )

      for (i in seq_along(dates)) {
        date <- dates[i]
        if (i > 1) {
          if (length(remaining) > 0) {
            while (length(remaining) > 0 && !is.na(remaining[1]) && remaining[1] <= date) {
              value <- value + recurring
              total_contrib <- total_contrib + recurring
              contribution_log[[length(contribution_log) + 1]] <- tibble::tibble(
                date = date,
                contribution = recurring,
                total_contributed = total_contrib
              )
              remaining <- remaining[-1]
            }
          }
          value <- value * exp(daily_returns[i])
        }
        values[i] <- value
        contributions[i] <- total_contrib
      }

      path <- tibble::tibble(
        date = dates,
        portfolio_value = values,
        total_contributed = contributions,
        gain = portfolio_value - total_contributed,
        return_pct = ifelse(total_contributed > 0, (portfolio_value / total_contributed) - 1, NA_real_)
      )

      contributions_tbl <- if (length(contribution_log) > 0) {
        dplyr::bind_rows(contribution_log) %>% dplyr::arrange(date)
      } else {
        tibble::tibble(date = start_date, contribution = initial, total_contributed = initial)
      }

      irr <- NA_real_
      if (nrow(contributions_tbl) >= 1) {
        cash_flow_dates <- c(contributions_tbl$date, tail(path$date, 1))
        cash_flows <- c(-contributions_tbl$contribution, tail(path$portfolio_value, 1))
        times <- as.numeric(cash_flow_dates - start_date) / 365.25
        irr <- tryCatch({
          fn <- function(rate) sum(cash_flows * (1 + rate) ^ (-times))
          lower <- -0.999
          upper <- 5
          if (fn(lower) * fn(upper) > 0) stop("root not bracketed")
          uniroot(fn, interval = c(lower, upper), tol = 1e-6)$root
        }, error = function(e) NA_real_)
      }

      list(path = path, contributions = contributions_tbl, irr = irr)
    }

    summary_metrics <- reactive({
      sim <- simulation()
      if (is.null(sim)) return(NULL)
      sim$metrics
    })

    output$summary_panel <- renderUI({
      metrics <- summary_metrics()
      if (is.null(metrics)) {
        return(tags$p("Adjust the inputs to simulate your portfolio."))
      }

      irr_text <- if (!is.na(metrics$irr)) sprintf("%.2f%%", metrics$irr * 100) else "N/A"
      twr_text <- if (!is.null(metrics$twr) && !is.na(metrics$twr)) sprintf("%.2f%%", metrics$twr * 100) else "N/A"

      tagList(
        tags$p(sprintf("Portfolio: %s (%s)", metrics$group, metrics$version)),
        fluidRow(
          column(6, tags$strong("Total contributed"), tags$p(format_currency(metrics$total_contributed))),
          column(6, tags$strong("Current value"), tags$p(format_currency(metrics$current_value)))
        ),
        fluidRow(
          column(6, tags$strong("Net gain"), tags$p(format_currency(metrics$gain))),
          column(3, tags$strong("IRR"), tags$p(irr_text)),
          column(3, tags$strong("Portfolio return"), tags$p(twr_text))
        )
      )
    })

    output$projection_plot <- plotly::renderPlotly({
      sim <- simulation()
      if (is.null(sim)) return(plotly_empty("No simulation yet"))

      path <- sim$path
      if (nrow(path) == 0) return(plotly_empty("No simulation yet"))

      plotly::plot_ly() %>%
        plotly::add_lines(
          data = path,
          x = ~date,
          y = ~portfolio_value,
          name = "Portfolio value",
          line = list(width = 2)
        ) %>%
        plotly::add_lines(
          data = path,
          x = ~date,
          y = ~total_contributed,
          name = "Total contributed",
          line = list(width = 2, dash = "dash"),
          hoverinfo = "text",
          text = ~sprintf("Date: %s<br>Total contributed: %s", date, format_currency(total_contributed))
        ) %>%
        plotly::layout(
          xaxis = list(title = "Date"),
          yaxis = list(title = "Value ($)", separatethousands = TRUE),
          hovermode = "x unified",
          legend = list(orientation = "h", x = 0, y = 1.05)
        )
    })

    output$contribution_table <- DT::renderDataTable({
      sim <- simulation()
      if (is.null(sim)) {
        return(DT::datatable(
          data.frame(Message = "No contributions simulated yet"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      contributions <- sim$contributions %>%
        dplyr::mutate(
          Date = format(date, "%Y-%m-%d"),
          Contribution = format_currency(contribution),
          `Total Contributed` = format_currency(total_contributed)
        ) %>%
        dplyr::select(Date, Contribution, `Total Contributed`)

      DT::datatable(
        contributions,
        options = list(pageLength = 10, order = list(list(0, 'asc'))),
        rownames = FALSE
      )
    })
  })
}
