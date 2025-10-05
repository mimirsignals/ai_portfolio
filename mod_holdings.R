
holdingsUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(
      12,
      box(
        title = "Portfolio Holdings",
        status = "primary",
        solidHeader = TRUE,
        width = NULL,
        uiOutput(ns("selection_summary")),
        uiOutput(ns("group_selector")),
        uiOutput(ns("version_selector")),
        h4("Portfolio Holdings"),
        DT::dataTableOutput(ns("combined_holdings_table")),
        br(),
        h4("Stock Performance Since Rebalancing"),
        plotly::plotlyOutput(ns("stock_performance_plot"), height = "400px"),
        br(),
        h4("Rebalance Transactions"),
        DT::dataTableOutput(ns("transaction_table"))
      )
    )
  )
}


holdingsServer <- function(id, portfolios_reactive, selection_state, portfolio_data) {
  moduleServer(id, function(input, output, session) {

    current_selection <- reactive({
      sel <- selection_state()
      if (is.null(sel)) {
        list(
          selected_group = NULL,
          selected_groups = character(),
          selected_ids = character(),
          metadata = tibble(),
          date_range = NULL
        )
      } else {
        sel
      }
    })

    available_groups <- reactive({
      sel <- current_selection()
      groups <- sel$selected_groups
      meta <- sel$metadata
      if ((is.null(groups) || length(groups) == 0) && !is.null(meta) && nrow(meta) > 0) {
        groups <- unique(meta$portfolio_name)
      }
      unique(groups)
    })

    output$group_selector <- renderUI({
      groups <- available_groups()
      if (length(groups) <= 1) {
        return(NULL)
      }
      selected <- isolate(input$holdings_group)
      if (is.null(selected) || !selected %in% groups) {
        selected <- groups[1]
      }
      selectInput(
        session$ns("holdings_group"),
        label = "Focus portfolio",
        choices = stats::setNames(groups, groups),
        selected = selected
      )
    })

    active_group <- reactive({
      groups <- available_groups()
      selected <- input$holdings_group
      if (!is.null(selected) && selected %in% groups) {
        selected
      } else if (length(groups) > 0) {
        groups[1]
      } else {
        NULL
      }
    })

    output$selection_summary <- renderUI({
      sel <- current_selection()
      meta <- sel$metadata
      group <- active_group()

      if (is.null(meta) || nrow(meta) == 0) {
        return(tags$p("No portfolios loaded."))
      }

      if (is.null(group) || !group %in% meta$portfolio_name) {
        return(tags$p("Select a portfolio to inspect holdings."))
      }

      group_meta <- meta %>%
        dplyr::filter(portfolio_name == group) %>%
        dplyr::arrange(dplyr::desc(start_date))

      if (nrow(group_meta) == 0) {
        return(tags$p("No versions available for the selected portfolio."))
      }

      selected_versions <- intersect(sel$selected_ids, group_meta$key)
      if (length(selected_versions) == 0) {
        latest <- group_meta$version_label[1]
        return(tags$p(sprintf("Add %s to the comparison to view holdings (latest: %s).", group, latest)))
      }

      tags$div(
        class = "selection-summary",
        tags$p(sprintf("Portfolio: %s", group)),
        tags$p(sprintf("Versions available in view: %s", paste(group_meta$version_label[group_meta$key %in% selected_versions], collapse = ", ")))
      )
    })

    output$version_selector <- renderUI({
      sel <- current_selection()
      meta <- sel$metadata
      group <- active_group()

      if (is.null(meta) || nrow(meta) == 0) {
        return(tags$p("No versions available."))
      }

      if (is.null(group) || !group %in% meta$portfolio_name) {
        return(tags$p("Select a portfolio to inspect holdings."))
      }

      group_meta <- meta %>%
        dplyr::filter(portfolio_name == group) %>%
        dplyr::arrange(dplyr::desc(start_date))

      available_keys <- intersect(sel$selected_ids, group_meta$key)
      if (length(available_keys) == 0) {
        latest <- group_meta$version_label[1]
        return(tags$p(sprintf("Add %s to the comparison to inspect holdings (latest: %s).", group, latest)))
      }

      options_df <- group_meta %>% dplyr::filter(key %in% available_keys)

      selected <- isolate(input$portfolio_version)
      if (is.null(selected) || !selected %in% options_df$key) {
        selected <- options_df$key[1]
      }

      selectInput(
        session$ns("portfolio_version"),
        label = "Version",
        choices = stats::setNames(options_df$key, options_df$version_label),
        selected = selected
      )
    })

    selected_key <- reactive({
      req(input$portfolio_version)
      input$portfolio_version
    })

    selected_definition <- reactive({
      portfolios <- portfolios_reactive()
      portfolios[[selected_key()]]
    })

    selected_portfolio_data <- reactive({
      data <- tryCatch(portfolio_data(), error = function(e) NULL)
      key <- selected_key()
      if (!is.null(data) && !is.null(data$portfolios[[key]])) {
        data$portfolios[[key]]
      } else {
        NULL
      }
    })

    selected_transactions <- reactive({
      data <- tryCatch(portfolio_data(), error = function(e) NULL)
      key <- selected_key()

      if (is.null(data) || is.null(data$transactions)) {
        return(tibble::tibble())
      }

      tx <- data$transactions[[key]]
      if (is.null(tx)) tibble::tibble() else tx
    })

    output$combined_holdings_table <- DT::renderDataTable({
      def <- selected_definition()
      req(def)

      target_weights_df <- tibble(
        Symbol = def$symbols,
        Target_Weight = paste0(round(def$weights * 100, 1), "%")
      )

      portfolio_entry <- selected_portfolio_data()

      if (is.null(portfolio_entry) ||
          is.null(portfolio_entry$individual_stocks)) {

        target_weights_df$Actual_Weight <- "N/A"

      } else {
        latest_values <- portfolio_entry$individual_stocks %>%
          dplyr::filter(symbol %in% def$symbols) %>%
          dplyr::group_by(symbol) %>%
          dplyr::arrange(date) %>%
          dplyr::slice_tail(n = 1) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(investment = as.numeric(investment)) %>%
          dplyr::mutate(actual_weight_pct = (investment / sum(investment)) * 100) %>%
          dplyr::select(symbol, actual_weight_pct)

        target_weights_df <- target_weights_df %>%
          dplyr::left_join(latest_values, by = c("Symbol" = "symbol")) %>%
          dplyr::mutate(
            Actual_Weight = ifelse(
              is.na(actual_weight_pct),
              "N/A",
              paste0(round(actual_weight_pct, 1), "%")
            )
          ) %>%
          dplyr::select(Symbol, Target_Weight, Actual_Weight)
      }

      DT::datatable(
        target_weights_df,
        options = list(pageLength = 20, dom = 't'),
        rownames = FALSE,
        colnames = c('Symbol', 'Target Weight', 'Actual Weight')
      )
    })

    output$transaction_table <- DT::renderDataTable({
      tx <- selected_transactions()

      if (nrow(tx) == 0) {
        return(DT::datatable(
          data.frame(Message = "No transactions for this version"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      display <- tx %>%
        dplyr::mutate(
          rebalance_date = as.character(rebalance_date),
          price_date = as.character(price_date)
        )

      DT::datatable(
        display,
        options = list(pageLength = 15, order = list(list(0, 'desc'))),
        rownames = FALSE
      ) %>%
        DT::formatRound(c('trade_value', 'shares_change', 'price', 'previous_value', 'target_value'), 4) %>%
        DT::formatPercentage(c('previous_weight', 'target_weight'), 1)
    })

    output$stock_performance_plot <- plotly::renderPlotly({
      def <- selected_definition()
      portfolio_entry <- selected_portfolio_data()
      req(def)

      if (is.null(portfolio_entry) || is.null(portfolio_entry$individual_stocks)) {
        return(plotly_empty("No holdings performance data"))
      }

      rebalance_date <- as.Date(def$start_date)
      fallback_data <- portfolio_entry$individual_stocks

      start_fetch <- rebalance_date
      if (is.na(start_fetch) && !is.null(fallback_data)) {
        start_fetch <- suppressWarnings(min(fallback_data$date, na.rm = TRUE))
      }

      stock_prices <- NULL
      if (!is.na(start_fetch)) {
        stock_prices <- fetch_stock_data(def$symbols, start_fetch)
      }

      if (!is.null(stock_prices) && nrow(stock_prices) > 0) {
        stock_performance <- stock_prices %>%
          dplyr::filter(symbol %in% def$symbols) %>%
          dplyr::filter(is.na(rebalance_date) | date >= rebalance_date) %>%
          dplyr::group_by(symbol) %>%
          dplyr::arrange(date) %>%
          dplyr::mutate(
            adjusted_price = as.numeric(adjusted_price),
            baseline_price = dplyr::first(adjusted_price),
            cumulative_return = ((adjusted_price / baseline_price) - 1) * 100
          ) %>%
          dplyr::ungroup() %>%
          dplyr::filter(is.finite(cumulative_return))
      } else if (!is.null(fallback_data)) {
        stock_performance <- fallback_data %>%
          dplyr::filter(symbol %in% def$symbols) %>%
          dplyr::filter(is.na(rebalance_date) | date >= rebalance_date) %>%
          dplyr::group_by(symbol) %>%
          dplyr::arrange(date) %>%
          dplyr::mutate(
            investment = as.numeric(investment),
            baseline_value = dplyr::first(investment),
            cumulative_return = ((investment / baseline_value) - 1) * 100
          ) %>%
          dplyr::ungroup() %>%
          dplyr::filter(is.finite(cumulative_return))
      } else {
        stock_performance <- NULL
      }

      if (is.null(stock_performance) || nrow(stock_performance) == 0) {
        return(plotly_empty("No holdings performance data"))
      }

      plotly::plot_ly(
        stock_performance,
        x = ~date,
        y = ~cumulative_return,
        color = ~symbol,
        type = 'scatter',
        mode = 'lines',
        line = list(width = 2),
        hovertemplate = paste(
          "<b>%{fullData.name}</b><br>",
          "Date: %{x}<br>",
          "Cumulative Return: %{y:.1f}%<br>",
          "<extra></extra>"
        )
      ) %>%
        plotly::layout(
          title = list(
            text = if (is.na(rebalance_date)) {
              "Cumulative Stock Returns Since Rebalancing"
            } else {
              paste(
                "Cumulative Stock Returns Since Rebalancing on",
                format(rebalance_date, "%Y-%m-%d")
              )
            },
            font = list(size = 16)
          ),
          xaxis = list(title = "Date", type = "date"),
          yaxis = list(
            title = "Cumulative Return Since Rebalancing (%)",
            tickformat = ".1f",
            zeroline = TRUE,
            zerolinecolor = "rgba(0,0,0,0.3)",
            zerolinewidth = 1
          ),
          hovermode = 'x unified',
          legend = list(title = list(text = "Stock Symbol"), orientation = "v", x = 1, y = 1),
          margin = list(l = 50, r = 50, t = 50, b = 50)
        ) %>%
        plotly::config(displayModeBar = TRUE, displaylogo = FALSE)
    })
  })
}






