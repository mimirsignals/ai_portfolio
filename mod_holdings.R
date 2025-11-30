
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
          selected_ids = character(),
          metadata = tibble()
        )
      } else {
        sel
      }
    })

    output$selection_summary <- renderUI({
      sel <- current_selection()
      meta <- sel$metadata
      group <- sel$selected_group

      if (is.null(meta) || nrow(meta) == 0) {
        return(tags$p("No portfolios loaded."))
      }

      if (is.null(group) || !group %in% meta$portfolio_name) {
        return(tags$p("Select a portfolio in the sidebar to view holdings."))
      }

      group_meta <- meta %>%
        dplyr::filter(portfolio_name == group) %>%
        dplyr::arrange(dplyr::desc(start_date))

      if (nrow(group_meta) == 0) {
        return(tags$p("No versions available for the selected portfolio."))
      }

      versions <- sel$selected_ids
      if (length(versions) == 0) {
        latest <- group_meta$version_label[1]
        return(tags$p(sprintf("Select at least one version in the sidebar. Latest available: %s", latest)))
      }

      tags$div(
        class = "selection-summary",
        tags$p(sprintf("Portfolio: %s", group)),
        tags$p(sprintf("Versions selected: %s", paste(group_meta$version_label[group_meta$key %in% versions], collapse = ", ")))
      )
    })

    output$version_selector <- renderUI({
      sel <- current_selection()
      meta <- sel$metadata
      group <- sel$selected_group

      if (is.null(meta) || nrow(meta) == 0) {
        return(tags$p("No versions available."))
      }

      if (is.null(group) || !group %in% meta$portfolio_name) {
        return(tags$p("Select a portfolio in the sidebar."))
      }

      available_keys <- sel$selected_ids
      if (length(available_keys) == 0) {
        return(tags$p("Select at least one version in the sidebar."))
      }

      group_meta <- meta %>%
        dplyr::filter(key %in% available_keys, portfolio_name == group) %>%
        dplyr::arrange(dplyr::desc(start_date))

      if (nrow(group_meta) == 0) {
        return(tags$p("No valid versions available for holdings view."))
      }

      selected <- isolate(input$portfolio_version)
      if (is.null(selected) || !selected %in% group_meta$key) {
        selected <- group_meta$key[1]
      }

      selectInput(
        session$ns("portfolio_version"),
        label = "Version",
        choices = stats::setNames(group_meta$key, group_meta$version_label),
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
        target_weights_df$Avg_Purchase_Price <- "N/A"
        target_weights_df$Current_Price <- "N/A"
        target_weights_df$Return <- "N/A"

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

        # Calculate average purchase price from transaction history
        # Get ALL transactions for this portfolio and its predecessors
        data <- tryCatch(portfolio_data(), error = function(e) NULL)
        key <- selected_key()

        avg_prices <- tibble(symbol = character(), avg_price = numeric())

        if (!is.null(data) && !is.null(data$transactions)) {
          # Get the portfolio name to find all versions in the chain
          portfolio_name <- def$portfolio_name

          # Collect all transactions from all versions of this portfolio
          all_transactions <- tibble()
          for (tx_key in names(data$transactions)) {
            tx_data <- data$transactions[[tx_key]]
            if (!is.null(tx_data) && nrow(tx_data) > 0) {
              # Only include if it's part of the same portfolio chain
              if (!is.null(tx_data$portfolio) && any(tx_data$portfolio == portfolio_name)) {
                all_transactions <- dplyr::bind_rows(all_transactions, tx_data)
              }
            }
          }

          # Calculate weighted average purchase price for buy transactions only
          if (nrow(all_transactions) > 0) {
            avg_prices <- all_transactions %>%
              dplyr::filter(action == "buy") %>%
              dplyr::group_by(symbol) %>%
              dplyr::summarise(
                total_value = sum(abs(shares_change) * price, na.rm = TRUE),
                total_shares = sum(abs(shares_change), na.rm = TRUE),
                .groups = 'drop'
              ) %>%
              dplyr::mutate(avg_price = ifelse(total_shares > 0, total_value / total_shares, NA_real_)) %>%
              dplyr::select(symbol, avg_price)
          }
        }

        # Get current prices for all symbols
        current_prices <- tibble(symbol = character(), current_price = numeric())

        tryCatch({
          # Fetch latest stock data for all symbols
          stock_data <- fetch_stock_data(def$symbols, start_date = Sys.Date() - 7)

          if (!is.null(stock_data) && nrow(stock_data) > 0) {
            current_prices <- stock_data %>%
              dplyr::group_by(symbol) %>%
              dplyr::arrange(dplyr::desc(date)) %>%
              dplyr::slice_head(n = 1) %>%
              dplyr::ungroup() %>%
              dplyr::select(symbol, current_price = adjusted_price)
          }
        }, error = function(e) {
          warning(paste("Error fetching current prices:", e$message))
        })

        target_weights_df <- target_weights_df %>%
          dplyr::left_join(latest_values, by = c("Symbol" = "symbol")) %>%
          dplyr::left_join(avg_prices, by = c("Symbol" = "symbol")) %>%
          dplyr::left_join(current_prices, by = c("Symbol" = "symbol")) %>%
          dplyr::mutate(
            # Calculate return percentage
            return_pct = ifelse(
              !is.na(avg_price) & !is.na(current_price) & is.finite(avg_price) & is.finite(current_price) & avg_price > 0,
              ((current_price - avg_price) / avg_price) * 100,
              NA_real_
            ),
            Actual_Weight = ifelse(
              is.na(actual_weight_pct),
              "N/A",
              paste0(round(actual_weight_pct, 1), "%")
            ),
            Avg_Purchase_Price = ifelse(
              is.na(avg_price) | !is.finite(avg_price),
              "N/A",
              sprintf("$%.2f", avg_price)
            ),
            Current_Price = ifelse(
              is.na(current_price) | !is.finite(current_price),
              "N/A",
              sprintf("$%.2f", current_price)
            ),
            Return = ifelse(
              is.na(return_pct) | !is.finite(return_pct),
              "N/A",
              sprintf("%+.1f%%", return_pct)
            )
          ) %>%
          dplyr::select(Symbol, Target_Weight, Actual_Weight, Avg_Purchase_Price, Current_Price, Return)
      }

      DT::datatable(
        target_weights_df,
        options = list(pageLength = 20, dom = 't'),
        rownames = FALSE,
        colnames = c('Symbol', 'Target Weight', 'Actual Weight', 'Avg Purchase Price', 'Current Price', 'Return %')
      ) %>%
        DT::formatStyle(
          'Return',
          color = DT::styleInterval(
            cuts = c(0),
            values = c('red', 'green')
          )
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

      # Use individual_stocks data (already calculated, no need to fetch)
      stock_performance <- portfolio_entry$individual_stocks %>%
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

      if (nrow(stock_performance) == 0) {
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



