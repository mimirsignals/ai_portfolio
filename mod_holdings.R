# mod_holdings.R - Portfolio holdings module with version tracking

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
        fluidRow(
          column(6, selectInput(ns("portfolio_group"), "Portfolio:", choices = NULL)),
          column(6, selectInput(ns("portfolio_version"), "Version:", choices = NULL))
        ),
        uiOutput(ns("selection_summary")),
        h4("Portfolio Holdings"),
        DT::dataTableOutput(ns("combined_holdings_table")),
        br(),
        h4("Stock Performance Since Rebalancing"),
        plotly::plotlyOutput(ns("stock_performance_plot"), height = "400px")
      )
    )
  )
}

holdingsServer <- function(id, portfolios_reactive, portfolio_calc, selection_state) {
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
        arrange(portfolio_name, desc(start_date))
    })

    observe({
      meta <- portfolio_metadata()
      choices <- unique(meta$portfolio_name)

      if (length(choices) == 0) {
        updateSelectInput(session, "portfolio_group", choices = choices, selected = character(0))
        updateSelectInput(session, "portfolio_version", choices = list(), selected = character(0))
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
        updateSelectInput(session, "portfolio_version", choices = list(), selected = character(0))
        return()
      }

      group_meta <- meta %>%
        filter(portfolio_name == group) %>%
        arrange(desc(start_date))

      choices <- stats::setNames(group_meta$key, group_meta$version_label)
      selected_version <- input$portfolio_version
      if (is.null(selected_version) || !selected_version %in% group_meta$key) {
        selected_version <- group_meta$key[1]
      }

      updateSelectInput(session, "portfolio_version", choices = choices, selected = selected_version)
    }, ignoreNULL = FALSE)

    observeEvent(selection_state(), {
      sel <- selection_state()
      meta <- portfolio_metadata()
      if (nrow(meta) == 0) return()

      group <- sel$selected_group
      if (!is.null(group) && group %in% meta$portfolio_name) {
        updateSelectInput(session, "portfolio_group", selected = group)
      }

      ids <- sel$selected_ids
      if (!is.null(ids) && length(ids) > 0) {
        selected_meta <- meta %>%
          filter(key %in% ids) %>%
          arrange(desc(start_date))
        if (nrow(selected_meta) > 0) {
          updateSelectInput(session, "portfolio_version", selected = selected_meta$key[1])
        }
      }
    }, ignoreNULL = TRUE)

    selected_key <- reactive({
      req(input$portfolio_version)
      input$portfolio_version
    })

    selected_definition <- reactive({
      portfolios <- portfolios_reactive()
      portfolios[[selected_key()]]
    })

    selection_summary_data <- reactive({
      meta <- portfolio_metadata()
      key <- selected_key()
      if (!key %in% meta$key) {
        return(NULL)
      }
      meta %>% filter(key == !!key)
    })

    output$selection_summary <- renderUI({
      meta_row <- selection_summary_data()
      def <- selected_definition()

      if (is.null(meta_row) || nrow(meta_row) == 0 || is.null(def)) {
        return(tags$p("Select a portfolio version to inspect."))
      }

      investment <- def$total_investment
      investment_text <- if (is.na(investment)) "Not specified" else formatC(investment, format = "f", digits = 2, big.mark = ",")

      tags$div(
        class = "selection-summary",
        tags$p(sprintf("Portfolio: %s", meta_row$portfolio_name[1])),
        tags$p(sprintf("Rebalance date: %s", meta_row$version_label[1])),
        tags$p(sprintf("Holdings: %d", length(def$symbols))),
        tags$p(sprintf("Initial investment: %s", investment_text))
      )
    })

    selected_portfolio_data <- reactive({
      key <- selected_key()
      req(key)

      tryCatch({
        portfolio_calc(
          portfolios = portfolios_reactive(),
          selected_portfolios = key,
          show_sp500 = FALSE,
          show_btc = FALSE
        )
      }, error = function(e) {
        warning(paste("Holdings module calculation error:", e$message))
        NULL
      })
    })

    output$combined_holdings_table <- DT::renderDataTable({
      def <- selected_definition()
      req(def)

      target_weights_df <- tibble(
        Symbol = def$symbols,
        Target_Weight = paste0(round(def$weights * 100, 1), "%")
      )

      portfolio_data <- selected_portfolio_data()

      if (is.null(portfolio_data) ||
          is.null(portfolio_data$portfolios[[selected_key()]]) ||
          is.null(portfolio_data$portfolios[[selected_key()]]$individual_stocks)) {

        target_weights_df$Actual_Weight <- "N/A"

      } else {
        latest_values <- portfolio_data$portfolios[[selected_key()]]$individual_stocks %>%
          filter(symbol %in% def$symbols) %>%
          group_by(symbol) %>%
          slice_tail(n = 1) %>%
          ungroup() %>%
          mutate(investment = as.numeric(investment)) %>%
          mutate(actual_weight_pct = (investment / sum(investment)) * 100) %>%
          select(symbol, actual_weight_pct)

        target_weights_df <- target_weights_df %>%
          left_join(latest_values, by = c("Symbol" = "symbol")) %>%
          mutate(
            Actual_Weight = ifelse(is.na(actual_weight_pct), "N/A",
                                   paste0(round(actual_weight_pct, 1), "%"))
          ) %>%
          select(Symbol, Target_Weight, Actual_Weight)
      }

      DT::datatable(
        target_weights_df,
        options = list(pageLength = 20, dom = 't'),
        rownames = FALSE,
        colnames = c('Symbol', 'Target Weight', 'Actual Weight')
      )
    })

    output$stock_performance_plot <- plotly::renderPlotly({
      def <- selected_definition()
      req(def)

      rebalance_date <- as.Date(def$start_date)
      symbols <- def$symbols

      stock_data <- fetch_stock_data(symbols, rebalance_date)

      if (is.null(stock_data) || nrow(stock_data) == 0) {
        return(plotly::plotly_empty())
      }

      stock_performance <- stock_data %>%
        group_by(symbol) %>%
        arrange(date) %>%
        mutate(
          baseline_price = first(adjusted_price),
          cumulative_return = ((adjusted_price / baseline_price) - 1) * 100
        ) %>%
        ungroup() %>%
        filter(!is.na(cumulative_return), is.finite(cumulative_return))

      if (nrow(stock_performance) == 0) {
        return(plotly::plotly_empty())
      }

      stock_performance %>%
        plotly::plot_ly(
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
            text = paste("Cumulative Stock Returns Since Rebalancing on", format(rebalance_date, "%Y-%m-%d")),
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
