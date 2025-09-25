# mod_history.R - Rebalance history module

historyUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    box(
      title = "Rebalance History",
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      DT::dataTableOutput(ns("history_table"))
    )
  )
}

historyServer <- function(id, portfolios_reactive, portfolio_calc) {
  moduleServer(id, function(input, output, session) {

    history_data <- reactive({
      portfolios <- portfolios_reactive()
      if (length(portfolios) == 0) {
        return(tibble())
      }

      meta <- purrr::imap_dfr(portfolios, function(def, key) {
        tibble(
          key = key,
          portfolio_name = if (!is.null(def$portfolio_name)) def$portfolio_name else key,
          version_label = if (!is.null(def$version_label)) def$version_label else format(as.Date(def$start_date), "%Y-%m-%d"),
          start_date = as.Date(def$start_date),
          holdings = length(def$symbols)
        )
      })

      calc <- tryCatch({
        portfolio_calc(
          portfolios = portfolios_reactive(),
          selected_portfolios = names(portfolios),
          show_sp500 = FALSE,
          show_btc = FALSE
        )
      }, error = function(e) {
        warning(paste("History module calculation error:", e$message))
        NULL
      })

      if (!is.null(calc)) {
        returns_df <- purrr::imap_dfr(calc$portfolios, function(series, name) {
          latest_return <- NA_real_
          if (!is.null(series$cumulative_returns) && length(series$cumulative_returns) > 0) {
            latest_return <- tail(series$cumulative_returns, 1) * 100
          }
          tibble(key = name, latest_return = latest_return)
        })

        meta <- meta %>% left_join(returns_df, by = "key")
      } else {
        meta$latest_return <- NA_real_
      }

      meta %>% arrange(portfolio_name, desc(start_date))
    })

    output$history_table <- DT::renderDataTable({
      data <- history_data()

      if (nrow(data) == 0) {
        return(DT::datatable(
          data.frame(Message = "No portfolio history available"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      display <- data %>%
        mutate(
          Rebalance = format(start_date, "%Y-%m-%d"),
          Holdings = holdings,
          `Latest Return (%)` = ifelse(is.na(latest_return), "N/A", sprintf("%.2f", latest_return)),
          `Version Key` = key
        ) %>%
        select(Portfolio = portfolio_name, Version = version_label, Rebalance, Holdings, `Latest Return (%)`, `Version Key`)

      DT::datatable(
        display,
        options = list(pageLength = 25, order = list(list(0, 'asc'), list(2, 'desc'))),
        rownames = FALSE
      )
    })
  })
}
