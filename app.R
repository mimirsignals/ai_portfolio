# app.R

source("global.R")

ui <- fluidPage(
  tags$head(
    tags$title("Portfolio Monitor"),
    tags$style(HTML('
      html, body {
        height: 100%;
        background-color: #f5f7fb;
      }
      .app-container {
        display: flex;
        min-height: 100vh;
      }
      .app-sidebar {
        width: 280px;
        background-color: #f8f9fb;
        border-right: 1px solid #dee2e6;
        padding: 24px 20px;
        display: flex;
        flex-direction: column;
        gap: 12px;
      }
      .app-title {
        font-size: 20px;
        font-weight: 600;
        margin-bottom: 12px;
      }
      .sidebar-section {
        margin-bottom: 18px;
      }
      .sidebar-section h4 {
        font-size: 12px;
        letter-spacing: 0.08em;
        text-transform: uppercase;
        font-weight: 600;
        color: #6c757d;
        margin-bottom: 10px;
      }
      .sidebar-section .form-group {
        margin-bottom: 10px;
      }
      .sidebar-section .checkbox {
        margin-top: 8px;
        margin-bottom: 8px;
      }
      .sidebar-actions .btn {
        margin-right: 8px;
        margin-bottom: 8px;
      }
      .sidebar-actions .btn-link {
        padding-left: 0;
      }
      .sidebar-status {
        margin-top: auto;
      }
      .app-main {
        flex: 1;
        padding: 24px;
      }
      .app-main .nav-tabs {
        margin-bottom: 16px;
      }
      .panel {
        border-radius: 4px;
        border: 1px solid #ddd;
        box-shadow: 0 1px 1px rgba(0, 0, 0, 0.05);
        margin-bottom: 20px;
      }
      .panel-heading {
        padding: 10px 15px;
        border-bottom: 1px solid transparent;
        border-top-left-radius: 3px;
        border-top-right-radius: 3px;
      }
      .panel-heading-solid {
        color: #fff;
        font-weight: 600;
      }
      .panel-default .panel-heading-solid {
        background-color: #f5f5f5;
        color: #333;
      }
      .panel-primary .panel-heading-solid {
        background-color: #337ab7;
      }
      .panel-info .panel-heading-solid {
        background-color: #5bc0de;
      }
      .panel-success .panel-heading-solid {
        background-color: #5cb85c;
      }
      .panel-warning .panel-heading-solid {
        background-color: #f0ad4e;
      }
      .panel-danger .panel-heading-solid {
        background-color: #d9534f;
      }
      .panel-body {
        background-color: #fff;
        padding: 15px;
      }
      .selection-summary {
        margin-top: 10px;
      }
      .selection-summary p {
        margin: 0 0 6px;
      }
      .selection-summary ul {
        margin: 0;
        padding-left: 18px;
      }
      @media (max-width: 992px) {
        .app-container {
          flex-direction: column;
        }
        .app-sidebar {
          width: 100%;
          border-right: 0;
          border-bottom: 1px solid #dee2e6;
        }
      }
    '))
  ),
  div(
    class = "app-container",
    div(
      class = "app-sidebar",
      h2(class = "app-title", "Portfolio Monitor"),
      div(
        class = "sidebar-section",
        h4("Portfolio"),
        selectInput("sidebar_portfolio_group", label = NULL, choices = character(0))
      ),
      div(
        class = "sidebar-section",
        h4("Versions"),
        checkboxGroupInput("sidebar_selected_versions", label = NULL, choices = list(), inline = FALSE)
      ),
      div(
        class = "sidebar-section",
        h4("Benchmarks"),
        checkboxInput("sidebar_show_sp500", "Include S&P 500", TRUE),
        checkboxInput("sidebar_show_btc", "Include Bitcoin", TRUE)
      ),
      div(
        class = "sidebar-section sidebar-actions",
        actionButton("sidebar_reload", "Reload Portfolios", class = "btn btn-default btn-sm"),
        actionButton("sidebar_clear_cache", "Clear Data Cache", class = "btn btn-link btn-sm")
      ),
      div(class = "sidebar-status", uiOutput("sidebar_status"))
    ),
    div(
      class = "app-main",
      tabsetPanel(
        id = "main_tabs",
        tabPanel(
          title = tagList(icon("chart-line"), "Performance"),
          div(class = "container-fluid", performanceUI("performance"))
        ),
        tabPanel(
          title = tagList(icon("exclamation-triangle"), "Risk"),
          div(class = "container-fluid", riskUI("risk"))
        ),
        tabPanel(
          title = tagList(icon("pie-chart"), "Holdings"),
          div(class = "container-fluid", holdingsUI("holdings"))
        ),
        tabPanel(
          title = tagList(icon("clock-rotate-left"), "History"),
          div(class = "container-fluid", historyUI("history"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  portfolios_reactive <- reactive({
    input$sidebar_reload
    load_portfolios_from_excel("portfolio.xlsx")
  })

  # Preload stock data at startup and on reload
  observe({
    portfolios <- portfolios_reactive()

    if (length(portfolios) > 0) {
      all_symbols <- extract_all_symbols(portfolios)

      if (length(all_symbols) > 0) {
        withProgress(message = "Loading stock data...", value = 0, {
          preload_stock_data(
            symbols = all_symbols,
            start_date = "2025-05-20"
          )
        })

        showNotification(
          sprintf("Preloaded data for %d symbols", length(all_symbols)),
          type = "message",
          duration = 3
        )
      }
    }
  }) %>% bindEvent(portfolios_reactive(), ignoreNULL = TRUE, ignoreInit = FALSE)

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
      updateSelectInput(session, "sidebar_portfolio_group", choices = choices, selected = character(0))
      return()
    }

    selected <- input$sidebar_portfolio_group
    if (is.null(selected) || !selected %in% choices) {
      preferred <- choices[grepl('^high risk$', choices, ignore.case = TRUE)]
      if (length(preferred) > 0) {
        selected <- preferred[1]
      } else {
        selected <- choices[1]
      }
    }

    updateSelectInput(session, "sidebar_portfolio_group", choices = choices, selected = selected)
  })

  observeEvent(list(input$sidebar_portfolio_group, portfolio_metadata()), {
    meta <- portfolio_metadata()
    group <- input$sidebar_portfolio_group

    if (is.null(group) || nrow(meta) == 0 || !group %in% meta$portfolio_name) {
      updateCheckboxGroupInput(session, "sidebar_selected_versions", choices = list(), selected = character(0))
      return()
    }

    group_meta <- meta %>%
      filter(portfolio_name == group) %>%
      arrange(desc(start_date))

    choices <- stats::setNames(group_meta$key, group_meta$version_label)
    defaults <- input$sidebar_selected_versions
    if (is.null(defaults) || !all(defaults %in% group_meta$key)) {
      defaults <- group_meta$key  # Select ALL versions by default
    }

    updateCheckboxGroupInput(
      session,
      "sidebar_selected_versions",
      choices = choices,
      selected = defaults,
      inline = FALSE
    )
  }, ignoreNULL = FALSE)

  observeEvent(input$sidebar_clear_cache, {
    clear_stock_data_cache()
    showNotification("Cleared cached price data.", type = "message")
  })

  observeEvent(input$sidebar_reload, {
    showNotification("Reloaded portfolios from portfolio.xlsx.", type = "message")
  })

  output$sidebar_status <- renderUI({
    meta <- portfolio_metadata()
    if (nrow(meta) == 0) {
      tags$p(class = "text-muted small mb-0", "No portfolios loaded.")
    } else {
      groups <- length(unique(meta$portfolio_name))
      tags$p(
        class = "text-muted small mb-0",
        sprintf("%d portfolio groups | %d versions", groups, nrow(meta))
      )
    }
  })

  selection_state <- reactive({
    meta <- portfolio_metadata()
    group <- input$sidebar_portfolio_group
    versions <- input$sidebar_selected_versions
    valid_versions <- intersect(versions, meta$key)

    list(
      metadata = meta,
      selected_group = group,
      selected_ids = valid_versions,
      show_sp500 = isTRUE(input$sidebar_show_sp500),
      show_btc = isTRUE(input$sidebar_show_btc)
    )
  })

  portfolio_calc <- calculate_all_portfolios_with_inheritance

  portfolio_data <- reactive({
    sel <- selection_state()
    selected <- sel$selected_ids
    if (length(selected) == 0) {
      return(NULL)
    }

    tryCatch({
      portfolio_calc(
        portfolios = portfolios_reactive(),
        selected_portfolios = selected,
        show_sp500 = sel$show_sp500,
        show_btc = sel$show_btc
      )
    }, error = function(e) {
      warning(paste("Portfolio calculation error:", e$message))
      NULL
    })
  })

  performanceServer("performance", selection_state, portfolio_data)
  riskServer("risk", selection_state, portfolio_data)
  holdingsServer("holdings", portfolios_reactive, selection_state, portfolio_data)
  historyServer("history", portfolios_reactive, portfolio_calc)
}

shinyApp(ui = ui, server = server)

