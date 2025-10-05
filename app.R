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
        width: 300px;
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
      .preset-actions {
        display: flex;
        gap: 8px;
        flex-wrap: wrap;
        align-items: center;
      }
      .preset-actions .form-group {
        flex: 1 1 100%;
        margin-bottom: 6px;
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
        display: flex;
        flex-direction: column;
      }
      .time-control-wrapper {
        margin-bottom: 16px;
        background-color: #fff;
        border: 1px solid #dee2e6;
        border-radius: 4px;
        padding: 12px 16px;
      }
      .time-control-wrapper h5 {
        margin-top: 0;
        font-size: 13px;
        font-weight: 600;
        letter-spacing: 0.03em;
        text-transform: uppercase;
        color: #6c757d;
      }
      .time-control-row {
        display: flex;
        flex-wrap: wrap;
        gap: 16px;
        align-items: center;
      }
      .time-control-row .preset-buttons {
        flex: 1 1 auto;
      }
      .time-control-row .preset-buttons .radio-inline {
        margin-right: 12px;
      }
      .time-control-row .date-range {
        min-width: 260px;
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
      .comparison-controls {
        margin-top: 12px;
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
        .time-control-row {
          flex-direction: column;
          align-items: flex-start;
        }
        .time-control-row .date-range {
          width: 100%;
          min-width: 0;
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
        h4("Portfolios"),
        selectizeInput(
          "sidebar_portfolio_groups",
          label = NULL,
          choices = character(0),
          multiple = TRUE,
          options = list(placeholder = "Select portfolios", plugins = list("remove_button"))
        )
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
      uiOutput("time_window_controls"),
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
        ),
        tabPanel(
          title = tagList(icon("user"), "My Portfolio"),
          div(class = "container-fluid", workspaceUI("workspace"))
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

  portfolio_metadata <- reactive({
    portfolios <- portfolios_reactive()
    if (length(portfolios) == 0) {
      return(tibble())
    }

    # Create metadata for individual versions
    version_meta <- purrr::imap_dfr(portfolios, function(def, key) {
      tibble(
        key = key,
        portfolio_name = if (!is.null(def$portfolio_name)) def$portfolio_name else key,
        version_label = if (!is.null(def$version_label)) def$version_label else format(as.Date(def$start_date), "%Y-%m-%d"),
        start_date = as.Date(def$start_date),
        is_stitched = FALSE
      )
    })

    # Create metadata for stitched portfolios
    stitched_meta <- version_meta %>%
      group_by(portfolio_name) %>%
      summarise(
        portfolio_name = portfolio_name[1],
        key = paste0(portfolio_name[1], "_stitched"),
        version_label = "Combined",
        start_date = min(start_date),
        is_stitched = TRUE,
        .groups = "drop"
      )

    # Combine both types of metadata
    bind_rows(version_meta, stitched_meta) %>%
      arrange(portfolio_name, desc(is_stitched), desc(start_date))
  })

  comparison_state <- reactiveValues(extra_ids = character())
  time_state <- reactiveValues(range = NULL, preset = "max")

  observeEvent(portfolio_metadata(), {
    meta <- portfolio_metadata()
    groups <- sort(unique(meta$portfolio_name))
    selected <- input$sidebar_portfolio_groups

    if (length(groups) == 0) {
      updateSelectizeInput(
        session,
        "sidebar_portfolio_groups",
        choices = groups,
        selected = character(0),
        server = TRUE
      )
      return()
    }

    selected <- intersect(selected, groups)
    if (length(selected) == 0) {
      selected <- groups[1]
    }

    updateSelectizeInput(
      session,
      "sidebar_portfolio_groups",
      choices = groups,
      selected = selected,
      server = TRUE
    )

    earliest <- suppressWarnings(min(meta$start_date, na.rm = TRUE))
    if (!is.finite(earliest)) {
      earliest <- Sys.Date() - 365
    }
    if (is.null(time_state$range) || length(time_state$range) != 2) {
      time_state$range <- c(earliest, Sys.Date())
      time_state$preset <- "max"
    }
  }, ignoreNULL = FALSE)

  selected_groups <- reactive({
    groups <- input$sidebar_portfolio_groups
    if (is.null(groups) || length(groups) == 0) {
      meta <- portfolio_metadata()
      if (nrow(meta) == 0) {
        return(character())
      }
      return(meta$portfolio_name[1])
    }
    groups
  })

  observe({
    meta <- portfolio_metadata()
    if (nrow(meta) == 0) {
      comparison_state$extra_ids <- character()
      return()
    }
    groups <- selected_groups()
    valid <- meta %>%
      filter(key %in% comparison_state$extra_ids, portfolio_name %in% groups) %>%
      pull(key)
    if (!identical(sort(valid), sort(comparison_state$extra_ids))) {
      comparison_state$extra_ids <- unique(valid)
    }
  })

  output$time_window_controls <- renderUI({
    meta <- portfolio_metadata()
    if (nrow(meta) == 0) {
      return(NULL)
    }
    earliest <- suppressWarnings(min(meta$start_date, na.rm = TRUE))
    if (!is.finite(earliest)) {
      earliest <- Sys.Date() - 365
    }
    current_range <- time_state$range
    if (is.null(current_range) || length(current_range) != 2) {
      current_range <- c(earliest, Sys.Date())
    }
    preset <- time_state$preset
    if (is.null(preset) || !preset %in% c("ytd", "1y", "3y", "max", "custom")) {
      preset <- "max"
    }

    div(
      class = "time-control-wrapper",
      h5("Time Window"),
      div(
        class = "time-control-row",
        div(
          class = "preset-buttons",
          radioButtons(
            "time_window_preset",
            label = NULL,
            choices = c("YTD" = "ytd", "1Y" = "1y", "3Y" = "3y", "Max" = "max", "Custom" = "custom"),
            selected = preset,
            inline = TRUE
          )
        ),
        div(
          class = "date-range",
          dateRangeInput(
            "time_window_custom",
            label = NULL,
            start = current_range[1],
            end = current_range[2],
            min = earliest,
            max = Sys.Date()
          )
        )
      )
    )
  })

  observeEvent(input$time_window_preset, {
    preset <- input$time_window_preset
    meta <- portfolio_metadata()
    if (nrow(meta) == 0 || is.null(preset)) {
      return()
    }
    earliest <- suppressWarnings(min(meta$start_date, na.rm = TRUE))
    if (!is.finite(earliest)) {
      earliest <- Sys.Date() - 365
    }
    end_date <- Sys.Date()
    if (preset == "custom") {
      time_state$preset <- "custom"
      return()
    }
    start_date <- switch(preset,
      ytd = as.Date(paste0(format(end_date, "%Y"), "-01-01")),
      `1y` = end_date - 365,
      `3y` = end_date - (365 * 3),
      max = earliest,
      earliest
    )
    start_date <- max(start_date, earliest, na.rm = TRUE)
    if (start_date > end_date) {
      start_date <- earliest
    }
    time_state$range <- c(start_date, end_date)
    time_state$preset <- preset
    updateDateRangeInput(session, "time_window_custom", start = start_date, end = end_date)
  })

  observeEvent(input$time_window_custom, {
    range <- input$time_window_custom
    if (is.null(range) || any(is.na(range))) {
      return()
    }
    time_state$range <- as.Date(range)
    time_state$preset <- "custom"
    updateRadioButtons(session, "time_window_preset", selected = "custom")
  }, ignoreNULL = TRUE)

  output$sidebar_status <- renderUI({
    meta <- portfolio_metadata()
    if (nrow(meta) == 0) {
      return(tags$p(class = "text-muted small mb-0", "No portfolios loaded."))
    }
    groups_total <- length(unique(meta$portfolio_name))
    versions_total <- nrow(meta)
    selected <- selected_groups()
    tags$p(
      class = "text-muted small mb-0",
      sprintf("%d portfolio groups | %d versions | Selected: %s", groups_total, versions_total, paste(selected, collapse = ", "))
    )
  })

  observeEvent(input$sidebar_clear_cache, {
    clear_stock_data_cache()
    showNotification("Cleared cached price data.", type = "message")
  })

  observeEvent(input$sidebar_reload, {
    showNotification("Reloaded portfolios from portfolio.xlsx.", type = "message")
  })

  latest_version_keys <- reactive({
    meta <- portfolio_metadata()
    groups <- selected_groups()
    if (length(groups) == 0 || nrow(meta) == 0) {
      return(character())
    }
    # Return stitched keys instead of individual version keys
    paste0(groups, "_stitched")
  })

  selection_state <- reactive({
    meta <- portfolio_metadata()
    groups <- selected_groups()
    active_ids <- latest_version_keys()
    comparison_ids <- intersect(comparison_state$extra_ids, meta$key)
    list(
      metadata = meta,
      selected_group = if (length(groups) > 0) groups[1] else NULL,
      selected_groups = groups,
      active_ids = active_ids,
      selected_ids = unique(c(active_ids, comparison_ids)),
      comparison_ids = comparison_ids,
      show_sp500 = isTRUE(input$sidebar_show_sp500),
      show_btc = isTRUE(input$sidebar_show_btc),
      date_range = time_state$range,
      time_preset = time_state$preset
    )
  })

  portfolio_calc <- calculate_all_portfolios_with_inheritance

  portfolio_data <- reactive({
    sel <- selection_state()
    selected <- sel$selected_ids
    if (length(selected) == 0) {
      return(NULL)
    }

    # Filter out stitched keys and get actual portfolio keys
    meta <- portfolio_metadata()
    actual_keys <- meta %>%
      filter(key %in% selected, is_stitched == FALSE) %>%
      pull(key)

    # Also get all keys for selected groups to ensure stitching works
    groups <- selected_groups()
    group_keys <- meta %>%
      filter(portfolio_name %in% groups, is_stitched == FALSE) %>%
      pull(key)

    all_keys <- unique(c(actual_keys, group_keys))

    # Debug output
    cat("Selected IDs:", paste(selected, collapse=", "), "\n")
    cat("Actual keys to calculate:", paste(all_keys, collapse=", "), "\n")

    tryCatch({
      result <- portfolio_calc(
        portfolios = portfolios_reactive(),
        selected_portfolios = all_keys,
        show_sp500 = sel$show_sp500,
        show_btc = sel$show_btc
      )

      # Debug: print what portfolios were calculated
      if (!is.null(result) && !is.null(result$portfolios)) {
        cat("Calculated portfolios:", paste(names(result$portfolios), collapse=", "), "\n")
      }

      result
    }, error = function(e) {
      warning(paste("Portfolio calculation error:", e$message))
      NULL
    })
  })

  set_comparisons <- function(ids) {
    ids <- unique(ids)
    meta <- portfolio_metadata()
    valid <- intersect(ids, meta$key)
    comparison_state$extra_ids <- valid
  }

  performanceServer(
    "performance",
    selection_state,
    portfolio_data,
    portfolio_metadata,
    comparison_state,
    set_comparisons
  )
  riskServer("risk", selection_state, portfolio_data)
  holdingsServer("holdings", portfolios_reactive, selection_state, portfolio_data)
  historyServer("history", portfolios_reactive, portfolio_calc)
  workspaceServer("workspace", portfolios_reactive, selection_state, portfolio_calc)
}

shinyApp(ui = ui, server = server)
