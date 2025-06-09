# Load required libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(quantmod)
library(plotly)
library(DT)
library(lubridate)
library(shinyWidgets)

# Define UI
ui <- dashboardPage(
    dashboardHeader(title = "Portfolio Performance Dashboard"),

    dashboardSidebar(
        sidebarMenu(
            menuItem("Performance Overview", tabName = "performance", icon = icon("chart-line")),
            menuItem("Risk Metrics", tabName = "risk", icon = icon("exclamation-triangle")),
            menuItem("Holdings", tabName = "holdings", icon = icon("pie-chart")),
            menuItem("Create Portfolio", tabName = "create", icon = icon("plus-circle")),
            menuItem("Manage Portfolios", tabName = "manage", icon = icon("cogs"))
        )
    ),

    dashboardBody(
        tabItems(
            # Performance tab
            tabItem(tabName = "performance",
                    fluidRow(
                        box(
                            title = "Portfolio Selection",
                            status = "info",
                            solidHeader = TRUE,
                            width = 12,
                            checkboxGroupInput("selected_portfolios", "Select Portfolios to Display:",
                                               choices = c(), selected = c(), inline = TRUE),
                            checkboxInput("show_sp500", "Show S&P 500", value = TRUE),
                            checkboxInput("show_btc", "Show Bitcoin", value = TRUE)
                        )
                    ),
                    fluidRow(
                        box(
                            title = "Portfolio Performance Comparison",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 12,
                            plotlyOutput("performance_plot", height = "400px")
                        )
                    ),
                    fluidRow(
                        uiOutput("portfolio_value_boxes")
                    ),
                    fluidRow(
                        box(
                            title = "Performance Metrics",
                            status = "info",
                            solidHeader = TRUE,
                            width = 12,
                            DT::dataTableOutput("metrics_table")
                        )
                    )
            ),

            # Risk tab
            tabItem(tabName = "risk",
                    fluidRow(
                        box(
                            title = "Daily Returns Distribution",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 6,
                            plotlyOutput("returns_distribution")
                        ),
                        box(
                            title = "Rolling Volatility (30 days)",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 6,
                            plotlyOutput("volatility_plot")
                        )
                    ),
                    fluidRow(
                        box(
                            title = "Drawdown Analysis",
                            status = "warning",
                            solidHeader = TRUE,
                            width = 12,
                            plotlyOutput("drawdown_plot")
                        )
                    )
            ),

            # Holdings tab
            tabItem(tabName = "holdings",
                    fluidRow(
                        uiOutput("holdings_plots")
                    )
            ),

            # Create Portfolio tab
            tabItem(tabName = "create",
                    fluidRow(
                        box(
                            title = "Portfolio Persistence Info",
                            status = "info",
                            solidHeader = TRUE,
                            width = 12,
                            p("📁 Portfolios are automatically saved to 'portfolios_data.csv' file"),
                            p("💾 Your portfolios will persist between app sessions"),
                            p("🗑️ Deleted portfolios are also removed from the CSV file")
                        )
                    ),
                    fluidRow(
                        box(
                            title = "Create New Portfolio",
                            status = "success",
                            solidHeader = TRUE,
                            width = 12,
                            textInput("portfolio_name", "Portfolio Name:",
                                      placeholder = "Enter a unique portfolio name"),

                            dateInput("start_date", "Start Date:",
                                      value = Sys.Date() - 365,
                                      max = Sys.Date()),

                            numericInput("total_investment", "Total Investment Amount ($):",
                                         value = 10000, min = 100, step = 100),

                            selectInput("allocation_method", "Allocation Method:",
                                        choices = list(
                                            "Equal Weight" = "equal",
                                            "Custom Weight" = "custom"
                                        )),

                            h4("Stock Selection:"),

                            fluidRow(
                                column(6,
                                       textInput("stock_input", "Add Stock Symbol:",
                                                 placeholder = "e.g., AAPL"),
                                       fluidRow(
                                           column(6, actionButton("add_stock", "Add Stock", class = "btn-primary")),
                                           column(6, actionButton("clear_stocks", "Clear All", class = "btn-warning"))
                                       )
                                ),
                                column(6,
                                       h5("Selected Stocks:"),
                                       verbatimTextOutput("selected_stocks_display")
                                )
                            ),

                            conditionalPanel(
                                condition = "input.allocation_method == 'custom'",
                                h4("Custom Weights:"),
                                uiOutput("custom_weights_ui")
                            ),

                            br(),
                            actionButton("create_portfolio", "Create Portfolio",
                                         class = "btn-success btn-lg"),

                            br(), br(),
                            verbatimTextOutput("creation_status")
                        )
                    )
            ),

            # Manage Portfolios tab
            tabItem(tabName = "manage",
                    fluidRow(
                        box(
                            title = "Manage Existing Portfolios",
                            status = "warning",
                            solidHeader = TRUE,
                            width = 12,
                            DT::dataTableOutput("portfolios_table"),
                            br(),
                            actionButton("delete_selected", "Delete Selected Portfolio",
                                         class = "btn-danger")
                        )
                    )
            )
        )
    )
)

# Define server logic
server <- function(input, output, session) {

    # CSV file path for storing portfolios
    portfolios_csv_path <- "portfolios_data.csv"

    # Reactive values to store portfolios
    portfolios <- reactiveValues(
        data = list(),
        loaded = FALSE
    )

    # Store selected stocks for portfolio creation
    selected_stocks <- reactiveVal(character(0))

    # Function to save portfolios to CSV
    save_portfolios_to_csv <- function() {
        if (length(portfolios$data) > 0) {
            portfolio_df <- data.frame(
                portfolio_name = character(0),
                symbols = character(0),
                start_date = character(0),
                total_investment = numeric(0),
                weights = character(0),
                stringsAsFactors = FALSE
            )

            for (name in names(portfolios$data)) {
                portfolio_info <- portfolios$data[[name]]
                new_row <- data.frame(
                    portfolio_name = name,
                    symbols = paste(portfolio_info$symbols, collapse = "|"),
                    start_date = as.character(portfolio_info$start_date),
                    total_investment = portfolio_info$total_investment,
                    weights = paste(portfolio_info$weights, collapse = "|"),
                    stringsAsFactors = FALSE
                )
                portfolio_df <- rbind(portfolio_df, new_row)
            }

            write.csv(portfolio_df, portfolios_csv_path, row.names = FALSE)
            cat("Portfolios saved to", portfolios_csv_path, "\n")
        } else {
            # If no portfolios, create empty CSV or delete existing one
            if (file.exists(portfolios_csv_path)) {
                file.remove(portfolios_csv_path)
                cat("Empty portfolios - removed", portfolios_csv_path, "\n")
            }
        }
    }

    # Function to load portfolios from CSV
    load_portfolios_from_csv <- function() {
        if (file.exists(portfolios_csv_path)) {
            tryCatch({
                portfolio_df <- read.csv(portfolios_csv_path, stringsAsFactors = FALSE)

                for (i in 1:nrow(portfolio_df)) {
                    row <- portfolio_df[i, ]
                    portfolios$data[[row$portfolio_name]] <- list(
                        symbols = strsplit(row$symbols, "\\|")[[1]],
                        start_date = as.Date(row$start_date),
                        total_investment = row$total_investment,
                        weights = as.numeric(strsplit(row$weights, "\\|")[[1]])
                    )
                }
                cat("Loaded", nrow(portfolio_df), "portfolios from", portfolios_csv_path, "\n")
            }, error = function(e) {
                cat("Error loading portfolios from CSV:", e$message, "\n")
                # Create default portfolio if loading fails
                create_default_portfolio()
            })
        } else {
            cat("No existing portfolio file found, creating default portfolio\n")
            create_default_portfolio()
        }
        portfolios$loaded <- TRUE
    }

    # Function to create default portfolio
    create_default_portfolio <- function() {
        default_symbols <- c("DDOG", "FSLR", "NET", "IOBT", "BEAM", "AMPX", "PGEN", "SERV", "QUBT", "INOD")
        portfolios$data[["Default Portfolio"]] <- list(
            symbols = default_symbols,
            start_date = as.Date("2025-05-21"),
            total_investment = 10000,
            weights = rep(1/length(default_symbols), length(default_symbols))
        )
        save_portfolios_to_csv()
    }

    # Load portfolios on startup
    observe({
        if (!portfolios$loaded) {
            load_portfolios_from_csv()
        }
    })

    # Update portfolio choices in UI
    observe({
        choices <- names(portfolios$data)
        selected <- if(length(choices) > 0) choices[1] else character(0)

        updateCheckboxGroupInput(session, "selected_portfolios",
                                 choices = choices,
                                 selected = selected)
    })

    # Add stock functionality
    observeEvent(input$add_stock, {
        if (input$stock_input != "") {
            current_stocks <- selected_stocks()
            new_stock <- toupper(trimws(input$stock_input))

            if (!new_stock %in% current_stocks) {
                selected_stocks(c(current_stocks, new_stock))
                updateTextInput(session, "stock_input", value = "")
            }
        }
    })

    # Clear stocks functionality
    observeEvent(input$clear_stocks, {
        selected_stocks(character(0))
    })

    # Display selected stocks
    output$selected_stocks_display <- renderText({
        stocks <- selected_stocks()
        if (length(stocks) > 0) {
            paste(stocks, collapse = ", ")
        } else {
            "No stocks selected"
        }
    })

    # Custom weights UI
    output$custom_weights_ui <- renderUI({
        stocks <- selected_stocks()
        if (length(stocks) > 0) {
            lapply(stocks, function(stock) {
                numericInput(paste0("weight_", stock),
                             paste("Weight for", stock, "(%)"),
                             value = round(100/length(stocks), 1),
                             min = 0, max = 100, step = 0.1)
            })
        }
    })

    # Create portfolio
    observeEvent(input$create_portfolio, {
        req(input$portfolio_name)
        req(length(selected_stocks()) > 0)

        # Validate portfolio name
        if (input$portfolio_name %in% names(portfolios$data)) {
            output$creation_status <- renderText("Error: Portfolio name already exists!")
            return()
        }

        # Get weights
        if (input$allocation_method == "equal") {
            weights <- rep(1/length(selected_stocks()), length(selected_stocks()))
        } else {
            # Custom weights
            weights <- sapply(selected_stocks(), function(stock) {
                weight_input <- input[[paste0("weight_", stock)]]
                if (is.null(weight_input)) 0 else weight_input
            })
            weights <- weights / sum(weights)  # Normalize to sum to 1
        }

        # Create portfolio
        portfolios$data[[input$portfolio_name]] <- list(
            symbols = selected_stocks(),
            start_date = input$start_date,
            total_investment = input$total_investment,
            weights = weights
        )

        # Save to CSV
        save_portfolios_to_csv()

        # Reset form
        selected_stocks(character(0))
        updateTextInput(session, "portfolio_name", value = "")
        output$creation_status <- renderText(paste("Portfolio", input$portfolio_name, "created and saved successfully!"))
    })

    # Portfolio data calculation (reactive)
    portfolio_data <- reactive({
        req(length(portfolios$data) > 0)

        selected_portfolios <- input$selected_portfolios
        if (length(selected_portfolios) == 0) return(NULL)

        all_portfolio_data <- list()
        all_returns_data <- list()

        # Process each selected portfolio
        for (portfolio_name in selected_portfolios) {
            portfolio_info <- portfolios$data[[portfolio_name]]
            symbols <- portfolio_info$symbols
            start_date <- portfolio_info$start_date
            total_investment <- portfolio_info$total_investment
            weights <- portfolio_info$weights

            # Download individual stock data
            data_ls <- list()
            successful_symbols <- c()

            for (i in 1:length(symbols)) {
                tryCatch({
                    temp_data <- getSymbols(symbols[[i]],
                                            from = start_date,
                                            to = (today() + 1),
                                            auto.assign = FALSE)

                    data_ls[[i]] <- temp_data %>%
                        as.data.frame() %>%
                        rownames_to_column("date") %>%
                        as_tibble() %>%
                        select(date, contains("Close")) %>%
                        set_names("date", "price") %>%
                        mutate(symbol = symbols[[i]])

                    successful_symbols <- c(successful_symbols, symbols[[i]])

                }, error = function(e) {
                    cat("Failed to download", symbols[[i]], "for portfolio", portfolio_name, "\n")
                })
            }

            if (length(data_ls) == 0) next

            # Combine all stock data
            data_tbl <- data_ls %>%
                bind_rows()

            # Calculate individual stock investments with custom weights
            individual_stocks <- data_tbl %>%
                arrange(symbol, date) %>%
                group_by(symbol) %>%
                mutate(
                    growth = price / lag(price) - 1,
                    growth = if_else(date == min(date), 0, growth)
                ) %>%
                ungroup()

            # Add weights to individual stocks
            weight_df <- data.frame(
                symbol = successful_symbols,
                weight = weights[match(successful_symbols, symbols)]
            )

            individual_stocks <- individual_stocks %>%
                left_join(weight_df, by = "symbol") %>%
                group_by(symbol) %>%
                mutate(
                    investment = total_investment * weight * cumprod(1 + growth)
                ) %>%
                ungroup()

            # Calculate portfolio total
            portfolio_tbl <- individual_stocks %>%
                group_by(date) %>%
                summarise(investment = sum(investment), .groups = 'drop') %>%
                mutate(portfolio_name = portfolio_name)

            all_portfolio_data[[portfolio_name]] <- list(
                portfolio_tbl = portfolio_tbl,
                individual_stocks = individual_stocks,
                successful_symbols = successful_symbols,
                total_investment = total_investment
            )
        }

        # Download benchmark data (S&P 500 and Bitcoin) if needed
        benchmark_data <- list()

        if (input$show_sp500 || input$show_btc) {
            # Find the earliest start date among selected portfolios
            earliest_date <- min(sapply(selected_portfolios, function(name) {
                portfolios$data[[name]]$start_date
            }))

            if (input$show_sp500) {
                sp500_data <- getSymbols("^GSPC",
                                         from = earliest_date,
                                         to = (today() + 1),
                                         auto.assign = FALSE)

                benchmark_data$sp500 <- sp500_data %>%
                    as.data.frame() %>%
                    rownames_to_column("date") %>%
                    as_tibble() %>%
                    select(date, contains("Close")) %>%
                    set_names("date", "price") %>%
                    arrange(date) %>%
                    mutate(
                        growth = price / lag(price) - 1,
                        growth = if_else(date == min(date), 0, growth),
                        sp500 = 10000 * cumprod(1 + growth)
                    ) %>%
                    select(date, sp500)
            }

            if (input$show_btc) {
                btc_data <- getSymbols("BTC-USD",
                                       from = earliest_date,
                                       to = (today() + 1),
                                       auto.assign = FALSE)

                benchmark_data$btc <- btc_data %>%
                    as.data.frame() %>%
                    rownames_to_column("date") %>%
                    as_tibble() %>%
                    select(date, contains("Close")) %>%
                    set_names("date", "price") %>%
                    arrange(date) %>%
                    mutate(
                        growth = price / lag(price) - 1,
                        growth = if_else(date == min(date), 0, growth),
                        btc = 10000 * cumprod(1 + growth)
                    ) %>%
                    select(date, btc)
            }
        }

        list(
            portfolios = all_portfolio_data,
            benchmarks = benchmark_data
        )
    })

    # Main performance plot
    output$performance_plot <- renderPlotly({
        data <- portfolio_data()
        if (is.null(data)) return(plotly_empty())

        # Combine all portfolio data
        all_data <- list()

        # Add portfolio data
        for (portfolio_name in names(data$portfolios)) {
            portfolio_info <- data$portfolios[[portfolio_name]]
            total_investment <- portfolio_info$total_investment

            portfolio_data_for_plot <- portfolio_info$portfolio_tbl %>%
                mutate(
                    type = portfolio_name,
                    return_pct = (investment / total_investment - 1) * 100,
                    date = ymd(date)
                ) %>%
                select(date, type, return_pct)

            all_data[[portfolio_name]] <- portfolio_data_for_plot
        }

        # Add benchmark data
        if (!is.null(data$benchmarks$sp500)) {
            sp500_plot_data <- data$benchmarks$sp500 %>%
                mutate(
                    type = "S&P 500",
                    return_pct = (sp500 / 10000 - 1) * 100,
                    date = ymd(date)
                ) %>%
                select(date, type, return_pct)

            all_data[["S&P 500"]] <- sp500_plot_data
        }

        if (!is.null(data$benchmarks$btc)) {
            btc_plot_data <- data$benchmarks$btc %>%
                mutate(
                    type = "Bitcoin",
                    return_pct = (btc / 10000 - 1) * 100,
                    date = ymd(date)
                ) %>%
                select(date, type, return_pct)

            all_data[["Bitcoin"]] <- btc_plot_data
        }

        if (length(all_data) == 0) return(plotly_empty())

        plot_data <- bind_rows(all_data)

        p <- plot_ly(plot_data, x = ~date, y = ~return_pct, color = ~type,
                     type = "scatter", mode = "lines") %>%
            layout(
                title = "Portfolio Performance Comparison",
                xaxis = list(title = "Date"),
                yaxis = list(title = "Return (%)"),
                hovermode = "x unified"
            )

        p
    })

    # Dynamic value boxes
    output$portfolio_value_boxes <- renderUI({
        data <- portfolio_data()
        if (is.null(data)) return(NULL)

        value_boxes <- list()

        # Portfolio value boxes
        for (portfolio_name in names(data$portfolios)) {
            portfolio_info <- data$portfolios[[portfolio_name]]
            current_value <- tail(portfolio_info$portfolio_tbl$investment, 1)
            total_investment <- portfolio_info$total_investment
            return_pct <- (current_value / total_investment - 1) * 100

            value_boxes[[portfolio_name]] <- valueBox(
                value = paste0("$", formatC(round(current_value), format = "f", digits = 0, big.mark = ",")),
                subtitle = paste0(portfolio_name, " (", round(return_pct, 1), "%)"),
                icon = icon("chart-line"),
                color = if (return_pct > 0) "green" else "red",
                width = 4
            )
        }

        # Benchmark value boxes
        if (!is.null(data$benchmarks$sp500) && input$show_sp500) {
            current_value <- tail(data$benchmarks$sp500$sp500, 1)
            return_pct <- (current_value / 10000 - 1) * 100

            value_boxes[["SP500"]] <- valueBox(
                value = paste0("$", formatC(round(current_value), format = "f", digits = 0, big.mark = ",")),
                subtitle = paste0("S&P 500 (", round(return_pct, 1), "%)"),
                icon = icon("chart-area"),
                color = if (return_pct > 0) "blue" else "red",
                width = 4
            )
        }

        if (!is.null(data$benchmarks$btc) && input$show_btc) {
            current_value <- tail(data$benchmarks$btc$btc, 1)
            return_pct <- (current_value / 10000 - 1) * 100

            value_boxes[["BTC"]] <- valueBox(
                value = paste0("$", formatC(round(current_value), format = "f", digits = 0, big.mark = ",")),
                subtitle = paste0("Bitcoin (", round(return_pct, 1), "%)"),
                icon = icon("bitcoin"),
                color = if (return_pct > 0) "yellow" else "red",
                width = 4
            )
        }

        do.call(fluidRow, value_boxes)
    })

    # Performance metrics table
    output$metrics_table <- DT::renderDataTable({
        data <- portfolio_data()
        if (is.null(data)) return(data.frame())

        metrics_list <- list()

        # Calculate metrics for each portfolio
        for (portfolio_name in names(data$portfolios)) {
            portfolio_info <- data$portfolios[[portfolio_name]]
            portfolio_tbl <- portfolio_info$portfolio_tbl
            total_investment <- portfolio_info$total_investment

            # Calculate returns
            returns <- portfolio_tbl %>%
                arrange(date) %>%
                mutate(
                    daily_return = (investment / lag(investment)) - 1
                ) %>%
                filter(!is.na(daily_return))

            if (nrow(returns) > 0) {
                total_return <- (tail(portfolio_tbl$investment, 1) / total_investment - 1) * 100
                volatility <- sd(returns$daily_return, na.rm = TRUE) * sqrt(252) * 100
                sharpe <- mean(returns$daily_return, na.rm = TRUE) / sd(returns$daily_return, na.rm = TRUE) * sqrt(252)

                # Calculate max drawdown
                cumulative <- cumprod(1 + returns$daily_return)
                max_drawdown <- min((cumulative / cummax(cumulative) - 1) * 100, na.rm = TRUE)

                metrics_list[[portfolio_name]] <- data.frame(
                    Portfolio = portfolio_name,
                    Total_Return = paste0(round(total_return, 2), "%"),
                    Volatility = paste0(round(volatility, 2), "%"),
                    Sharpe_Ratio = round(sharpe, 3),
                    Max_Drawdown = paste0(round(max_drawdown, 2), "%")
                )
            }
        }

        if (length(metrics_list) > 0) {
            metrics_df <- bind_rows(metrics_list)
            DT::datatable(metrics_df, options = list(dom = 't'))
        } else {
            DT::datatable(data.frame())
        }
    })

    # Holdings plots
    output$holdings_plots <- renderUI({
        data <- portfolio_data()
        if (is.null(data)) return(NULL)

        plot_list <- list()

        for (i in seq_along(names(data$portfolios))) {
            portfolio_name <- names(data$portfolios)[i]
            portfolio_info <- data$portfolios[[portfolio_name]]

            # Create pie chart for this portfolio
            plot_list[[paste0("pie_", i)]] <- box(
                title = paste("Portfolio Allocation:", portfolio_name),
                status = "success",
                solidHeader = TRUE,
                width = 6,
                plotlyOutput(paste0("holdings_pie_", i))
            )

            # Create individual performance chart
            plot_list[[paste0("perf_", i)]] <- box(
                title = paste("Individual Stock Performance:", portfolio_name),
                status = "success",
                solidHeader = TRUE,
                width = 6,
                plotlyOutput(paste0("individual_performance_", i))
            )
        }

        # Create plots in pairs (2 per row)
        rows <- list()
        for (i in seq(1, length(plot_list), by = 2)) {
            if (i + 1 <= length(plot_list)) {
                rows[[ceiling(i/2)]] <- fluidRow(plot_list[[i]], plot_list[[i+1]])
            } else {
                rows[[ceiling(i/2)]] <- fluidRow(plot_list[[i]])
            }
        }

        do.call(tagList, rows)
    })

    # Dynamic holdings plots
    observe({
        data <- portfolio_data()
        if (is.null(data)) return()

        for (i in seq_along(names(data$portfolios))) {
            local({
                my_i <- i
                portfolio_name <- names(data$portfolios)[my_i]
                portfolio_info <- data$portfolios[[portfolio_name]]

                output[[paste0("holdings_pie_", my_i)]] <- renderPlotly({
                    symbols <- portfolio_info$successful_symbols
                    weights <- portfolio_info$total_investment *
                        portfolio_info$portfolio_tbl$investment[1] / portfolio_info$total_investment *
                        rep(1/length(symbols), length(symbols))  # Simplified for display

                    plot_ly(
                        labels = symbols,
                        values = rep(1, length(symbols)),
                        type = "pie",
                        textinfo = "label+percent",
                        textposition = "outside"
                    ) %>%
                        layout(title = paste("Allocation for", portfolio_name))
                })

                output[[paste0("individual_performance_", my_i)]] <- renderPlotly({
                    individual_data <- portfolio_info$individual_stocks %>%
                        mutate(
                            date = ymd(date),
                            return_pct = (investment / (portfolio_info$total_investment * weight) - 1) * 100
                        )

                    plot_ly(individual_data, x = ~date, y = ~return_pct, color = ~symbol,
                            type = "scatter", mode = "lines") %>%
                        layout(
                            title = paste("Individual Performance -", portfolio_name),
                            xaxis = list(title = "Date"),
                            yaxis = list(title = "Return (%)")
                        )
                })
            })
        }
    })

    # Returns distribution (simplified for first portfolio)
    output$returns_distribution <- renderPlotly({
        data <- portfolio_data()
        if (is.null(data) || length(data$portfolios) == 0) return(plotly_empty())

        # Use first portfolio for returns distribution
        portfolio_info <- data$portfolios[[1]]
        returns <- portfolio_info$portfolio_tbl %>%
            arrange(date) %>%
            mutate(daily_return = (investment / lag(investment) - 1) * 100) %>%
            filter(!is.na(daily_return))

        plot_ly(x = ~returns$daily_return, type = "histogram", nbinsx = 30) %>%
            layout(
                title = "Distribution of Daily Returns",
                xaxis = list(title = "Daily Return (%)"),
                yaxis = list(title = "Frequency")
            )
    })

    # Rolling volatility (simplified)
    output$volatility_plot <- renderPlotly({
        data <- portfolio_data()
        if (is.null(data) || length(data$portfolios) == 0) return(plotly_empty())

        # Use first portfolio
        portfolio_info <- data$portfolios[[1]]
        returns <- portfolio_info$portfolio_tbl %>%
            arrange(date) %>%
            mutate(daily_return = investment / lag(investment) - 1) %>%
            filter(!is.na(daily_return))

        if (nrow(returns) > 30) {
            volatility_data <- returns %>%
                mutate(
                    volatility = zoo::rollapply(daily_return, width = 30, FUN = sd,
                                                fill = NA, align = "right") * sqrt(252) * 100,
                    date = ymd(date)
                ) %>%
                filter(!is.na(volatility))

            plot_ly(volatility_data, x = ~date, y = ~volatility, type = "scatter", mode = "lines") %>%
                layout(
                    title = "30-Day Rolling Volatility",
                    xaxis = list(title = "Date"),
                    yaxis = list(title = "Volatility (%)")
                )
        } else {
            plotly_empty() %>% layout(title = "Not enough data for rolling volatility")
        }
    })

    # Drawdown plot (simplified)
    output$drawdown_plot <- renderPlotly({
        data <- portfolio_data()
        if (is.null(data) || length(data$portfolios) == 0) return(plotly_empty())

        # Use first portfolio
        portfolio_info <- data$portfolios[[1]]
        returns <- portfolio_info$portfolio_tbl %>%
            arrange(date) %>%
            mutate(
                daily_return = investment / lag(investment) - 1,
                cumulative = cumprod(1 + coalesce(daily_return, 0)),
                drawdown = (cumulative / cummax(cumulative) - 1) * 100,
                date = ymd(date)
            )

        plot_ly(returns, x = ~date, y = ~drawdown, type = "scatter", mode = "lines",
                fill = "tonexty", fillcolor = "rgba(31, 119, 180, 0.3)") %>%
            layout(
                title = "Drawdown Analysis",
                xaxis = list(title = "Date"),
                yaxis = list(title = "Drawdown (%)")
            )
    })

    # Portfolios management table
    output$portfolios_table <- DT::renderDataTable({
        portfolio_names <- names(portfolios$data)

        if (length(portfolio_names) > 0) {
            portfolio_summary <- data.frame(
                Select = portfolio_names,
                Portfolio = portfolio_names,
                Stocks = sapply(portfolio_names, function(name) {
                    paste(portfolios$data[[name]]$symbols, collapse = ", ")
                }),
                Start_Date = sapply(portfolio_names, function(name) {
                    as.character(portfolios$data[[name]]$start_date)
                }),
                Investment = sapply(portfolio_names, function(name) {
                    paste0("$", formatC(portfolios$data[[name]]$total_investment,
                                        format = "f", digits = 0, big.mark = ","))
                }),
                stringsAsFactors = FALSE
            )

            DT::datatable(portfolio_summary,
                          selection = "single",
                          options = list(pageLength = 10))
        } else {
            DT::datatable(data.frame())
        }
    })

    # Delete portfolio
    observeEvent(input$delete_selected, {
        selected_row <- input$portfolios_table_rows_selected
        if (length(selected_row) > 0) {
            portfolio_names <- names(portfolios$data)
            portfolio_to_delete <- portfolio_names[selected_row]

            if (portfolio_to_delete != "Default Portfolio") {
                portfolios$data[[portfolio_to_delete]] <- NULL

                # Save updated portfolios to CSV
                save_portfolios_to_csv()

                # Show confirmation message
                showNotification(
                    paste("Portfolio '", portfolio_to_delete, "' deleted successfully!"),
                    type = "message",
                    duration = 3
                )
            } else {
                showNotification(
                    "Cannot delete the Default Portfolio!",
                    type = "warning",
                    duration = 3
                )
            }
        }
    })
}

# Run the application
shinyApp(ui = ui, server = server)
