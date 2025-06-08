# Load required libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(quantmod)
library(plotly)
library(DT)
library(lubridate)

# Define UI
ui <- dashboardPage(
    dashboardHeader(title = "Portfolio Performance Dashboard"),

    dashboardSidebar(
        sidebarMenu(
            menuItem("Performance Overview", tabName = "performance", icon = icon("chart-line")),
            menuItem("Risk Metrics", tabName = "risk", icon = icon("exclamation-triangle")),
            menuItem("Holdings", tabName = "holdings", icon = icon("pie-chart"))
        )
    ),

    dashboardBody(
        tabItems(
            # Performance tab
            tabItem(tabName = "performance",
                    fluidRow(
                        box(
                            title = "Portfolio vs S&P 500 Performance",
                            status = "primary",
                            solidHeader = TRUE,
                            width = 12,
                            plotlyOutput("performance_plot", height = "400px")
                        )
                    ),
                    fluidRow(
                        valueBoxOutput("portfolio_value"),
                        valueBoxOutput("sp500_value"),
                        valueBoxOutput("btc_value")
                    ),
                    fluidRow(
                        valueBoxOutput("excess_return_sp500"),
                        valueBoxOutput("excess_return_btc"),
                        valueBoxOutput("btc_vs_sp500")
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
                        box(
                            title = "Portfolio Allocation",
                            status = "success",
                            solidHeader = TRUE,
                            width = 6,
                            plotlyOutput("holdings_pie")
                        ),
                        box(
                            title = "Individual Stock Performance",
                            status = "success",
                            solidHeader = TRUE,
                            width = 6,
                            plotlyOutput("individual_performance")
                        )
                    ),
                    fluidRow(
                        box(
                            title = "Stock Details",
                            status = "info",
                            solidHeader = TRUE,
                            width = 12,
                            DT::dataTableOutput("stock_details")
                        )
                    )
            )
        )
    )
)

# Define server logic
server <- function(input, output, session) {

    # Portfolio data calculation (reactive)
    portfolio_data <- reactive({

        # Stock symbols
        symbols <- c("DDOG", "FSLR", "NET", "IOBT", "BEAM", "AMPX", "PGEN", "SERV", "QUBT", "INOD")

        # Download individual stock data
        data_ls <- list()
        successful_symbols <- c()

        for (i in 1:length(symbols)) {
            tryCatch({
                temp_data <- getSymbols(symbols[[i]],
                                        from = "2025-05-21",
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
                cat("Failed to download", symbols[[i]], "\n")
            })
        }

        # Combine all stock data
        data_tbl <- data_ls %>%
            bind_rows()

        # Calculate individual stock investments ($1000 each)
        individual_stocks <- data_tbl %>%
            arrange(symbol, date) %>%
            group_by(symbol) %>%
            mutate(
                growth = price / lag(price) - 1,
                growth = if_else(date == min(date), 0, growth),
                investment = 1000 * cumprod(1 + growth)
            ) %>%
            ungroup()

        # Calculate portfolio total
        portfolio_tbl <- individual_stocks %>%
            group_by(date) %>%
            summarise(investment = sum(investment), .groups = 'drop')

        # Download S&P 500 data
        sp500_data <- getSymbols("^GSPC",
                                 from = "2025-05-21",
                                 to = (today() + 1),
                                 auto.assign = FALSE)

        sp500_tbl <- sp500_data %>%
            as.data.frame() %>%
            rownames_to_column("date") %>%
            as_tibble() %>%
            select(date, contains("Close")) %>%
            set_names("date", "price") %>%
            mutate(symbol = "S&P500")

        sp500_tbl <- sp500_tbl %>%
            arrange(symbol, date) %>%
            group_by(symbol) %>%
            mutate(
                growth = price / lag(price) - 1,
                growth = if_else(date == min(date), 0, growth),
                sp500 = 10000 * cumprod(1 + growth)
            ) %>%
            group_by(date) %>%
            summarise(sp500 = sum(sp500), .groups = 'drop')

        # Download BTC data
        btc_data <- getSymbols("BTC-USD",
                               from = "2025-05-21",
                               to = (today() + 1),
                               auto.assign = FALSE)

        btc_tbl <- btc_data %>%
            as.data.frame() %>%
            rownames_to_column("date") %>%
            as_tibble() %>%
            select(date, contains("Close")) %>%
            set_names("date", "price") %>%
            mutate(symbol = "BTC")

        btc_tbl <- btc_tbl %>%
            arrange(symbol, date) %>%
            group_by(symbol) %>%
            mutate(
                growth = price / lag(price) - 1,
                growth = if_else(date == min(date), 0, growth),
                btc = 10000 * cumprod(1 + growth)
            ) %>%
            group_by(date) %>%
            summarise(btc = sum(btc), .groups = 'drop')

        # Combine portfolio, S&P 500, and BTC
        combined_data <- sp500_tbl %>%
            left_join(portfolio_tbl, by = "date") %>%
            left_join(btc_tbl, by = "date") %>%
            mutate(date = ymd(date))

        # Calculate daily returns for risk metrics
        portfolio_returns <- combined_data %>%
            mutate(
                portfolio_return = (investment / lag(investment)) - 1,
                sp500_return = (sp500 / lag(sp500)) - 1,
                btc_return = (btc / lag(btc)) - 1
            ) %>%
            filter(!is.na(portfolio_return))

        list(
            combined_data = combined_data,
            individual_stocks = individual_stocks,
            portfolio_returns = portfolio_returns,
            successful_symbols = successful_symbols,
            failed_symbols = setdiff(symbols, successful_symbols)
        )
    })

    # Main performance plot
    output$performance_plot <- renderPlotly({
        data <- portfolio_data()

        plot_data <- data$combined_data %>%
            pivot_longer(cols = c(sp500, investment, btc),
                         names_to = "type",
                         values_to = "value") %>%
            mutate(
                type = case_when(
                    type == "investment" ~ "Portfolio ($10,000)",
                    type == "sp500" ~ "S&P 500 ($10,000)",
                    type == "btc" ~ "Bitcoin ($10,000)"
                ),
                return_pct = (value / 10000 - 1) * 100
            )

        p <- plot_ly(plot_data, x = ~date, y = ~return_pct, color = ~type,
                     type = "scatter", mode = "lines") %>%
            layout(
                title = "Portfolio vs S&P 500 vs Bitcoin Performance (Starting May 21, 2025)",
                xaxis = list(title = "Date"),
                yaxis = list(title = "Return (%)"),
                hovermode = "x unified"
            )

        p
    })

    # Value boxes
    output$portfolio_value <- renderValueBox({
        data <- portfolio_data()
        current_value <- tail(data$combined_data$investment, 1)
        return_pct <- (current_value / 10000 - 1) * 100

        valueBox(
            value = paste0("$", formatC(round(current_value), format = "f", digits = 0, big.mark = ",")),
            subtitle = paste0("Portfolio Value (", round(return_pct, 1), "%)"),
            icon = icon("chart-line"),
            color = if (return_pct > 0) "green" else "red"
        )
    })

    output$sp500_value <- renderValueBox({
        data <- portfolio_data()
        current_value <- tail(data$combined_data$sp500, 1)
        return_pct <- (current_value / 10000 - 1) * 100

        valueBox(
            value = paste0("$", formatC(round(current_value), format = "f", digits = 0, big.mark = ",")),
            subtitle = paste0("S&P 500 Value (", round(return_pct, 1), "%)"),
            icon = icon("chart-area"),
            color = if (return_pct > 0) "green" else "red"
        )
    })

    output$btc_value <- renderValueBox({
        data <- portfolio_data()
        current_value <- tail(data$combined_data$btc, 1)
        return_pct <- (current_value / 10000 - 1) * 100

        valueBox(
            value = paste0("$", formatC(round(current_value), format = "f", digits = 0, big.mark = ",")),
            subtitle = paste0("Bitcoin Value (", round(return_pct, 1), "%)"),
            icon = icon("bitcoin"),
            color = if (return_pct > 0) "yellow" else "red"
        )
    })

    output$excess_return_sp500 <- renderValueBox({
        data <- portfolio_data()
        portfolio_return <- (tail(data$combined_data$investment, 1) / 10000 - 1) * 100
        sp500_return <- (tail(data$combined_data$sp500, 1) / 10000 - 1) * 100
        excess <- portfolio_return - sp500_return

        valueBox(
            value = paste0(round(excess, 1), "%"),
            subtitle = "Portfolio vs S&P 500",
            icon = icon("balance-scale"),
            color = if (excess > 0) "green" else "red"
        )
    })

    output$excess_return_btc <- renderValueBox({
        data <- portfolio_data()
        portfolio_return <- (tail(data$combined_data$investment, 1) / 10000 - 1) * 100
        btc_return <- (tail(data$combined_data$btc, 1) / 10000 - 1) * 100
        excess <- portfolio_return - btc_return

        valueBox(
            value = paste0(round(excess, 1), "%"),
            subtitle = "Portfolio vs Bitcoin",
            icon = icon("balance-scale"),
            color = if (excess > 0) "green" else "red"
        )
    })

    output$btc_vs_sp500 <- renderValueBox({
        data <- portfolio_data()
        btc_return <- (tail(data$combined_data$btc, 1) / 10000 - 1) * 100
        sp500_return <- (tail(data$combined_data$sp500, 1) / 10000 - 1) * 100
        excess <- btc_return - sp500_return

        valueBox(
            value = paste0(round(excess, 1), "%"),
            subtitle = "Bitcoin vs S&P 500",
            icon = icon("exchange-alt"),
            color = if (excess > 0) "yellow" else "orange"
        )
    })

    # Performance metrics table
    output$metrics_table <- DT::renderDataTable({
        data <- portfolio_data()

        # Calculate metrics
        portfolio_total_return <- (tail(data$combined_data$investment, 1) / 10000 - 1) * 100
        sp500_total_return <- (tail(data$combined_data$sp500, 1) / 10000 - 1) * 100
        btc_total_return <- (tail(data$combined_data$btc, 1) / 10000 - 1) * 100

        # Calculate volatility (annualized)
        portfolio_vol <- sd(data$portfolio_returns$portfolio_return, na.rm = TRUE) * sqrt(252) * 100
        sp500_vol <- sd(data$portfolio_returns$sp500_return, na.rm = TRUE) * sqrt(252) * 100
        btc_vol <- sd(data$portfolio_returns$btc_return, na.rm = TRUE) * sqrt(252) * 100

        # Calculate Sharpe ratio (assuming 0% risk-free rate)
        portfolio_sharpe <- mean(data$portfolio_returns$portfolio_return, na.rm = TRUE) /
            sd(data$portfolio_returns$portfolio_return, na.rm = TRUE) * sqrt(252)
        sp500_sharpe <- mean(data$portfolio_returns$sp500_return, na.rm = TRUE) /
            sd(data$portfolio_returns$sp500_return, na.rm = TRUE) * sqrt(252)
        btc_sharpe <- mean(data$portfolio_returns$btc_return, na.rm = TRUE) /
            sd(data$portfolio_returns$btc_return, na.rm = TRUE) * sqrt(252)

        # Calculate max drawdown
        portfolio_cumulative <- cumprod(1 + data$portfolio_returns$portfolio_return)
        sp500_cumulative <- cumprod(1 + data$portfolio_returns$sp500_return)
        btc_cumulative <- cumprod(1 + data$portfolio_returns$btc_return)

        portfolio_drawdown <- min((portfolio_cumulative / cummax(portfolio_cumulative) - 1) * 100, na.rm = TRUE)
        sp500_drawdown <- min((sp500_cumulative / cummax(sp500_cumulative) - 1) * 100, na.rm = TRUE)
        btc_drawdown <- min((btc_cumulative / cummax(btc_cumulative) - 1) * 100, na.rm = TRUE)

        metrics_df <- data.frame(
            Metric = c("Total Return", "Annualized Volatility", "Sharpe Ratio", "Max Drawdown"),
            Portfolio = c(
                paste0(round(portfolio_total_return, 2), "%"),
                paste0(round(portfolio_vol, 2), "%"),
                round(portfolio_sharpe, 3),
                paste0(round(portfolio_drawdown, 2), "%")
            ),
            SP500 = c(
                paste0(round(sp500_total_return, 2), "%"),
                paste0(round(sp500_vol, 2), "%"),
                round(sp500_sharpe, 3),
                paste0(round(sp500_drawdown, 2), "%")
            ),
            Bitcoin = c(
                paste0(round(btc_total_return, 2), "%"),
                paste0(round(btc_vol, 2), "%"),
                round(btc_sharpe, 3),
                paste0(round(btc_drawdown, 2), "%")
            )
        )

        DT::datatable(metrics_df, options = list(dom = 't'))
    })

    # Returns distribution plot
    output$returns_distribution <- renderPlotly({
        data <- portfolio_data()

        plot_data <- data$portfolio_returns %>%
            select(date, portfolio_return, sp500_return, btc_return) %>%
            pivot_longer(cols = c(portfolio_return, sp500_return, btc_return),
                         names_to = "type", values_to = "return") %>%
            mutate(
                return = return * 100,
                type = case_when(
                    type == "portfolio_return" ~ "Portfolio",
                    type == "sp500_return" ~ "S&P 500",
                    type == "btc_return" ~ "Bitcoin"
                )
            )

        p <- plot_ly(plot_data, x = ~return, color = ~type, type = "histogram",
                     alpha = 0.7, nbinsx = 30) %>%
            layout(
                title = "Distribution of Daily Returns",
                xaxis = list(title = "Daily Return (%)"),
                yaxis = list(title = "Frequency"),
                barmode = "overlay"
            )

        p
    })

    # Rolling volatility plot
    output$volatility_plot <- renderPlotly({
        data <- portfolio_data()

        if (nrow(data$portfolio_returns) > 30) {
            volatility_data <- data$portfolio_returns %>%
                arrange(date) %>%
                mutate(
                    portfolio_vol = zoo::rollapply(portfolio_return, width = 30, FUN = sd,
                                                   fill = NA, align = "right") * sqrt(252) * 100,
                    sp500_vol = zoo::rollapply(sp500_return, width = 30, FUN = sd,
                                               fill = NA, align = "right") * sqrt(252) * 100,
                    btc_vol = zoo::rollapply(btc_return, width = 30, FUN = sd,
                                             fill = NA, align = "right") * sqrt(252) * 100
                ) %>%
                filter(!is.na(portfolio_vol))

            p <- plot_ly(volatility_data, x = ~date) %>%
                add_trace(y = ~portfolio_vol, name = "Portfolio", type = "scatter", mode = "lines") %>%
                add_trace(y = ~sp500_vol, name = "S&P 500", type = "scatter", mode = "lines") %>%
                add_trace(y = ~btc_vol, name = "Bitcoin", type = "scatter", mode = "lines") %>%
                layout(
                    title = "30-Day Rolling Volatility (Annualized)",
                    xaxis = list(title = "Date"),
                    yaxis = list(title = "Volatility (%)")
                )
        } else {
            p <- plotly_empty() %>% layout(title = "Not enough data for rolling volatility (need 30+ days)")
        }

        p
    })

    # Drawdown plot
    output$drawdown_plot <- renderPlotly({
        data <- portfolio_data()

        drawdown_data <- data$portfolio_returns %>%
            arrange(date) %>%
            mutate(
                portfolio_cumulative = cumprod(1 + portfolio_return),
                sp500_cumulative = cumprod(1 + sp500_return),
                btc_cumulative = cumprod(1 + btc_return),
                portfolio_drawdown = (portfolio_cumulative / cummax(portfolio_cumulative) - 1) * 100,
                sp500_drawdown = (sp500_cumulative / cummax(sp500_cumulative) - 1) * 100,
                btc_drawdown = (btc_cumulative / cummax(btc_cumulative) - 1) * 100
            )

        p <- plot_ly(drawdown_data, x = ~date) %>%
            add_trace(y = ~portfolio_drawdown, name = "Portfolio", type = "scatter", mode = "lines",
                      fill = "tonexty", fillcolor = "rgba(31, 119, 180, 0.3)") %>%
            add_trace(y = ~sp500_drawdown, name = "S&P 500", type = "scatter", mode = "lines",
                      fill = "tonexty", fillcolor = "rgba(255, 127, 14, 0.3)") %>%
            add_trace(y = ~btc_drawdown, name = "Bitcoin", type = "scatter", mode = "lines",
                      fill = "tonexty", fillcolor = "rgba(255, 193, 7, 0.3)") %>%
            layout(
                title = "Drawdown Analysis",
                xaxis = list(title = "Date"),
                yaxis = list(title = "Drawdown (%)")
            )

        p
    })

    # Holdings pie chart
    output$holdings_pie <- renderPlotly({
        data <- portfolio_data()

        plot_ly(
            labels = data$successful_symbols,
            values = rep(1000, length(data$successful_symbols)),
            type = "pie",
            textinfo = "label+percent",
            textposition = "outside"
        ) %>%
            layout(title = paste("Portfolio Allocation ($1000 per stock,", length(data$successful_symbols), "stocks)"))
    })

    # Individual stock performance
    output$individual_performance <- renderPlotly({
        data <- portfolio_data()

        individual_plot_data <- data$individual_stocks %>%
            mutate(
                date = ymd(date),
                return_pct = (investment / 1000 - 1) * 100
            )

        p <- plot_ly(individual_plot_data, x = ~date, y = ~return_pct, color = ~symbol,
                     type = "scatter", mode = "lines") %>%
            layout(
                title = "Individual Stock Performance ($1000 investment each)",
                xaxis = list(title = "Date"),
                yaxis = list(title = "Return (%)")
            )

        p
    })

    # Stock details table
    output$stock_details <- DT::renderDataTable({
        data <- portfolio_data()

        stock_summary <- data$individual_stocks %>%
            group_by(symbol) %>%
            summarise(
                current_value = tail(investment, 1),
                total_return = (tail(investment, 1) / 1000 - 1) * 100,
                start_price = head(price, 1),
                current_price = tail(price, 1),
                .groups = 'drop'
            ) %>%
            arrange(desc(total_return))

        stock_details <- stock_summary %>%
            mutate(
                Investment = "$1,000",
                Current_Value = paste0("$", formatC(round(current_value), format = "f", digits = 0, big.mark = ",")),
                Total_Return = paste0(round(total_return, 2), "%"),
                Start_Price = paste0("$", round(start_price, 2)),
                Current_Price = paste0("$", round(current_price, 2))
            ) %>%
            select(Symbol = symbol, Investment, Start_Price, Current_Price, Current_Value, Total_Return)

        # Add failed stocks info
        if (length(data$failed_symbols) > 0) {
            failed_info <- data.frame(
                Symbol = paste("Failed to download:", paste(data$failed_symbols, collapse = ", ")),
                Investment = "", Start_Price = "", Current_Price = "", Current_Value = "", Total_Return = ""
            )
            stock_details <- rbind(stock_details, failed_info)
        }

        DT::datatable(stock_details, options = list(pageLength = 15))
    })
}

# Run the application
shinyApp(ui = ui, server = server)
