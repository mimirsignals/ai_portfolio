# R/mod_holdings.R - Holdings Module

#' Holdings Module UI
#' @param id Module namespace ID
holdingsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "Portfolio Holdings Analysis",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        p("Select portfolios from the Performance Overview tab to view their holdings breakdown.")
      )
    ),
    fluidRow(
      uiOutput(ns("holdings_plots"))
    )
  )
}

#' Holdings Module Server
#' @param id Module namespace ID
#' @param portfolios_reactive Reactive expression containing portfolio data
#' @param portfolio_calc Portfolio calculator function
#' @param performance_selections Reactive with selected portfolios from performance module
holdingsServer <- function(id, portfolios_reactive, portfolio_calc, performance_selections) {
  moduleServer(id, function(input, output, session) {
    
    # Get portfolio data based on performance module selections
    portfolio_data <- reactive({
      req(performance_selections())
      
      selections <- performance_selections()
      
      if (length(selections$selected) == 0) {
        return(NULL)
      }
      
      portfolio_calc(
        portfolios = portfolios_reactive(),
        selected_portfolios = selections$selected,
        show_sp500 = FALSE,
        show_btc = FALSE
      )
    })
    
    # Dynamic holdings plots
    output$holdings_plots <- renderUI({
      data <- portfolio_data()
      if (is.null(data) || length(data$portfolios) == 0) {
        return(
          box(
            width = 12,
            p("No portfolios selected. Please select portfolios from the Performance Overview tab.")
          )
        )
      }
      
      plot_list <- list()
      
      # Create plots for each portfolio
      for (i in seq_along(names(data$portfolios))) {
        portfolio_name <- names(data$portfolios)[i]
        
        # Allocation pie chart
        plot_list[[paste0("pie_", i)]] <- box(
          title = paste("Allocation:", portfolio_name),
          status = "success",
          solidHeader = TRUE,
          width = 6,
          plotlyOutput(session$ns(paste0("holdings_pie_", i)))
        )
        
        # Individual performance chart
        plot_list[[paste0("perf_", i)]] <- box(
          title = paste("Individual Stock Performance:", portfolio_name),
          status = "primary",
          solidHeader = TRUE,
          width = 6,
          plotlyOutput(session$ns(paste0("individual_performance_", i)))
        )
        
        # Contribution chart
        plot_list[[paste0("contrib_", i)]] <- box(
          title = paste("Return Contribution:", portfolio_name),
          status = "info",
          solidHeader = TRUE,
          width = 6,
          plotlyOutput(session$ns(paste0("contribution_", i)))
        )
        
        # Holdings table
        plot_list[[paste0("table_", i)]] <- box(
          title = paste("Holdings Details:", portfolio_name),
          status = "warning",
          solidHeader = TRUE,
          width = 6,
          DT::dataTableOutput(session$ns(paste0("holdings_table_", i)))
        )
      }
      
      # Arrange plots in rows
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
    
    # Create dynamic plot outputs
    observe({
      data <- portfolio_data()
      if (is.null(data)) return()
      
      for (i in seq_along(names(data$portfolios))) {
        local({
          my_i <- i
          portfolio_name <- names(data$portfolios)[my_i]
          portfolio_info <- data$portfolios[[portfolio_name]]
          
          # Allocation pie chart
          output[[paste0("holdings_pie_", my_i)]] <- renderPlotly({
            create_allocation_pie(portfolio_info, portfolio_name)
          })
          
          # Individual performance chart
          output[[paste0("individual_performance_", my_i)]] <- renderPlotly({
            create_individual_performance_chart(portfolio_info, portfolio_name)
          })
          
          # Contribution chart
          output[[paste0("contribution_", my_i)]] <- renderPlotly({
            create_contribution_chart(portfolio_info, portfolio_name)
          })
          
          # Holdings table
          output[[paste0("holdings_table_", my_i)]] <- DT::renderDataTable({
            create_holdings_table(portfolio_info)
          })
        })
      }
    })
  })
}

# Helper functions for holdings visualizations

#' Create allocation pie chart
create_allocation_pie <- function(portfolio_info, portfolio_name) {
  # Get current values for each stock
  current_values <- portfolio_info$individual_stocks %>%
    group_by(symbol) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    select(symbol, investment)
  
  plot_ly(
    labels = current_values$symbol,
    values = current_values$investment,
    type = "pie",
    textinfo = "label+percent",
    textposition = "outside",
    marker = list(
      colors = RColorBrewer::brewer.pal(
        min(nrow(current_values), 12), 
        "Set3"
      )
    )
  ) %>%
    layout(
      title = paste("Current Allocation -", portfolio_name),
      showlegend = TRUE
    )
}

#' Create individual stock performance chart
create_individual_performance_chart <- function(portfolio_info, portfolio_name) {
  individual_data <- portfolio_info$individual_stocks %>%
    mutate(
      date = ymd(date),
      return_pct = (investment / (portfolio_info$total_investment * weight) - 1) * 100
    )
  
  plot_ly(individual_data, x = ~date, y = ~return_pct, color = ~symbol,
          type = "scatter", mode = "lines") %>%
    layout(
      title = paste("Individual Returns -", portfolio_name),
      xaxis = list(title = "Date"),
      yaxis = list(title = "Return (%)"),
      hovermode = "x unified"
    )
}

#' Create contribution chart
create_contribution_chart <- function(portfolio_info, portfolio_name) {
  # Calculate contribution to total return
  contributions <- portfolio_info$individual_stocks %>%
    group_by(symbol) %>%
    summarise(
      initial_value = first(portfolio_info$total_investment * weight),
      final_value = last(investment),
      contribution = final_value - initial_value,
      return_pct = (final_value / initial_value - 1) * 100,
      .groups = 'drop'
    ) %>%
    mutate(
      contribution_pct = contribution / sum(abs(contribution)) * 100
    )
  
  plot_ly(contributions, x = ~symbol, y = ~contribution,
          type = "bar",
          marker = list(
            color = ~contribution,
            colorscale = list(
              list(0, "red"),
              list(0.5, "white"),
              list(1, "green")
            ),
            showscale = TRUE
          ),
          text = ~paste("Return: ", round(return_pct, 1), "%<br>",
                       "Contribution: $", round(contribution, 0)),
          hovertemplate = "%{text}<extra></extra>") %>%
    layout(
      title = paste("Return Contribution by Stock -", portfolio_name),
      xaxis = list(title = "Stock Symbol"),
      yaxis = list(title = "Contribution ($)"),
      showlegend = FALSE
    )
}

#' Create holdings detail table
create_holdings_table <- function(portfolio_info) {
  holdings_summary <- portfolio_info$individual_stocks %>%
    group_by(symbol) %>%
    summarise(
      Weight = round(first(weight) * 100, 1),
      Initial_Investment = round(first(portfolio_info$total_investment * weight), 0),
      Current_Value = round(last(investment), 0),
      Total_Return = round((last(investment) / first(portfolio_info$total_investment * weight) - 1) * 100, 2),
      Daily_Volatility = round(sd(c(0, diff(investment)) / lag(investment)[-1], na.rm = TRUE) * 100, 2),
      .groups = 'drop'
    ) %>%
    arrange(desc(Current_Value))
  
  # Format for display
  holdings_display <- holdings_summary %>%
    mutate(
      Weight = paste0(Weight, "%"),
      Initial_Investment = paste0("$", formatC(Initial_Investment, format = "f", 
                                               digits = 0, big.mark = ",")),
      Current_Value = paste0("$", formatC(Current_Value, format = "f", 
                                          digits = 0, big.mark = ",")),
      Total_Return = paste0(Total_Return, "%"),
      Daily_Volatility = paste0(Daily_Volatility, "%")
    ) %>%
    rename(
      Symbol = symbol,
      `Weight (%)` = Weight,
      `Initial ($)` = Initial_Investment,
      `Current ($)` = Current_Value,
      `Return (%)` = Total_Return,
      `Volatility (%)` = Daily_Volatility
    )
  
  DT::datatable(
    holdings_display,
    options = list(
      dom = 't',
      pageLength = 20,
      ordering = TRUE
    ),
    rownames = FALSE
  )
}