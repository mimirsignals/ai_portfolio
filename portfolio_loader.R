# R/portfolio_loader.R

#' Load and process portfolios from a CSV file
#'
#' Reads a CSV where each unique date signifies a distinct portfolio version.
#' It handles comma decimal separators for weights.
#'
#' @param file_path Path to the CSV file.
#' @param initial_investment The initial investment amount.
#' @return A named list of portfolio definitions, sorted by date.
load_portfolios_from_csv <- function(file_path, initial_investment = 10000) {
  if (!file.exists(file_path)) {
    shiny::showNotification(paste("Portfolio file not found:", file_path), type = "error")
    return(list())
  }
  
  tryCatch({
    portfolio_df <- readr::read_csv(
      file_path,
      locale = readr::locale(decimal_mark = ","),
      col_types = readr::cols(
        date = readr::col_date(format = "%Y-%m-%d"),
        symbol = readr::col_character(),
        weight = readr::col_double()
      )
    )

    portfolio_defs <- portfolio_df %>%
      dplyr::group_by(date) %>%
      dplyr::summarise(
        symbols = list(symbol),
        weights = list(weight),
        .groups = "drop"
      ) %>%
      dplyr::arrange(date)

    portfolios_list <- setNames(
      purrr::map(1:nrow(portfolio_defs), ~{
        row <- portfolio_defs[.x, ]
        weights <- unlist(row$weights)
        list(
          symbols = unlist(row$symbols),
          weights = weights / sum(weights),
          start_date = as.Date(row$date),
          total_investment = initial_investment
        )
      }),
      paste("Portfolio", portfolio_defs$date)
    )
    
    return(portfolios_list)
  }, error = function(e) {
    shiny::showNotification(paste("Error reading portfolio file:", e$message), type = "error")
    return(list())
  })
}