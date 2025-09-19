# R/portfolio_loader.R - Enhanced with European Decimal Format Handling

#' Safe numeric conversion function for European decimal format
#' @param x Vector that may contain European decimal format (commas instead of periods)
#' @return Numeric vector with proper decimal conversion
safe_numeric_convert <- function(x) {
  if (is.null(x)) return(numeric(0))
  
  # Convert to character first to handle any factor levels
  x_char <- as.character(x)
  
  # Replace commas with periods for European decimal format
  x_char <- gsub(",", ".", x_char)
  
  # Remove any thousand separators (spaces or dots) if they exist
  # This handles cases like "1.234,56" (European) or "1 234,56"
  x_char <- gsub("\\s+", "", x_char)  # Remove spaces
  
  # For cases where dots might be thousand separators and comma is decimal
  # Check if there are multiple dots and one comma (European format)
  has_comma_and_dots <- grepl(",", x_char) & grepl("\\.", x_char)
  if (any(has_comma_and_dots)) {
    # Remove dots (thousand separators) but keep comma as decimal
    x_char[has_comma_and_dots] <- gsub("\\.", "", x_char[has_comma_and_dots])
    x_char[has_comma_and_dots] <- gsub(",", ".", x_char[has_comma_and_dots])
  }
  
  # Convert to numeric
  x_numeric <- suppressWarnings(as.numeric(x_char))
  
  # Return the numeric vector
  return(x_numeric)
}

#' Load and process portfolios from an Excel file
#'
#' Reads an Excel file where each unique date signifies a distinct portfolio version.
#' Handles European decimal format (commas instead of periods).
#'
#' @param file_path Path to the Excel file.
#' @param initial_investment The initial investment amount.
#' @return A named list of portfolio definitions, sorted by date.
load_portfolios_from_excel <- function(file_path, initial_investment = 10000) {
  if (!file.exists(file_path)) {
    shiny::showNotification(paste("Portfolio file not found:", file_path), type = "error")
    return(list())
  }
  
  tryCatch({
    # Use readxl to read the Excel file
    portfolio_df <- readxl::read_excel(file_path)
    
    # Check if required columns exist
    required_cols <- c("date", "symbol", "weight")
    missing_cols <- required_cols[!required_cols %in% names(portfolio_df)]
    
    if (length(missing_cols) > 0) {
      shiny::showNotification(paste("Missing required columns:", paste(missing_cols, collapse = ", ")), type = "error")
      return(list())
    }
    
    # Enhanced cleaning and conversion for European decimal format
    portfolio_df <- portfolio_df %>%
      mutate(
        # Handle weight column with European decimal format
        weight = safe_numeric_convert(weight),
        # Ensure date is properly formatted
        date = as.Date(date),
        # Ensure symbol is character
        symbol = as.character(symbol)
      ) %>%
      # Filter out invalid data
      filter(
        !is.na(date),
        !is.na(symbol),
        symbol != "",
        !is.na(weight),
        is.finite(weight),
        weight > 0
      )
    
    # Check if we have any valid data after cleaning
    if (nrow(portfolio_df) == 0) {
      shiny::showNotification("No valid data found in portfolio file after cleaning", type = "error")
      return(list())
    }
    
    # Group by date and create portfolio definitions
    portfolio_defs <- portfolio_df %>%
      dplyr::group_by(date) %>%
      dplyr::summarise(
        symbols = list(symbol),
        weights = list(weight),
        .groups = "drop"
      ) %>%
      dplyr::arrange(date)
    
    # Validate that we have portfolio definitions
    if (nrow(portfolio_defs) == 0) {
      shiny::showNotification("No portfolio definitions could be created", type = "error")
      return(list())
    }
    
    # Create the portfolios list with enhanced validation
    portfolios_list <- setNames(
      purrr::map(1:nrow(portfolio_defs), ~{
        row <- portfolio_defs[.x, ]
        symbols <- unlist(row$symbols)
        weights <- unlist(row$weights)
        
        # Additional validation
        if (length(symbols) == 0 || length(weights) == 0) {
          warning(paste("Empty portfolio for date:", row$date))
          return(NULL)
        }
        
        if (length(symbols) != length(weights)) {
          warning(paste("Mismatched symbols and weights for date:", row$date))
          return(NULL)
        }
        
        # Ensure weights are numeric and valid
        weights <- safe_numeric_convert(weights)
        valid_indices <- !is.na(weights) & is.finite(weights) & weights > 0
        
        if (!any(valid_indices)) {
          warning(paste("No valid weights for date:", row$date))
          return(NULL)
        }
        
        # Filter to valid entries only
        symbols <- symbols[valid_indices]
        weights <- weights[valid_indices]
        
        # Normalize weights to sum to 1
        weights_sum <- sum(weights, na.rm = TRUE)
        if (weights_sum == 0) {
          warning(paste("Zero weight sum for date:", row$date))
          return(NULL)
        }
        
        weights <- weights / weights_sum
        
        list(
          symbols = symbols,
          weights = weights,
          start_date = as.Date(row$date),
          total_investment = as.numeric(initial_investment)
        )
      }),
      paste("Portfolio", portfolio_defs$date)
    )
    
    # Remove any NULL entries (failed validations)
    portfolios_list <- portfolios_list[!sapply(portfolios_list, is.null)]
    
    if (length(portfolios_list) == 0) {
      shiny::showNotification("No valid portfolios could be created after validation", type = "error")
      return(list())
    }
    
    # Success message
    shiny::showNotification(
      paste("Successfully loaded", length(portfolios_list), "portfolio(s) from", file_path), 
      type = "message"
    )
    
    return(portfolios_list)
    
  }, error = function(e) {
    shiny::showNotification(paste("Error reading portfolio file:", e$message), type = "error")
    return(list())
  })
}