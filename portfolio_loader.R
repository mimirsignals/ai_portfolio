# R/portfolio_loader.R - Enhanced with European Decimal Format Handling and flexible column support

#' Safe numeric conversion function for European decimal format
#' @param x Vector that may contain European decimal format (commas instead of periods)
#' @return Numeric vector with proper decimal conversion
safe_numeric_convert <- function(x) {
  if (is.null(x)) return(numeric(0))
  
  x_char <- as.character(x)
  x_char <- gsub("\u00A0", " ", x_char)  # replace non-breaking spaces
  x_char <- gsub(",", ".", x_char)
  x_char <- gsub("\\s+", "", x_char)
  
  has_comma_and_dots <- grepl(",", x_char) & grepl("\\.", x_char)
  if (any(has_comma_and_dots)) {
    x_char[has_comma_and_dots] <- gsub("\\.", "", x_char[has_comma_and_dots])
    x_char[has_comma_and_dots] <- gsub(",", ".", x_char[has_comma_and_dots])
  }
  
  suppressWarnings(as.numeric(x_char))
}

#' Standardise column names to lower snake_case
standardise_col_name <- function(x) {
  x <- trimws(x)
  x <- gsub("\u00A0", " ", x)  # non-breaking spaces
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("__+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x[x == ""] <- NA_character_
  x
}

#' Ensure a required column exists, optionally renaming aliases or creating defaults
ensure_column <- function(df, target, aliases = character(0), default = NULL) {
  if (target %in% names(df)) return(df)
  match_alias <- aliases[aliases %in% names(df)]
  if (length(match_alias) > 0) {
    names(df)[names(df) == match_alias[1]] <- target
    return(df)
  }
  if (!is.null(default)) {
    df[[target]] <- default
    return(df)
  }
  df
}

#' Load and process portfolios from an Excel file
#'
#' Reads an Excel file where each unique portfolio name and rebalance date
#' signify a distinct portfolio version. Handles European decimal format and
#' flexible column headings.
#'
#' @param file_path Path to the Excel file.
#' @param initial_investment The initial investment amount (fallback when not provided).
#' @return A named list of portfolio definitions, sorted by portfolio and date.
load_portfolios_from_excel <- function(file_path, initial_investment = 10000) {
  if (!file.exists(file_path)) {
    shiny::showNotification(paste("Portfolio file not found:", file_path), type = "error")
    return(list())
  }
  
  parse_rebalance_date <- function(x) {
    if (inherits(x, "Date")) {
      return(as.Date(x))
    }
    x_char <- as.character(x)
    x_char <- trimws(x_char)
    parsed <- suppressWarnings(as.Date(x_char, tryFormats = c("%Y-%m-%d", "%d-%m-%Y", "%d-%m-%y", "%m/%d/%Y", "%Y/%m/%d", "%d.%m.%Y", "%d.%m.%y")))
    needs_numeric <- is.na(parsed) & suppressWarnings(!is.na(as.numeric(x_char)))
    if (any(needs_numeric)) {
      parsed[needs_numeric] <- as.Date(as.numeric(x_char[needs_numeric]), origin = "1899-12-30")
    }
    parsed
  }
  
  tryCatch({
    portfolio_df <- readxl::read_excel(file_path)
    original_names <- names(portfolio_df)
    message(sprintf("portfolio_loader: detected columns -> %s", paste(original_names, collapse = ", ")))
    
    if (nrow(portfolio_df) == 0) {
      shiny::showNotification("Portfolio file is empty", type = "error")
      return(list())
    }
    
    clean_names <- standardise_col_name(original_names)
    if (any(is.na(clean_names))) {
      nm_map <- paste(sprintf("%s->%s", original_names, clean_names), collapse = ", ")
      warning(sprintf("portfolio_loader: unable to fully standardise column names (%s)", nm_map))
    }
    names(portfolio_df) <- clean_names
    message(sprintf("portfolio_loader: standardised columns -> %s", paste(names(portfolio_df), collapse = ", ")))
    
    portfolio_df <- ensure_column(
      portfolio_df,
      target = "portfolio_name",
      aliases = c("portfolio", "portfolioid", "portfolio_label", "name"),
      default = "Portfolio"
    )
    portfolio_df <- ensure_column(
      portfolio_df,
      target = "rebalance_date",
      aliases = c("date", "rebalance", "rebalance_dt", "rebalance_date_"),
      default = NA
    )
    portfolio_df <- ensure_column(
      portfolio_df,
      target = "symbol",
      aliases = c("ticker", "asset", "isin", "symbol_name"),
      default = NA
    )
    portfolio_df <- ensure_column(
      portfolio_df,
      target = "weight",
      aliases = c("allocation", "target_weight", "weight_pct", "weights"),
      default = NA
    )
    if (!"initial_investment" %in% names(portfolio_df)) {
      portfolio_df$initial_investment <- NA_real_
    }
    
    required_cols <- c("portfolio_name", "rebalance_date", "symbol", "weight")
    missing_cols <- required_cols[!required_cols %in% names(portfolio_df)]
    if (length(missing_cols) > 0) {
      shiny::showNotification(
        paste("Missing required columns:", paste(missing_cols, collapse = ", ")), type = "error"
      )
      return(list())
    }
    
    portfolio_df <- portfolio_df %>%
      mutate(
        portfolio_name = if_else(is.na(portfolio_name) | portfolio_name == "", "Portfolio", as.character(portfolio_name)),
        rebalance_date = parse_rebalance_date(rebalance_date),
        symbol = as.character(symbol),
        weight = safe_numeric_convert(weight),
        initial_investment = safe_numeric_convert(initial_investment)
      ) %>%
      filter(
        !is.na(portfolio_name),
        !is.na(rebalance_date),
        !is.na(symbol),
        symbol != "",
        !is.na(weight),
        is.finite(weight),
        weight > 0
      )
    
    message(sprintf("portfolio_loader: rows after cleaning -> %d", nrow(portfolio_df)))
    
    if (nrow(portfolio_df) == 0) {
      shiny::showNotification("No valid data found in portfolio file after cleaning", type = "error")
      return(list())
    }
    
    portfolio_index <- portfolio_df %>%
      dplyr::distinct(portfolio_name, rebalance_date) %>%
      dplyr::arrange(portfolio_name, rebalance_date)

    message(sprintf("portfolio_loader: distinct portfolios -> %d", nrow(portfolio_index)))

    if (nrow(portfolio_index) == 0) {
      shiny::showNotification("No portfolio definitions could be created", type = "error")
      return(list())
    }

    portfolios_list <- vector("list", nrow(portfolio_index))
    list_names <- character(nrow(portfolio_index))

    for (idx in seq_len(nrow(portfolio_index))) {
      row <- portfolio_index[idx, ]
      subset_df <- portfolio_df %>%
        dplyr::filter(portfolio_name == row$portfolio_name, rebalance_date == row$rebalance_date)

      symbols <- subset_df$symbol
      weights <- safe_numeric_convert(subset_df$weight)

      if (length(symbols) == 0 || length(weights) == 0 || length(symbols) != length(weights)) {
        warning(paste("Invalid portfolio definition for", row$portfolio_name, "on", row$rebalance_date))
        next
      }

      valid_indices <- !is.na(weights) & is.finite(weights) & weights > 0
      symbols <- symbols[valid_indices]
      weights <- weights[valid_indices]

      if (length(symbols) == 0) {
        warning(paste("No valid weights for", row$portfolio_name, "on", row$rebalance_date))
        next
      }

      weights <- weights / sum(weights, na.rm = TRUE)

      version_date <- as.Date(row$rebalance_date)
      version_label <- format(version_date, "%Y-%m-%d")
      portfolio_name <- as.character(row$portfolio_name)
      display_name <- paste(portfolio_name, version_label, sep = " - ")

      group_investment <- suppressWarnings(subset_df$initial_investment[!is.na(subset_df$initial_investment)])
      if (length(group_investment) == 0) {
        group_investment <- initial_investment
      } else {
        group_investment <- as.numeric(group_investment[1])
      }

      portfolios_list[[idx]] <- list(
        portfolio_name = portfolio_name,
        symbols = symbols,
        weights = weights,
        start_date = version_date,
        total_investment = group_investment,
        version_label = version_label
      )

      list_names[[idx]] <- display_name
    }

    valid_entries <- !vapply(portfolios_list, is.null, logical(1))
    portfolios_list <- portfolios_list[valid_entries]
    valid_entries <- !vapply(portfolios_list, is.null, logical(1))
    portfolios_list <- portfolios_list[valid_entries]
    list_names <- list_names[valid_entries]
    
    if (length(portfolios_list) == 0) {
      shiny::showNotification("No valid portfolios could be created after validation", type = "error")
      return(list())
    }
    
    names(portfolios_list) <- make.unique(list_names, sep = " #")
    
    shiny::showNotification(
      paste("Successfully loaded", length(portfolios_list), "portfolio(s) from", file_path),
      type = "message"
    )
    
    portfolios_list
    
  }, error = function(e) {
    shiny::showNotification(paste("Error reading portfolio file:", e$message), type = "error")
    return(list())
  })
}
