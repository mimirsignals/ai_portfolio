# R/data_manager.R - Portfolio Data Manager using R6

DataManager <- R6::R6Class("DataManager",
  public = list(
    #' Initialize DataManager
    #' @param csv_path Path to CSV file for persistence
    initialize = function(csv_path = "portfolios_data.csv") {
      private$csv_path <- csv_path
      private$ensure_data_directory()
      self$load_portfolios()
      
      # Create default portfolio if none exist
      if (length(private$portfolios) == 0) {
        private$create_default_portfolio()
      }
    },
    
    #' Get all portfolios
    #' @return List of portfolios
    get_portfolios = function() {
      private$portfolios
    },
    
    #' Get specific portfolio
    #' @param name Portfolio name
    #' @return Portfolio data or NULL
    get_portfolio = function(name) {
      private$portfolios[[name]]
    },
    
    #' Add new portfolio
    #' @param name Portfolio name
    #' @param symbols Stock symbols
    #' @param start_date Start date
    #' @param total_investment Total investment amount
    #' @param weights Portfolio weights
    #' @return TRUE if successful, FALSE otherwise
    add_portfolio = function(name, symbols, start_date, total_investment, weights) {
      if (name %in% names(private$portfolios)) {
        warning(paste("Portfolio", name, "already exists"))
        return(FALSE)
      }
      
      # Validate inputs
      if (length(symbols) == 0) {
        warning("Portfolio must have at least one symbol")
        return(FALSE)
      }
      
      if (length(weights) != length(symbols)) {
        warning("Number of weights must match number of symbols")
        return(FALSE)
      }
      
      if (total_investment <= 0) {
        warning("Total investment must be positive")
        return(FALSE)
      }
      
      # Normalize weights
      weights <- weights / sum(weights)
      
      private$portfolios[[name]] <- list(
        symbols = symbols,
        start_date = as.Date(start_date),
        total_investment = total_investment,
        weights = weights,
        created_at = Sys.time(),
        modified_at = Sys.time()
      )
      
      private$save_portfolios()
      message(paste("Portfolio", name, "added successfully"))
      return(TRUE)
    },
    
    #' Update existing portfolio
    #' @param name Portfolio name
    #' @param symbols Stock symbols
    #' @param start_date Start date
    #' @param total_investment Total investment amount
    #' @param weights Portfolio weights
    #' @return TRUE if successful, FALSE otherwise
    update_portfolio = function(name, symbols = NULL, start_date = NULL, 
                               total_investment = NULL, weights = NULL) {
      if (!name %in% names(private$portfolios)) {
        warning(paste("Portfolio", name, "does not exist"))
        return(FALSE)
      }
      
      if (!is.null(symbols)) {
        if (length(symbols) == 0) {
          warning("Portfolio must have at least one symbol")
          return(FALSE)
        }
        private$portfolios[[name]]$symbols <- symbols
      }
      
      if (!is.null(start_date)) {
        private$portfolios[[name]]$start_date <- as.Date(start_date)
      }
      
      if (!is.null(total_investment)) {
        if (total_investment <= 0) {
          warning("Total investment must be positive")
          return(FALSE)
        }
        private$portfolios[[name]]$total_investment <- total_investment
      }
      
      if (!is.null(weights)) {
        current_symbols <- private$portfolios[[name]]$symbols
        if (length(weights) != length(current_symbols)) {
          warning("Number of weights must match number of symbols")
          return(FALSE)
        }
        # Normalize weights
        weights <- weights / sum(weights)
        private$portfolios[[name]]$weights <- weights
      }
      
      # Update modification time
      private$portfolios[[name]]$modified_at <- Sys.time()
      
      private$save_portfolios()
      message(paste("Portfolio", name, "updated successfully"))
      return(TRUE)
    },
    
    #' Remove portfolio
    #' @param name Portfolio name to remove
    #' @return TRUE if successful, FALSE otherwise
    remove_portfolio = function(name) {
      if (name == "Default Portfolio") {
        warning("Cannot delete the Default Portfolio")
        return(FALSE)
      }
      
      if (!name %in% names(private$portfolios)) {
        warning(paste("Portfolio", name, "does not exist"))
        return(FALSE)
      }
      
      private$portfolios[[name]] <- NULL
      private$save_portfolios()
      message(paste("Portfolio", name, "removed successfully"))
      return(TRUE)
    },
    
    #' Check if portfolio exists
    #' @param name Portfolio name
    #' @return TRUE if exists, FALSE otherwise
    portfolio_exists = function(name) {
      name %in% names(private$portfolios)
    },
    
    #' Get portfolio names
    #' @return Character vector of portfolio names
    get_portfolio_names = function() {
      names(private$portfolios)
    },
    
    #' Load portfolios from CSV
    load_portfolios = function() {
      if (file.exists(private$csv_path)) {
        tryCatch({
          portfolio_df <- read.csv(private$csv_path, stringsAsFactors = FALSE)
          
          private$portfolios <- list()
          
          for (i in seq_len(nrow(portfolio_df))) {
            row <- portfolio_df[i, ]
            
            # Parse symbols and weights
            symbols <- strsplit(row$symbols, "\\|")[[1]]
            weights <- as.numeric(strsplit(row$weights, "\\|")[[1]])
            
            # Validate data
            if (length(symbols) == 0 || length(weights) != length(symbols)) {
              warning(paste("Skipping invalid portfolio:", row$portfolio_name))
              next
            }
            
            # Normalize weights if needed
            if (abs(sum(weights) - 1) > 0.001) {
              weights <- weights / sum(weights)
            }
            
            private$portfolios[[row$portfolio_name]] <- list(
              symbols = symbols,
              start_date = as.Date(row$start_date),
              total_investment = row$total_investment,
              weights = weights,
              created_at = if ("created_at" %in% names(row) && !is.na(row$created_at)) {
                as.POSIXct(row$created_at)
              } else {
                Sys.time()
              },
              modified_at = if ("modified_at" %in% names(row) && !is.na(row$modified_at)) {
                as.POSIXct(row$modified_at)
              } else {
                Sys.time()
              }
            )
          }
          
          message(paste("Loaded", length(private$portfolios), "portfolios from", private$csv_path))
        }, error = function(e) {
          warning(paste("Error loading portfolios:", e$message))
          private$create_default_portfolio()
        })
      } else {
        message("No existing portfolio file found")
        private$create_default_portfolio()
      }
    },
    
    #' Get portfolio summary
    #' @return Data frame with portfolio summaries
    get_portfolio_summary = function() {
      if (length(private$portfolios) == 0) {
        return(data.frame())
      }
      
      portfolio_names <- names(private$portfolios)
      
      data.frame(
        Portfolio = portfolio_names,
        Stocks = sapply(portfolio_names, function(name) {
          paste(private$portfolios[[name]]$symbols, collapse = ", ")
        }),
        Start_Date = sapply(portfolio_names, function(name) {
          as.character(private$portfolios[[name]]$start_date)
        }),
        Investment = sapply(portfolio_names, function(name) {
          paste0("$", formatC(private$portfolios[[name]]$total_investment,
                             format = "f", digits = 0, big.mark = ","))
        }),
        stringsAsFactors = FALSE
      )
    }
  ),
  
  private = list(
    csv_path = NULL,
    portfolios = list(),
    
    #' Ensure data directory exists
    ensure_data_directory = function() {
      dir_path <- dirname(private$csv_path)
      if (!dir.exists(dir_path)) {
        dir.create(dir_path, recursive = TRUE)
        message(paste("Created directory:", dir_path))
      }
    },
    
    #' Save portfolios to CSV
    save_portfolios = function() {
      tryCatch({
        if (length(private$portfolios) > 0) {
          portfolio_df <- data.frame(
            portfolio_name = character(0),
            symbols = character(0),
            start_date = character(0),
            total_investment = numeric(0),
            weights = character(0),
            created_at = character(0),
            modified_at = character(0),
            stringsAsFactors = FALSE
          )
          
          for (name in names(private$portfolios)) {
            portfolio_info <- private$portfolios[[name]]
            new_row <- data.frame(
              portfolio_name = name,
              symbols = paste(portfolio_info$symbols, collapse = "|"),
              start_date = as.character(portfolio_info$start_date),
              total_investment = portfolio_info$total_investment,
              weights = paste(round(portfolio_info$weights, 6), collapse = "|"),
              created_at = as.character(portfolio_info$created_at %||% Sys.time()),
              modified_at = as.character(portfolio_info$modified_at %||% Sys.time()),
              stringsAsFactors = FALSE
            )
            portfolio_df <- rbind(portfolio_df, new_row)
          }
          
          write.csv(portfolio_df, private$csv_path, row.names = FALSE)
          message(paste("Portfolios saved to", private$csv_path))
        } else {
          if (file.exists(private$csv_path)) {
            file.remove(private$csv_path)
            message(paste("Removed empty portfolio file:", private$csv_path))
          }
        }
      }, error = function(e) {
        warning(paste("Error saving portfolios:", e$message))
      })
    },
    
    #' Create default portfolio
    create_default_portfolio = function() {
      # Use APP_CONFIG if available, otherwise use fallback values
      default_symbols <- if (exists("APP_CONFIG") && !is.null(APP_CONFIG$default_symbols)) {
        APP_CONFIG$default_symbols
      } else {
        c("AAPL", "MSFT", "GOOGL", "AMZN", "META")
      }
      
      default_investment <- if (exists("APP_CONFIG") && !is.null(APP_CONFIG$default_investment)) {
        APP_CONFIG$default_investment
      } else {
        10000
      }
      
      default_start_offset <- if (exists("APP_CONFIG") && !is.null(APP_CONFIG$default_start_date_offset)) {
        APP_CONFIG$default_start_date_offset
      } else {
        365
      }
      
      private$portfolios[["Default Portfolio"]] <- list(
        symbols = default_symbols,
        start_date = Sys.Date() - default_start_offset,
        total_investment = default_investment,
        weights = rep(1/length(default_symbols), length(default_symbols)),
        created_at = Sys.time(),
        modified_at = Sys.time()
      )
      
      private$save_portfolios()
      message("Default portfolio created")
    }
  )
)

# Null-coalescing operator helper
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}