# R/utils_persistence.R - Data Persistence Utilities

#' Save portfolios to CSV file
#' @param portfolios List of portfolio objects
#' @param file_path Path to CSV file
#' @param backup Whether to create a backup of existing file
#' @return TRUE if successful, FALSE otherwise
save_portfolios_to_csv <- function(portfolios, file_path, backup = TRUE) {
  
  tryCatch({
    # Create backup if requested and file exists
    if (backup && file.exists(file_path)) {
      backup_path <- paste0(file_path, ".backup_", format(Sys.time(), "%Y%m%d_%H%M%S"))
      file.copy(file_path, backup_path)
      message(paste("Backup created:", backup_path))
    }
    
    # Ensure directory exists
    dir_path <- dirname(file_path)
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
    }
    
    if (length(portfolios) > 0) {
      # Create data frame for CSV
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
      
      for (name in names(portfolios)) {
        portfolio_info <- portfolios[[name]]
        
        # Add timestamps if not present
        if (is.null(portfolio_info$created_at)) {
          portfolio_info$created_at <- Sys.time()
        }
        if (is.null(portfolio_info$modified_at)) {
          portfolio_info$modified_at <- Sys.time()
        }
        
        new_row <- data.frame(
          portfolio_name = name,
          symbols = paste(portfolio_info$symbols, collapse = "|"),
          start_date = as.character(portfolio_info$start_date),
          total_investment = portfolio_info$total_investment,
          weights = paste(round(portfolio_info$weights, 6), collapse = "|"),
          created_at = as.character(portfolio_info$created_at),
          modified_at = as.character(portfolio_info$modified_at),
          stringsAsFactors = FALSE
        )
        
        portfolio_df <- rbind(portfolio_df, new_row)
      }
      
      # Write to CSV
      write.csv(portfolio_df, file_path, row.names = FALSE)
      message(paste("Saved", nrow(portfolio_df), "portfolios to", file_path))
      return(TRUE)
      
    } else {
      # Remove file if no portfolios
      if (file.exists(file_path)) {
        file.remove(file_path)
        message("No portfolios to save - removed existing file")
      }
      return(TRUE)
    }
    
  }, error = function(e) {
    warning(paste("Error saving portfolios:", e$message))
    return(FALSE)
  })
}

#' Load portfolios from CSV file
#' @param file_path Path to CSV file
#' @param validate Whether to validate the loaded data
#' @return List of portfolio objects
load_portfolios_from_csv <- function(file_path, validate = TRUE) {
  
  portfolios <- list()
  
  if (!file.exists(file_path)) {
    message("Portfolio file not found:", file_path)
    return(portfolios)
  }
  
  tryCatch({
    # Read CSV
    portfolio_df <- read.csv(file_path, stringsAsFactors = FALSE)
    
    if (nrow(portfolio_df) == 0) {
      message("Empty portfolio file")
      return(portfolios)
    }
    
    # Process each row
    for (i in seq_len(nrow(portfolio_df))) {
      row <- portfolio_df[i, ]
      
      # Parse symbols and weights
      symbols <- strsplit(row$symbols, "\\|")[[1]]
      weights <- as.numeric(strsplit(row$weights, "\\|")[[1]])
      
      # Validate if requested
      if (validate) {
        if (length(symbols) == 0 || length(weights) != length(symbols)) {
          warning(paste("Invalid portfolio data for:", row$portfolio_name))
          next
        }
        
        # Normalize weights if they don't sum to 1
        weight_sum <- sum(weights)
        if (abs(weight_sum - 1) > 0.001) {
          warning(paste("Normalizing weights for:", row$portfolio_name))
          weights <- weights / weight_sum
        }
      }
      
      # Create portfolio object
      portfolio <- list(
        symbols = symbols,
        start_date = as.Date(row$start_date),
        total_investment = row$total_investment,
        weights = weights
      )
      
      # Add timestamps if available
      if ("created_at" %in% names(row) && !is.na(row$created_at)) {
        portfolio$created_at <- as.POSIXct(row$created_at)
      }
      if ("modified_at" %in% names(row) && !is.na(row$modified_at)) {
        portfolio$modified_at <- as.POSIXct(row$modified_at)
      }
      
      portfolios[[row$portfolio_name]] <- portfolio
    }
    
    message(paste("Loaded", length(portfolios), "portfolios from", file_path))
    
  }, error = function(e) {
    warning(paste("Error loading portfolios:", e$message))
  })
  
  return(portfolios)
}

#' Export portfolio performance to Excel
#' @param portfolio_data Portfolio performance data
#' @param file_path Path to Excel file
#' @param include_charts Whether to include charts
#' @return TRUE if successful, FALSE otherwise
export_portfolio_to_excel <- function(portfolio_data, file_path, include_charts = TRUE) {
  
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    warning("Package 'openxlsx' is required for Excel export")
    return(FALSE)
  }
  
  tryCatch({
    # Create workbook
    wb <- openxlsx::createWorkbook()
    
    # Add summary sheet
    openxlsx::addWorksheet(wb, "Summary")
    
    # Write portfolio summary
    summary_data <- data.frame(
      Metric = c("Portfolio Name", "Total Investment", "Current Value", 
                 "Total Return", "Number of Holdings"),
      Value = c(
        portfolio_data$portfolio_name,
        paste0("$", format(portfolio_data$total_investment, big.mark = ",")),
        paste0("$", format(tail(portfolio_data$portfolio_tbl$investment, 1), big.mark = ",")),
        paste0(round((tail(portfolio_data$portfolio_tbl$investment, 1) / 
                     portfolio_data$total_investment - 1) * 100, 2), "%"),
        length(portfolio_data$successful_symbols)
      )
    )
    
    openxlsx::writeData(wb, "Summary", summary_data)
    
    # Add performance data sheet
    openxlsx::addWorksheet(wb, "Performance")
    openxlsx::writeData(wb, "Performance", portfolio_data$portfolio_tbl)
    
    # Add holdings sheet
    if (!is.null(portfolio_data$individual_stocks)) {
      openxlsx::addWorksheet(wb, "Holdings")
      openxlsx::writeData(wb, "Holdings", portfolio_data$individual_stocks)
    }
    
    # Add metrics sheet
    metrics <- calculate_portfolio_metrics(portfolio_data)
    if (nrow(metrics) > 0) {
      openxlsx::addWorksheet(wb, "Metrics")
      openxlsx::writeData(wb, "Metrics", metrics)
    }
    
    # Save workbook
    openxlsx::saveWorkbook(wb, file_path, overwrite = TRUE)
    message(paste("Portfolio exported to:", file_path))
    return(TRUE)
    
  }, error = function(e) {
    warning(paste("Error exporting to Excel:", e$message))
    return(FALSE)
  })
}

#' Import portfolios from Excel file
#' @param file_path Path to Excel file
#' @param sheet_name Name of sheet containing portfolio data
#' @return List of portfolio objects
import_portfolios_from_excel <- function(file_path, sheet_name = "Portfolios") {
  
  if (!requireNamespace("readxl", quietly = TRUE)) {
    warning("Package 'readxl' is required for Excel import")
    return(list())
  }
  
  portfolios <- list()
  
  tryCatch({
    # Read Excel file
    portfolio_df <- readxl::read_excel(file_path, sheet = sheet_name)
    
    # Process similar to CSV import
    for (i in seq_len(nrow(portfolio_df))) {
      row <- portfolio_df[i, ]
      
      # Parse symbols and weights (assuming comma-separated in Excel)
      symbols <- trimws(unlist(strsplit(as.character(row$symbols), ",")))
      weights <- as.numeric(unlist(strsplit(as.character(row$weights), ",")))
      
      if (length(symbols) > 0 && length(weights) == length(symbols)) {
        # Normalize weights
        weights <- weights / sum(weights)
        
        portfolios[[row$portfolio_name]] <- list(
          symbols = symbols,
          start_date = as.Date(row$start_date),
          total_investment = as.numeric(row$total_investment),
          weights = weights
        )
      }
    }
    
    message(paste("Imported", length(portfolios), "portfolios from Excel"))
    
  }, error = function(e) {
    warning(paste("Error importing from Excel:", e$message))
  })
  
  return(portfolios)
}

#' Save portfolio performance history
#' @param portfolio_data Portfolio performance data
#' @param history_dir Directory to save history files
#' @param max_files Maximum number of history files to keep
#' @return TRUE if successful, FALSE otherwise
save_performance_history <- function(portfolio_data, history_dir = "data/history", max_files = 30) {
  
  tryCatch({
    # Ensure directory exists
    if (!dir.exists(history_dir)) {
      dir.create(history_dir, recursive = TRUE)
    }
    
    # Generate filename with timestamp
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    filename <- paste0(portfolio_data$portfolio_name, "_", timestamp, ".rds")
    file_path <- file.path(history_dir, filename)
    
    # Save as RDS for efficient storage
    saveRDS(portfolio_data, file_path)
    message(paste("Performance history saved to:", file_path))
    
    # Clean up old files if max_files is set
    if (max_files > 0) {
      pattern <- paste0("^", portfolio_data$portfolio_name, "_.*\\.rds$")
      existing_files <- list.files(history_dir, pattern = pattern, full.names = TRUE)
      
      if (length(existing_files) > max_files) {
        # Sort by modification time
        file_info <- file.info(existing_files)
        sorted_files <- existing_files[order(file_info$mtime)]
        
        # Remove oldest files
        files_to_remove <- sorted_files[1:(length(existing_files) - max_files)]
        file.remove(files_to_remove)
        message(paste("Removed", length(files_to_remove), "old history files"))
      }
    }
    
    return(TRUE)
    
  }, error = function(e) {
    warning(paste("Error saving performance history:", e$message))
    return(FALSE)
  })
}

#' Load portfolio performance history
#' @param portfolio_name Name of portfolio
#' @param history_dir Directory containing history files
#' @param n_files Number of recent files to load
#' @return List of historical performance data
load_performance_history <- function(portfolio_name, history_dir = "data/history", n_files = 10) {
  
  history_data <- list()
  
  if (!dir.exists(history_dir)) {
    message("History directory not found")
    return(history_data)
  }
  
  tryCatch({
    # Find history files for this portfolio
    pattern <- paste0("^", portfolio_name, "_.*\\.rds$")
    history_files <- list.files(history_dir, pattern = pattern, full.names = TRUE)
    
    if (length(history_files) == 0) {
      message("No history files found for portfolio:", portfolio_name)
      return(history_data)
    }
    
    # Sort by modification time (most recent first)
    file_info <- file.info(history_files)
    sorted_files <- history_files[order(file_info$mtime, decreasing = TRUE)]
    
    # Load requested number of files
    files_to_load <- head(sorted_files, n_files)
    
    for (file_path in files_to_load) {
      data <- readRDS(file_path)
      timestamp <- file_info[file_path, "mtime"]
      history_data[[as.character(timestamp)]] <- data
    }
    
    message(paste("Loaded", length(history_data), "history files for", portfolio_name))
    
  }, error = function(e) {
    warning(paste("Error loading performance history:", e$message))
  })
  
  return(history_data)
}

#' Create portfolio backup
#' @param portfolios List of portfolios
#' @param backup_dir Directory for backups
#' @param compress Whether to compress the backup
#' @return Path to backup file if successful, NULL otherwise
create_portfolio_backup <- function(portfolios, backup_dir = "data/backups", compress = TRUE) {
  
  tryCatch({
    # Ensure directory exists
    if (!dir.exists(backup_dir)) {
      dir.create(backup_dir, recursive = TRUE)
    }
    
    # Generate backup filename
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    filename <- paste0("portfolio_backup_", timestamp)
    
    if (compress) {
      # Save as compressed RDS
      file_path <- file.path(backup_dir, paste0(filename, ".rds"))
      saveRDS(portfolios, file_path, compress = TRUE)
    } else {
      # Save as CSV
      file_path <- file.path(backup_dir, paste0(filename, ".csv"))
      save_portfolios_to_csv(portfolios, file_path, backup = FALSE)
    }
    
    message(paste("Backup created:", file_path))
    return(file_path)
    
  }, error = function(e) {
    warning(paste("Error creating backup:", e$message))
    return(NULL)
  })
}

#' Restore portfolios from backup
#' @param backup_path Path to backup file
#' @return List of portfolios if successful, empty list otherwise
restore_portfolio_backup <- function(backup_path) {
  
  if (!file.exists(backup_path)) {
    warning("Backup file not found:", backup_path)
    return(list())
  }
  
  tryCatch({
    # Check file extension
    if (grepl("\\.rds$", backup_path)) {
      # Restore from RDS
      portfolios <- readRDS(backup_path)
    } else if (grepl("\\.csv$", backup_path)) {
      # Restore from CSV
      portfolios <- load_portfolios_from_csv(backup_path)
    } else {
      warning("Unsupported backup file format")
      return(list())
    }
    
    message(paste("Restored", length(portfolios), "portfolios from backup"))
    return(portfolios)
    
  }, error = function(e) {
    warning(paste("Error restoring backup:", e$message))
    return(list())
  })
}