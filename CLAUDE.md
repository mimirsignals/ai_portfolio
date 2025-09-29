# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Development Commands

This is an R Shiny application for portfolio monitoring and analysis. Common development tasks:

- **Run the application**: Open `app.R` in RStudio and click "Run App", or use `shiny::runApp(".")`
- **Install dependencies**: The app requires `shiny`, `tidyverse`, `quantmod`, `plotly`, `DT`, `readxl`, and `lubridate`
- **Reload portfolios**: Use the "Reload Portfolios" button in the app sidebar to refresh data from `portfolio.xlsx`
- **Clear cache**: Use "Clear Data Cache" button to clear stock price data cache

## Architecture Overview

### Core Application Structure
- **`app.R`**: Main Shiny application file containing UI layout and server logic
- **`global.R`**: Global dependencies, utilities, and module imports
- **`portfolio.xlsx`**: Excel file containing portfolio definitions and weights

### Module System
The application follows a modular architecture with dedicated modules for each main feature:

- **Performance Module** (`mod_performance.R`): Portfolio performance tracking and visualization vs benchmarks
- **Risk Module** (`mod_risk.R`): Risk analysis and metrics
- **Holdings Module** (`mod_holdings.R`): Portfolio composition and holdings analysis  
- **History Module** (`mod_history.R`): Historical portfolio transactions and rebalancing

### Core Utilities
- **`portfolio_loader.R`**: Excel file parsing with European decimal format support and flexible column mapping
- **`utils_data.R`**: Stock data fetching, caching, and portfolio performance calculations using quantmod
- **`utils_portfolio.R`**: Portfolio inheritance, rebalancing logic, and transaction tracking

### Data Flow
1. Portfolio definitions loaded from Excel via `load_portfolios_from_excel()`
2. Stock price data fetched and cached using quantmod through `fetch_stock_data()`
3. Portfolio calculations handle inheritance between versions using `calculate_all_portfolios_with_inheritance()`
4. Modules display results through reactive data flows

### Key Features
- **Portfolio Inheritance**: Newer portfolio versions inherit value from previous versions at rebalancing dates
- **European Number Format**: Supports comma-decimal Excel data (e.g., "1,234.56")
- **Flexible Column Mapping**: Automatically maps various column names to required fields
- **Benchmark Comparison**: Includes S&P 500 and Bitcoin benchmarks
- **Transaction Tracking**: Logs buy/sell actions during rebalancing

## Data Structure

The Excel file should contain columns for:
- Portfolio name (various aliases supported: "portfolio", "portfolioid", etc.)
- Rebalance date (various date formats supported)
- Symbol/ticker (aliases: "ticker", "asset", "isin")
- Weight/allocation (aliases: "allocation", "target_weight", etc.)
- Optional: Initial investment amount

Each unique combination of portfolio name and rebalance date creates a distinct portfolio version.

## Development Notes

- Stock data is cached in `.stock_data_cache` environment for performance
- The application uses a custom `box()` function as a lightweight replacement for shinydashboard
- Portfolio calculations handle missing data gracefully with extensive error checking
- All numeric conversions handle European decimal format automatically