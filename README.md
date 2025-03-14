<p align="right">
  <a href="https://CRAN.R-project.org/package=KrakenR">
    <img src="https://cranlogs.r-pkg.org/badges/grand-total/KrakenR" alt="CRAN total downloads">
  </a>
</p>

## Introduction

The KrakenR package provides a user-friendly interface to the Kraken cryptocurrency exchange's REST API. It allows R users to access real-time and historical market data, asset information, and exchange metrics, making it a valuable tool for financial analysts, traders, and researchers interested in the cryptocurrency markets.

This vignette will introduce key functionalities of the package, showcasing how to use the API functions to retrieve market data and more.

## Installation

You can install KrakenR directly from GitHub:

```{r install_load, eval=FALSE}
# Install the development version from GitHub
devtools::install_github("nathanael-g-durst/KrakenR")

```

```{r setup}
# Load the package
library(KrakenR)
```

## Overview of Available Functions

The package includes several key functions that allow users to fetch various types of data from the Kraken exchange:

  * `getAssets()` - Retrieve detailed asset information.
  * `getOB()` - Fetch order book data for a specific trading pair.
  * `getOHLC()` - Access OHLC (Open, High, Low, Close) data for a trading pair.
  * `getPairs()` - Retrieve tradable asset pair information.
  * `getSpreads()` - Fetch recent spread data.
  * `getStatus()` - Get the current system status of the Kraken exchange.
  * `getTickers()` - Retrieve detailed ticker information for trading pairs.
  * `getTime()` - Fetch the current Kraken server time.
  * `getTrades()` - Retrieve recent trade data for a trading pair.

## Example Workflow

This section provides a detailed walkthrough using KrakenR to access Kraken market data.

### 1. Fetching Asset Information

To retrieve detailed information about all available assets or specific ones:
```{r fetching_asset_information}
# Fetch all available assets
assets_all <- getAssets()

# Fetch data for specific assets
assets_specific <- getAssets(c("BTC", "ETH", "ADA"))

```

The getAssets() function returns a data frame containing asset information, such as the asset class, decimals, and status.

```{r fetching_asset_information_example, echo=FALSE}
# Preview of asset information
knitr::kable(head(assets_all))

```

### 2. Retrieving Order Book Data

You can use `getOB()` to fetch order book data for a specific trading pair:

```{r retrieving_order_book_data}
# Fetch order book data for ADAEUR pair
order_book <- getOB("ADAEUR")

# Fetch order book data with a limit on the number of orders
order_book_limited <- getOB("ADAEUR", count = 3)

```

The output includes bid and ask orders, sorted by price, and can be used for market analysis.

```{r retrieving_order_book_data_example, echo=FALSE}
# Preview of asset information
knitr::kable(head(order_book_limited))

```

### 3. Fetching OHLC Data

The `getOHLC()` function allows you to retrieve OHLC (Open, High, Low, Close) data for a given trading pair at various time intervals:

```{r fetching_ohlc_data}
# Fetch 1-minute interval OHLC data for ADAEUR
ohlc_data <- getOHLC("ADAEUR", interval = 1)

# Fetch 4-hour interval data
ohlc_data_4h <- getOHLC("ADAEUR", interval = 240)

```

This function is useful for technical analysis and charting.

```{r fetching_ohlc_data_example, echo=FALSE}
# Preview of OHLC data
knitr::kable(head(ohlc_data_4h))

```

### 4. Getting Asset Pair Information

To retrieve tradable asset pairs and their details:

```{r getting_asset_pair_information}
# Fetch all available asset pairs
pairs_all <- getPairs()

# Fetch information for a specific pair
pair_info <- getPairs(c("ADAEUR", "BTCUSD"))

```

You can also filter by specific details such as leverage, fees, or margin.

```{r getting_asset_pair_information_example, echo=FALSE}
# Preview of asset pair information in a simplified two-column table
data_frame <- data.frame(Column = names(pairs_all),
                         Example = as.character(pairs_all[1, ]))
knitr::kable(data_frame)

```

### 5. Fetching Spread Data

To get recent spread data for a trading pair:

```{r fetching_spread_data}
# Fetch spread data for ADAEUR
spread_data <- getSpreads("ADAEUR")

```

The spread data provides insight into the bid-ask spread over time, which is useful for liquidity analysis.

```{r fetching_spread_data_example, echo=FALSE}
# Preview of spread data
knitr::kable(head(spread_data))

```

### 6. Getting Ticker Information

To fetch real-time ticker information for trading pairs:

```{r getting_ticker_information}
# Fetch ticker information for all pairs
tickers_all <- getTickers()

# Fetch ticker information for specific pairs
tickers_specific <- getTickers(c("ADAEUR", "BTCUSD"))

```

This function provides real-time price, volume, and trading information.

```{r getting_ticker_information_example, echo=FALSE}
# Preview of asset pair information in a simplified two-column table
data_frame <- data.frame(Column = names(tickers_all),
                         Example = as.character(tickers_all[1, ]))
knitr::kable(data_frame)

```

### 7. Fetching Recent Trade Data

To retrieve recent trade data for a trading pair:

```{r fetching_recent_trade_data}
# Fetch recent trades for ADAEUR
recent_trades <- getTrades("ADAEUR")

# Fetch trades since a specific timestamp
recent_trades_since <- getTrades("ADAEUR", since = "2024-10-01 12:00:00")

```

```{r fetching_recent_trade_data_example, echo=FALSE}
# Preview of recent trades
knitr::kable(head(recent_trades))

```

### 8. Retrieving System Status

To check the operational status of the Kraken exchange:

```{r retrieving_system_status}
# Fetch both system status and timestamp
status_info <- getStatus()

# Fetch only system status
system_status <- getStatus("status")

```

```{r retrieving_system_status_example, echo=FALSE}
# Display system status
print(paste("System Status:", status_info))

```

### 9. Retrieving Server Time

You can check the current server time in UNIX or RFC formats:

```{r retrieving_server_time}
# Fetch UNIX time
server_time_unix <- getTime("unix")

# Fetch RFC 1123 time
server_time_rfc <- getTime("rfc")

```

```{r retrieving_server_time_example, echo=FALSE}
# Display server times
print(paste("UNIX Time:", server_time_unix))

```
