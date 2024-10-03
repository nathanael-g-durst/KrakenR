#' Retrieve Ticker Information from Kraken Exchange
#'
#' This function fetches detailed ticker information from the Kraken API for specified trading pairs or all available pairs.
#'
#' @param pairs A character vector specifying the pairs to retrieve ticker information for. Use "All" to fetch data for all pairs.
#'               For specific pairs, provide their abbreviations (e.g., "ADAEUR" or c("ADAEUR", "BTCUSD")). Default is "All".
#'
#' @return A data frame containing detailed ticker information for the requested pairs with renamed and split columns, where applicable, and all numbers treated as numeric.
#' @export
#'
#' @examples
#' getTickers()
#' getTickers("ADAEUR")
#' getTickers(c("ADAEUR", "BTCUSD"))


getTickers <- function(pairs = "All") {

  # Validate pairs input
  if (!is.character(pairs) || length(pairs) < 1) {
    stop("Invalid input: 'pairs' must be a character vector with at least one element.")
  }

  # Build the base URL
  url <- "https://api.kraken.com/0/public/Ticker"

  # Add the pairs or set to all pairs
  if (pairs[1] != "All") {
    pairsList <- paste(pairs, collapse = ",")
    url <- paste0(url, "?pair=", pairsList)
  }

  # Fetch data from the Kraken API
  jsonFile <- tryCatch({
    jsonlite::fromJSON(url)
  }, error = function(e) {
    stop("Error fetching data from the Kraken API: ", e$message)
  })

  # Check for errors in the API response
  if (length(jsonFile[["error"]]) > 0 && jsonFile[["error"]][1] != "") {
    stop("API returned the following error(s): ", paste(jsonFile[["error"]], collapse = ", "))
  }

  # Check if the 'result' element exists
  if (is.null(jsonFile[["result"]]) || length(jsonFile[["result"]]) == 0) {
    stop("No ticker data returned from the API.")
  }

  # Extract the result list
  tickerList <- jsonFile[["result"]]

  # Initialize an empty list to store data frames
  result <- list()

  # Process each ticker pair
  for (pair in names(tickerList)) {
    ticker_data <- tryCatch({
      as.data.frame(t(tickerList[[pair]]), stringsAsFactors = FALSE)
    }, error = function(e) {
      warning(paste("Failed to process ticker pair:", pair, "with error:", e$message))
      return(NULL)  # Skip to the next pair in case of an error
    })

    # Add the pair name to the data
    if (!is.null(ticker_data)) {
      ticker_data$Pair <- pair
      result[[pair]] <- ticker_data
    }
  }

  # Check if any data was processed
  if (length(result) == 0) {
    stop("No valid ticker data could be processed.")
  }

  # Combine all results into a single data frame
  finalResult <- tryCatch({
    dplyr::bind_rows(result, .id = "PairID")
  }, error = function(e) {
    stop("Error combining the result data frames: ", e$message)
  })

  # Rename the columns to be more descriptive
  colnames(finalResult) <- c("PairID", "Ask", "Bid", "LastTradeClosed", "Volume", "VWAP", "Trades", "Low", "High", "Open", "Pair")

  # Split columns with multiple values (Ask, Bid, LastTradeClosed) and convert to numeric
  if ("Ask" %in% colnames(finalResult)) {
    finalResult <- dplyr::mutate(finalResult,
                                 Ask_Price = as.numeric(sapply(Ask, function(x) x[1])),
                                 Ask_WholeLotVolume = as.numeric(sapply(Ask, function(x) x[2])),
                                 Ask_LotVolume = as.numeric(sapply(Ask, function(x) x[3]))
    ) %>% dplyr::select(-Ask)
  }

  if ("Bid" %in% colnames(finalResult)) {
    finalResult <- dplyr::mutate(finalResult,
                                 Bid_Price = as.numeric(sapply(Bid, function(x) x[1])),
                                 Bid_WholeLotVolume = as.numeric(sapply(Bid, function(x) x[2])),
                                 Bid_LotVolume = as.numeric(sapply(Bid, function(x) x[3]))
    ) %>% dplyr::select(-Bid)
  }

  if ("LastTradeClosed" %in% colnames(finalResult)) {
    finalResult <- dplyr::mutate(finalResult,
                                 LastTrade_Price = as.numeric(sapply(LastTradeClosed, function(x) x[1])),
                                 LastTrade_LotVolume = as.numeric(sapply(LastTradeClosed, function(x) x[2]))
    ) %>% dplyr::select(-LastTradeClosed)
  }

  # Split other columns with today and last 24 hours data, and convert to numeric
  if ("Volume" %in% colnames(finalResult)) {
    finalResult <- dplyr::mutate(finalResult,
                                 Volume_Today = as.numeric(sapply(Volume, function(x) x[1])),
                                 Volume_24h = as.numeric(sapply(Volume, function(x) x[2]))
    ) %>% dplyr::select(-Volume)
  }

  if ("VWAP" %in% colnames(finalResult)) {
    finalResult <- dplyr::mutate(finalResult,
                                 VWAP_Today = as.numeric(sapply(VWAP, function(x) x[1])),
                                 VWAP_24h = as.numeric(sapply(VWAP, function(x) x[2]))
    ) %>% dplyr::select(-VWAP)
  }

  if ("Trades" %in% colnames(finalResult)) {
    finalResult <- dplyr::mutate(finalResult,
                                 Trades_Today = as.numeric(sapply(Trades, function(x) x[1])),
                                 Trades_24h = as.numeric(sapply(Trades, function(x) x[2]))
    ) %>% dplyr::select(-Trades)
  }

  if ("Low" %in% colnames(finalResult)) {
    finalResult <- dplyr::mutate(finalResult,
                                 Low_Today = as.numeric(sapply(Low, function(x) x[1])),
                                 Low_24h = as.numeric(sapply(Low, function(x) x[2]))
    ) %>% dplyr::select(-Low)
  }

  if ("High" %in% colnames(finalResult)) {
    finalResult <- dplyr::mutate(finalResult,
                                 High_Today = as.numeric(sapply(High, function(x) x[1])),
                                 High_24h = as.numeric(sapply(High, function(x) x[2]))
    ) %>% dplyr::select(-High)
  }

  # Unnest the "Open" column (single value) and convert to numeric
  if ("Open" %in% colnames(finalResult)) {
    finalResult <- dplyr::mutate(finalResult,
                                 Open_Price = as.numeric(sapply(Open, function(x) x[1]))
    ) %>% dplyr::select(-Open)
  }

  # Return the cleaned data frame with renamed and split columns
  return(finalResult)
}
