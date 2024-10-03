#' Retrieve Recent Trades Data from Kraken Exchange
#'
#' This function fetches recent trade data from the Kraken API for a specified trading pair.
#'
#' @param pair A character string specifying the trading pair (e.g., "XTZUSD", "ADAEUR"). This is a required parameter.
#' @param since A character string for a human-readable date-time (e.g., "2024-10-01 12:00:00") or a Unix timestamp. Default is NULL (returns all available trades).
#' @param count An optional integer between 1 and 1000 specifying the number of trades to retrieve. Default is NULL (returns up to 1000 trades).
#'
#' @return A data frame containing the recent trade data for the requested trading pair.
#' @export
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr mutate arrange desc
#' @importFrom anytime anytime
#'
#' @examples
#' getTrades("XTZUSD")
#' getTrades("XTZUSD", since = "2024-10-01")
#' getTrades("XTZUSD", since = "2024-10-01 00:00:00", count = 100)


getTrades <- function(pair, since = NULL, count = NULL) {

  # Validate the pair input
  if (!is.character(pair) || length(pair) != 1) {
    stop("Invalid input: 'pair' must be a single character string.")
  }

  # Convert human-readable date-time into Unix timestamp if 'since' is provided
  if (!is.null(since) && !is.numeric(since)) {
    since <- as.numeric(anytime::anytime(since))
    if (is.na(since)) {
      stop("Invalid 'since' format. Please provide a valid date-time string or a Unix timestamp.")
    }
  }

  # Validate the count input if provided
  if (!is.null(count)) {
    if (!is.numeric(count) || count < 1 || count > 1000) {
      stop("Invalid input: 'count' must be a number between 1 and 1000.")
    }
  }

  # Build the URL for the recent trades data request
  url <- paste0("https://api.kraken.com/0/public/Trades?pair=", pair)
  if (!is.null(since)) {
    url <- paste0(url, "&since=", since)
  }
  if (!is.null(count)) {
    url <- paste0(url, "&count=", count)
  }

  # Fetch data from the Kraken API
  jsonFile <- tryCatch({
    jsonlite::fromJSON(url)
  }, error = function(e) {
    stop("Error fetching data from the Kraken API: ", e$message)
  })

  # Check for API errors
  if (length(jsonFile[["error"]]) > 0 && jsonFile[["error"]][1] != "") {
    stop("API returned the following error(s): ", paste(jsonFile[["error"]], collapse = ", "))
  }

  # Extract the trades data
  trades_data <- jsonFile[["result"]][[pair]]
  if (is.null(trades_data)) {
    stop("No trade data returned for the given pair.")
  }

  # Convert trades data into a data frame and label columns
  trades_df <- as.data.frame(trades_data, stringsAsFactors = FALSE)
  colnames(trades_df) <- c("Price", "Volume", "Time", "Order_Type", "Execution_Type", "Miscellaneous", "Trade_ID")

  # Convert numeric columns to proper numeric format and sort by time (latest trade first)
  trades_df <- dplyr::mutate(trades_df,
                             Price = as.numeric(Price),
                             Volume = as.numeric(Volume),
                             Time = anytime::anytime(as.numeric(Time)),
                             Order_Type = ifelse(Order_Type == "b", "buy", "sell"),
                             Execution_Type = ifelse(Execution_Type == "m", "market", "limit"))

  # Sort by time in descending order (latest first)
  trades_df <- dplyr::arrange(trades_df, dplyr::desc(Time))

  return(trades_df)
}
