#' Retrieve Order Book Data from Kraken Exchange
#'
#' This function fetches order book data from the Kraken API for a specified trading pair.
#'
#' @param pair A character string specifying the trading pair (e.g., "ADAEUR"). This is a required parameter.
#' @param count An optional integer between 1 and 500 specifying the number of orders to retrieve. Default is NULL.
#'
#' @return A data frame containing the order book data for the requested trading pair, with bid orders appearing first (sorted by price ascending) followed by ask orders (sorted by price ascending).
#' @export
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows mutate arrange
#' @importFrom anytime anytime
#'
#' @examples
#' getOB("ADAEUR")
#' getOB("ADAEUR", count = 100)

getOB <- function(pair, count = NULL) {

  # Validate the pair input
  if (!is.character(pair) || length(pair) != 1) {
    stop("Invalid input: 'pair' must be a single character string.")
  }

  # Validate the count input if provided
  if (!is.null(count)) {
    if (!is.numeric(count) || count < 1 || count > 500) {
      stop("Invalid input: 'count' must be a number between 1 and 500.")
    }
  }

  # Build the URL for the order book data request
  url <- paste0("https://api.kraken.com/0/public/Depth?pair=", pair)
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

  # Extract the order book data
  order_book <- jsonFile[["result"]][[pair]]
  if (is.null(order_book)) {
    stop("No order book data returned for the given pair.")
  }

  # Extract asks and bids
  asks <- as.data.frame(order_book$asks, stringsAsFactors = FALSE)
  bids <- as.data.frame(order_book$bids, stringsAsFactors = FALSE)

  # Name the columns for asks and bids
  colnames(asks) <- c("Ask_Price", "Ask_Volume", "Ask_Timestamp")
  colnames(bids) <- c("Bid_Price", "Bid_Volume", "Bid_Timestamp")

  # Convert numeric columns to the proper format
  asks <- dplyr::mutate(asks,
                        Ask_Price = as.numeric(Ask_Price),
                        Ask_Volume = as.numeric(Ask_Volume),
                        Ask_Timestamp = anytime::anytime(as.numeric(Ask_Timestamp)))

  bids <- dplyr::mutate(bids,
                        Bid_Price = as.numeric(Bid_Price),
                        Bid_Volume = as.numeric(Bid_Volume),
                        Bid_Timestamp = anytime::anytime(as.numeric(Bid_Timestamp)))

  # Sort bids (lowest to highest) and asks (lowest to highest)
  sorted_bids <- dplyr::arrange(bids, Bid_Price)
  sorted_asks <- dplyr::arrange(asks, Ask_Price)

  # Combine bids and asks into a single data frame
  result <- dplyr::bind_rows(
    dplyr::mutate(sorted_bids, Order_Type = "Bid"),
    dplyr::mutate(sorted_asks, Order_Type = "Ask")
  )

  return(result)
}
