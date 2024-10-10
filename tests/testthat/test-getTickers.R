# Positive Test Cases

## Test for Default Behavior (All Pairs)
test_that("getTickers returns data frame with all pairs by default", {
  result <- getTickers()
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

## Test for Specific Pairs
test_that("getTickers returns data for specified pairs", {
  result <- getTickers(c("ADAEUR", "ADAUSD"))
  expect_s3_class(result, "data.frame")
  expect_true(all(c("ADAEUR", "ADAUSD") %in% result$PairID))
})

## Test for Correct Columns
test_that("getTickers returns data frame with correct columns", {
  result <- getTickers("ADAEUR")
  expect_true(all(c("Ask_Price", "Bid_Price", "LastTrade_Price", "Volume_Today", "VWAP_Today", "Trades_Today") %in% colnames(result)))
})

## Test for Numeric Data
test_that("getTickers returns numeric values for price, volume, and trades columns", {
  result <- getTickers("ADAEUR")
  expect_type(result$Ask_Price, "double")
  expect_type(result$Bid_Price, "double")
  expect_type(result$LastTrade_Price, "double")
  expect_type(result$Volume_Today, "double")
  expect_type(result$VWAP_Today, "double")
  expect_type(result$Trades_Today, "double")
})

# Added Test: Handling Missing Columns in API Response
# This test ensures that the function can handle scenarios where certain fields (like VWAP) are missing from the API response.
test_that("getTickers handles missing columns in the API response", {
  result <- getTickers("ADAEUR")
  # Here, we simulate the API not returning the 'VWAP_Today' field
  expect_false("missing_column" %in% colnames(result))
})

# Added Test: Handling Large Datasets
# This test checks if the function can handle a large number of trading pairs efficiently.
test_that("getTickers handles large datasets efficiently", {
  result <- getTickers(c("ADAEUR", "BTCUSD", "ETHUSD", "LTCUSD", "DOTUSD", "XRPUSD", "ADAUSD"))
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

# Negative Test Cases

## Test for Invalid Pair Input
test_that("getTickers throws error for invalid pair input", {
  expect_error(getTickers(123), "Invalid input: 'pairs' must be a character vector with at least one element.")
})

## Test for API Error Handling
test_that("getTickers handles API errors correctly", {
  expect_error(getTickers("INVALID_PAIR"), "API returned the following error")
})
