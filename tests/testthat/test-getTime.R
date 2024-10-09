# Positive Test Cases

## Test for Default Behavior (Unix Time)
test_that("getTime returns current time in Unix format by default", {
  result <- getTime()
  expect_s3_class(result, "POSIXct")
})

## Test for RFC 1123 Format
test_that("getTime returns current time in RFC 1123 format", {
  result <- getTime("rfc")
  expect_type(result, "character")
})

# Negative Test Cases

## Test for Invalid Format Input
test_that("getTime throws error for invalid format input", {
  expect_error(getTime("invalid_format"), "Invalid format specified. Use 'unix' or 'rfc'.")
})

## Test for API Error Handling
test_that("getTime handles API errors correctly", {
  expect_error(getTime(), "API returned the following error")
})
