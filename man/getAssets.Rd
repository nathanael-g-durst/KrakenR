% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getAssets.R
\name{getAssets}
\alias{getAssets}
\title{Retrieve Asset Information from Kraken Exchange}
\usage{
getAssets(assets = "All")
}
\arguments{
\item{assets}{A character vector specifying the assets to retrieve. Use "All" to fetch data for all assets.
For specific assets, provide their abbreviations (e.g., "ADA" or c("BTC", "ETH")). Default is "All".}
}
\value{
A data frame containing detailed information about the requested assets.
}
\description{
This function fetches detailed asset information from the Kraken API, either for all available assets or a specified subset.
}
\examples{
getAssets()
getAssets("ETH")
getAssets(c("BTC", "AAVE", "ADA"))
}
