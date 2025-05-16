#' Select Stocks to Buy Based on WSE Data
#'
#' This function downloads daily closing price data for selected Warsaw Stock Exchange (WSE)
#' tickers within a given date range and generates a comparative line plot for all stocks.
#'
#' @param wektor A character vector of stock tickers (e.g., `"PKN"`, `"CDR"`).
#' @param from Start date in `"YYYY-MM-DD"` format (as a string).
#' @param to End date in `"YYYY-MM-DD"` format (as a string).
#'
#' @return A list with two elements:
#' \describe{
#'   \item{df}{A data frame containing the downloaded OHLCV (Close) data.}
#'   \item{p}{A `ggplot` object showing a line chart of closing prices for each ticker.}
#' }
#'
#' @details
#' The function uses the external `getWSE()` script by m-dadej
#' (source: \url{https://github.com/m-dadej/Downloading-and-aggregating-stocks}).
#' Data is retrieved at a daily frequency, including corporate actions such as dividends and stock splits.
#'
#' @import ggplot2
#' @import tidyr
#' @importFrom lubridate ymd
#'
#' @examples
#' \dontrun{
#' what_to_buy(c("PKN", "CDR"), from = "2023-01-01", to = "2023-12-31")
#' }
#'
#' @export


#' https://github.com/m-dadej/Downloading-and-aggregating-stocks
#' Dependencies:

what_to_buy <- function(wektor, from, to){
source("https://raw.githubusercontent.com/m-dadej/Downloading-and-aggregating-stocks/master/getWSE.R")

  stopifnot(
    is.character(wektor),
    is.character(from), !is.na(lubridate::ymd(from)),
    is.character(to), !is.na(lubridate::ymd(to))
  )

df <- getWSE(tickers = wektor,
                     ohlcv = "Close",
                     from = from,
                     to = to,
                     freq = "daily",
                     corpo_action = c("div", "split"))
p <- ggplot2::ggplot()  # empty plot, aes conflict
x_col <- df[[1]]
x_label <- names(df)[1]

for (i in 2:ncol(df)) {
  seria_df <- data.frame(
    x = x_col,
    y = df[[i]],
    seria = names(df)[i]
  )
  p <- p + geom_line(data = seria_df, aes(x = x, y = y, color = seria))
}

p + labs(x = x_label, y = "Wartość", color = "Seria") +
  theme_minimal()
return(list(df,p))
}
