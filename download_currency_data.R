# Functions to download required currency data from various websites 
# and exchanges

library(jsonlite)

# Convert from currency string to coingecko format
currency_to_coingecko <- function(currency){
  if (currency == "sbd"){
    return("steem-dollars")
  }
  else if (currency == "steem"){
    return("steem")
  }
  else{
    stop("Bad currency string")
  }
}

# Get currency price across exchanges from coingecko
get_usd_price <- function(currency){
  data <- fromJSON(paste0("https://api.coingecko.com/api/v3/coins/",
                          currency_to_coingecko(currency)))
  usd <- data$market_data$current_price$usd
}

# Get chart data of last 7 days for specified currency from coingecko
get_chart_data <- function(currency){
  data <- fromJSON(paste0("https://api.coingecko.com/api/v3/coins/",
                          currency_to_coingecko(currency), 
                          "/market_chart?vs_currency=usd&days=7"))
  chart_df <- data.frame(data$prices)
  colnames(chart_df) <- c("date", "price")
  chart_df$date <- as.POSIXct(chart_df$date/1000, origin="1970-01-01", 
                              tz="EST")
  return(chart_df)
}