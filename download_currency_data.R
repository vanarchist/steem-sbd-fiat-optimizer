# Functions to download required currency data from various websites 
# and exchanges. Also calculation functions. Should probably be using
# exact representations for money quantities but for now we're not.

library(jsonlite)
library(rgdax)

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

# Get conversions data frame
get_conversions <- function(input_amount, input_currency){
  # GDAX has supports 4 crytocurrencies: BTC, BCH, LTC, ETH
  gdax_btc <- public_ticker("BTC-USD")
  gdax_bch <- public_ticker("BCH-USD")
  gdax_ltc <- public_ticker("LTC-USD")
  gdax_eth <- public_ticker("ETH-USD")
  
  # Find echange rate between steem/sbd on steemit market
  sbd_steem_rate <- as.double(get_steemit_market_rate()$result$latest)
  
  if (input_currency == "steem"){
    steem_amount <- input_amount
    sbd_amount <- input_amount*sbd_steem_rate
  }
  else if(input_currency == "sbd"){
    steem_amount <- input_amount/sbd_steem_rate
    sbd_amount <- input_amount
  }
  else{
    stop("Bad input currency")
  }

  # blocktrades supports also LTC, ETH, BTC, and BCH
  # we'll look at sbd/steem via the all 4 for the best rates
  btrades_steem_btc <- get_blocktrades_output_amt(steem_amount, "steem", "btc")
  btrades_steem_bch <- get_blocktrades_output_amt(steem_amount, "steem", "bch")
  btrades_steem_ltc <- get_blocktrades_output_amt(steem_amount, "steem", "ltc")
  btrades_steem_eth <- get_blocktrades_output_amt(steem_amount, "steem", "eth")
  
  btrades_sbd_btc <- get_blocktrades_output_amt(sbd_amount, "sbd", "btc")
  btrades_sbd_bch <- get_blocktrades_output_amt(sbd_amount, "sbd", "bch")
  btrades_sbd_ltc <- get_blocktrades_output_amt(sbd_amount, "sbd", "ltc")
  btrades_sbd_eth <- get_blocktrades_output_amt(sbd_amount, "sbd", "eth")
  
  # Binance supports BTC and ETH STEEM trading pairs
  binance_steem_btc <- get_binance_output_amt(steem_amount, "steem", "btc")
  binance_steem_eth <- get_binance_output_amt(steem_amount, "steem", "eth")
  
  # Form dataframe will all conversions
  steem_btc_usd_amt <- as.double(btrades_steem_btc)*as.double(gdax_btc$price)
  steem_bch_usd_amt <- as.double(btrades_steem_bch)*as.double(gdax_bch$price)
  steem_ltc_usd_amt <- as.double(btrades_steem_ltc)*as.double(gdax_ltc$price)
  steem_eth_usd_amt <- as.double(btrades_steem_eth)*as.double(gdax_eth$price)
  
  bin_steem_btc_usd_amt <- binance_steem_btc*as.double(gdax_btc$price)
  bin_steem_eth_usd_amt <- binance_steem_eth*as.double(gdax_eth$price)
  
  sbd_btc_usd_amt <- as.double(btrades_sbd_btc)*as.double(gdax_btc$price)
  sbd_bch_usd_amt <- as.double(btrades_sbd_bch)*as.double(gdax_bch$price)
  sbd_ltc_usd_amt <- as.double(btrades_sbd_ltc)*as.double(gdax_ltc$price)
  sbd_eth_usd_amt <- as.double(btrades_sbd_eth)*as.double(gdax_eth$price)
  
  if(input_currency == "sbd"){
    steem_insert <- "Steemit[SBD -> STEEM], "
    sbd_insert <- ""
  }
  else if (input_currency == "steem"){
    steem_insert <- ""
    sbd_insert <- "Steemit[STEEM -> SBD], "
  }
  
  data <- data.frame(c(steem_btc_usd_amt, steem_bch_usd_amt,
                       steem_ltc_usd_amt, steem_eth_usd_amt,
                       sbd_btc_usd_amt, sbd_bch_usd_amt,
                       sbd_ltc_usd_amt, sbd_eth_usd_amt,
                       bin_steem_btc_usd_amt, bin_steem_eth_usd_amt),
                     c(paste0(steem_insert, "Blocktrades[STEEM -> BTC], ",
                              "GDAX[BTC -> USD]"),
                       paste0(steem_insert, "Blocktrades[STEEM -> BCH], ",
                              "GDAX[BCH -> USD]"),
                       paste0(steem_insert, "Blocktrades[STEEM -> LTC], ",
                              "GDAX[LTC -> USD]"),
                       paste0(steem_insert, "Blocktrades[STEEM -> ETH], ",
                              "GDAX[ETH -> USD]"),
                       paste0(sbd_insert, "Blocktrades[SBD -> BTC], ",
                              "GDAX[BTC -> USD]"),
                       paste0(sbd_insert, "Blocktrades[SBD -> BCH], ",
                              "GDAX[BCH -> USD]"),
                       paste0(sbd_insert, "Blocktrades[SBD -> LTC], ",
                              "GDAX[LTC -> USD]"),
                       paste0(sbd_insert, "Blocktrades[SBD -> ETH], ",
                              "GDAX[ETH -> USD]"),
                       paste0(steem_insert, "Binance[STEEM -> BTC], ",
                              "GDAX[BTC -> USD]"),
                       paste0(steem_insert, "Binance[STEEM -> ETH], ",
                              "GDAX[ETH -> USD]")
                       ))
  colnames(data) <- c("amount", "transactions")
  data
}

# Get SBD/STEEM from steemit market
get_steemit_market_rate <- function(){
  query <- '{"jsonrpc":"2.0", "method":"market_history_api.get_ticker", "id":1}'
  r <- httr::POST("https://api.steemit.com", body = query)
  data <- httr::content(r, "text", "application/json")
  data <- fromJSON(data)
}

# Get blocktrades conversion rate
get_blocktrades_output_amt <- function(input_amount, input_currency,
                                       output_currency){
  # Get anonymous session token
  query <- '{"SessionsModel2":"{}"}'
  r <- httr::POST("https://blocktrades.us:443/api/v2/sessions", body = query)
  data <- httr::content(r, "text", "application/json")
  data <- fromJSON(data)
  session_token <- data$token
  
  # Get conversion amount
  url <- paste0("https://blocktrades.us:443/api/v2/estimate-output-amount?",
                "inputAmount=", input_amount, "&inputCoinType=", input_currency,
                "&outputCoinType=", output_currency, "&sessionToken=", session_token)
  data <- fromJSON(url)
  data$outputAmount
}

# Get Binance conversion rate (only STEEM)
get_binance_output_amt <- function(input_amount, input_currency,
                                   output_currency){
  if (input_currency != "steem"){
    stop("Unsupported currency")
  }
  
  symbol <- paste0("STEEM", toupper(output_currency))
  url <- paste0("https://api.binance.com/api/v3/ticker/price?symbol=",
                symbol)
  data <- fromJSON(url)
  price <- as.double(data$price)
  input_amount*price
}