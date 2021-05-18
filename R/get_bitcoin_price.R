#' get HUf price of given number of BTC
#' @param retried number
#' @return number
#' @inheritParams get_usdhuf
#' @inheritParams forint
#' @export
#' @importFrom logger log_info
#'  @importFrom binancer binance_coins_prices
#' @example
#' get_bitcoin_price(1)


library(binancer)
library(logger)


get_bitcoin_price<- function(x) {
    BITCOINS <- x
    log_info('Number of Bitcoins: {BITCOINS}')

    usdhuf <- get_usdhuf()

    btcusd <- binance_coins_prices()[symbol == 'BTC', usd]
    log_info('1 BTC={dollar(btcusd)}')

    log_info('My crypto fortune is {forint(BITCOINS * btcusd * usdhuf)}')

    return(BITCOINS * btcusd * usdhuf)

}
