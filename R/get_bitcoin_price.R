#' get HUf price of given number of BTC
#' @param x number
#' @param retried number
#' @return number
#' @inheritParams get_usdhuf
#' @inheritParams forint
#' @export
#' @importFrom checkmate assert_number
#' @importFrom jsonlite fromJSON
#' @importFrom logger log_error log_info
#' @importFrom binancer binance_coins_prices
#' @example
#' get_bitcoin_price(1)


library(binancer)
library(logger)


get_bitcoin_price<- function(x, retried = 0) {
    BITCOINS <- x
    tryCatch({

    log_info('Number of Bitcoins: {BITCOINS}')

    usdhuf <- get_usdhuf()

    assert_number(usdhuf, lower = 250, upper = 400)

    btcusd <- binance_coins_prices()[symbol == 'BTC', usd]
    log_info('1 BTC={dollar(btcusd)}')

    log_info('My crypto fortune is {forint(BITCOINS * btcusd * usdhuf)}')




    }, error = function(e) {
        log_error(e$message)
        Sys.sleep(1 + retried^2)
        get_bitcoin_price(retried = retried + 1)

    })
    return(BITCOINS * btcusd * usdhuf)
}
