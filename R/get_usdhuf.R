#' get HUf price of BTC
#' @param retried number
#' @return number
#' @inheritParams forint
#' @export
#' @importFrom checkmate assert_number
#' @importFrom jsonlite fromJSON
#' @importFrom logger log_error log_info
#' @example
#' get_usdhuf()


get_usdhuf <- function(retried = 0) {
  tryCatch({ # check whether it works if not, somethng else can run

    # httr



    usdhuf <- fromJSON('https://api.exchangerate.host/convert?from=USD&to=HUF')$result

    # stop if:
    assert_number(usdhuf, lower = 250, upper = 400) # checkmate package

  }, error = function(e) {
    log_error(e$message)
    Sys.sleep(1 + retried^2)
    get_usdhuf(retried = retried + 1)

  })
  log_info("1 USD = {forint(usdhuf)}")
  return(usdhuf)
}
