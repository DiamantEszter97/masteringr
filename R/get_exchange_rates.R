#' Look up the historical value of two chosen currencies
#' @param start_date date
#' @param end_date date
#' @param currfrom string
#' @param currto string
#' @inheritParams get_usdhuf
#' @return \code{data.table} object
#' @export
#' @importFrom logger log_error log_info
#' @importFrom data.table data.table
#' @importFrom httr GET content
#' example: get_exchange_rates('2020-11-05', '2020-11-06', 'usd', 'eur')
#'
#'
get_exchange_rates <- function(start_date, end_date, currfrom, currto, retried = 0) {
  tryCatch({
    response <- GET(
      'https://api.exchangerate.host/timeseries',
      query = list(
        start_date = start_date,
        end_date = end_date,
        base = currfrom,
        symbols = currto
      )
    )
    exchange_rates <- content(response)$rates
    exchange <- data.table(
      date = as.Date(names(exchange_rates)),
      rate = as.numeric(unlist(exchange_rates)))
  }, error = function(e) {
    ## str(e)
    log_error(e$message)
    Sys.sleep(1 + retried ^ 2)
    get_exchange_rates(start_date, end_date, currfrom, currto, retried = retried + 1)
  })
  return(exchange)
}
