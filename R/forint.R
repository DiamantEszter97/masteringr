
#' Formats numbers as Hungarion Forints
#' @param x number
#' @return string
#' @export
#' @importFrom checkmate assert_number
#' @importFrom scales dollar
#' @examples
#' forint(42)
#' forint(120548.246784)

forint <- function(x) {
  assert_number(x) # make sure that it is number
  dollar(x, prefix = "", suffix = " Ft")
}
