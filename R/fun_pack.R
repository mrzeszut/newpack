#' Funkcja oblicz srednia z losowych zmiennych
#'
#' @param a double
#' @param b double
#' @param d double
#'
#' @return double
#'
#' @examples
#' fun_pack(a = 1, b = 500, d = 23)
#' @export

fun_pack <- function(a = 10, b = 20, d = 2) {

  out <- base::sqrt(a*b*d/2 + 100+1)

  out <- stats::rnorm(n = base::round(out,0))
  out <- base::as.data.frame(out)
  out <- dplyr::summarise(out,
                          mean = base::mean(out, na.rm = T))

  return(out)
}

