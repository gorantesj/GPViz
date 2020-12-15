#' Title
#'
#' @param bd
#' @param variable
#' @param n_niveles
#' @param otro
#'
#' @return
#' @export
#' @import dplyr ggplot2
#' @examples
calcular_mtc <- function(bd, variable, grupo, medida="media") {
  funcion <- switch(medida, media=mean, mediana=median)
  res <- bd %>%
    group_by({{ grupo }}) %>%
    summarise("{medida}_{{variable}}":=funcion({{variable}}))
  return(res)
}
