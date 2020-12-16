#' Gráfica de formato propio de números
#' @author Gerardo Orantes Jordan, \email{gorantes@@gerenciadelpoder.com.mx}
#' @section Quasiquotation:
#' `formatear_num` es una funci\'{o}n [quoting function][rlang::quotation]. Esto significa
#' que se utiliza en el contexto de un formato de numeros para un label
#' @param num es un tipo de dato num\'{e}rico
#' @return el numero a caracter segun su orden
#' @export
#' @examples
#' formatear_num(10000)
formatear_num <- function(num) {
  num <- abs(num)
  num <- case_when(
    num < 1e3 ~ as.character(num),
    num < 1e6 ~ paste0(round(num / 1000, digits = 1), "k"),
    num > 1e6 ~ paste0(round(num / 1e6, digits = 1), "M")
  )
  return(num)
}
