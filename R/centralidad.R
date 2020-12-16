#' Procesaimento de calcular frecuencia
#' @author Gerardo Orantes Jordan, \email{gorantes@@gerenciadelpoder.com.mx}
#' @section Quasiquotation:
#'
#' `calcular_mtc()` es una funci\'{o}n [quoting function][rlang::quotation]. Esto significa
#' que se utiliza en el contexto del preprocesamiento lo que facilita el trabajo con variables del
#' data frame calculando sus medidas de tendencia central.
#' @param bd data frame de entrada
#' @param variable variable a analizar
#' @param grupo dentro de que subconjutunto de ellos se analiza, filtro
#' @param otro el numbre de la variable "Otro/Otros... etc"
#' @param medida selector entre medidas de tendencia, dejando por defecto la media
#' @return el calculo de medida central como un data frame con la variable dentro del grupo
#' @export
#' @import dplyr ggplot2
#' @examples
#' calcular_mtc(mtcars, mpg, wt)
calcular_mtc <- function(bd, variable, grupo, medida = "media") {
  funcion <- switch(medida, media = mean, mediana = median)
  res <- bd %>%
    group_by({{ grupo }}) %>%
    summarise("{{variable}}" := funcion({{ variable }}))
  return(res)
}

#' Grafico de medidas de tendencia central
#' @author Gerardo Orantes Jordan, \email{gorantes@@gerenciadelpoder.com.mx}
#' @section Quasiquotation:
#'
#' `graficar_mtc()` es una funci\'{o}n [quoting function][rlang::quotation]. Esto significa
#' que se utiliza en el contexto del preprocesamiento lo que facilita el trabajo con variables del
#' data frame graficando sus medidas de tendencia central.
#' @param bd data frame de entrada.
#' @param variable variable a analizar.
#' @param grupo dentro de que subconjutunto de ellos se analiza, filtro.
#' @param otro el numbre de la variable "Otro/Otros... etc".
#' @param medida selector entre medidas de tendencia, dejando por defecto la media.
#' @param grafico tipo de grafico ggplot2 a mostrar, por defecto es "barras".
#' @param color_base el color del gr\'{a}fico, por defecto es color Gerencia del poder.
#' @return el gr\'{a}fico de medida central como un data frame con la variable dentro del grupo.
#' @export
#' @import dplyr ggplot2
#' @examples
#' graficar_mtc(mtcars, mpg, wt)
graficar_mtc <- function(bd, variable, grupo, medida = "media", grafico = "barras", color_base = "#912F40") {
  resEst <- calcular_mtc(bd,
    variable = {{ variable }},
    grupo = {{ grupo }},
    medida = "media"
  )
  g <- switch(grafico,
    barras = mtc_barras(resEst,
      x = {{ grupo }},
      y = {{ variable }},
      color_base = color_base
    )
  )
  return(g)
}
#' Grafico de medidas de tendencia central
#' @author Gerardo Orantes Jordan, \email{gorantes@@gerenciadelpoder.com.mx}
#' @section Quasiquotation:
#'
#' `graficar_mtc()` es una funci\'{o}n [quoting function][rlang::quotation]. Esto significa
#' que se utiliza en el contexto del preprocesamiento lo que facilita el trabajo con variables del
#' data frame graficando sus medidas de tendencia central.
#' @param bd data frame de entrada.
#' @param x es la variable dependiente, eje y
#' @param y es la variable independiente, eje x, a ser la frecuencia
#' @param color_base el color del gr\'{a}fico, por defecto es color Gerencia del poder.
#' @return el gr\'{a}fico de medida central como un data frame con la variable dentro del grupo.
#' @export
#' @import dplyr ggplot2
#' @examples
#' mtc_barras(mtcars, x = mpg, y = wt)
mtc_barras <- function(bd, x, y, color_base = "#912F40") {
  g <- bd %>%
    mutate("{{x}}" := forcats::fct_reorder(as.factor({{ x }}), {{ y }})) %>%
    ggplot(aes(x = {{ x }}, y = {{ y }})) +
    geom_bar(stat = "identity", fill = color_base) +
    scale_y_continuous(
      labels = scales::comma_format()
    ) +
    # scale_x_discrete(name=stringr::str_to_sentence(eje_x)) +
    geom_hline(yintercept = 0) +
    coord_flip() +
    theme(
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major.x = element_line(color = "grey50", size = .1)
    )
  return(g)
}
