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
    summarise("{{variable}}":=funcion({{variable}}))
  return(res)
}

#' Title
#'
#' @param bd
#' @param variable
#' @param frecuencia
#' @param n_niveles
#' @param otro
#' @param grafico
#' @param color_base
#'
#' @return
#' @export
#'
#' @examples
graficar_mtc <- function(bd, variable, grupo, medida="media", grafico="barras", color_base = "#912F40") {
  resEst <- calcular_mtc(bd,
                                variable = {{ variable }},
                                grupo = {{grupo}},
                                medida ="media"
  )
  g <- switch(grafico,
              barras = mtc_barras(resEst,
                                         x = {{grupo}},
                                         y = {{variable}},
                                         color_base = color_base
              )
  )
  return(g)
}

mtc_barras <- function(bd, x, y, color_base = "#912F40") {
  g <- bd %>%
    mutate("{{x}}":=forcats::fct_reorder( as.factor({{x}}),{{y}})) %>%
    ggplot(aes(x = {{ x }}, y = {{ y }})) +
    geom_bar(stat = "identity", fill = color_base) +
    scale_y_continuous(
      labels = scales::comma_format()) +
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
