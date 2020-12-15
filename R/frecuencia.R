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
calcular_frecuencia <- function(bd, variable, n_niveles = 9, otro = "Otro") {
  res <- bd %>%
    count({{ variable }}) %>%
    arrange(desc(n)) %>%
    mutate({{ variable }} := if_else(row_number() <= n_niveles,
      as.character({{ variable }}),
      otro
    )) %>%
    group_by({{ variable }}) %>%
    summarise(n = sum(n))
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
graficar_frecuencia <- function(bd,
                                variable,
                                frecuencia = n,
                                n_niveles = 9,
                                otro = "Otro",
                                grafico = "barras",
                                color_base = "#912F40") {
  resEst <- calcular_frecuencia(bd,
    variable = {{ variable }},
    n_niveles = n_niveles,
    otro = otro
  )
  g <- switch(grafico,
    barras = frecuencia_barras(resEst,
      x = {{ variable }},
      y = n,
      color_base = color_base
    ),
    gota = frecuencia_gota(resEst,
      x = {{ variable }},
      y = n,
      color_base = color_base
    ),
    paleta = frecuencia_paleta(resEst,
      x = {{ variable }},
      y = n,
      color_base = color_base
    )
  )
  return(g)
}

frecuencia_barras <- function(bd, x, y, color_base = "#912F40") {
  g <- bd %>%
    ggplot(aes(x = {{ x }}, y = {{ y }})) +
    geom_bar(stat = "identity", fill = color_base, alpha=.8) +
    scale_y_continuous(
      labels = scales::comma_format(),
      name = "Frecuencia"
    ) +
    # scale_x_discrete(name=stringr::str_to_sentence(eje_x)) +
    geom_hline(yintercept = 0,
               color= colortools::complementary(color=color_base, plot = F)[[2]],
               size=1.5) +
    coord_flip() +
    theme(
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major.x = element_line(color = "grey50", size = .1)
    )
  return(g)
}

frecuencia_gota <- function(bd, x, y, color_base = "#912F40") {
  g <- bd %>%
    ggplot(aes(xend = {{ x }}, x = {{ x }}, y = 0, yend = {{ y }})) +
    geom_segment(stat = "identity", color = color_base, size = 10 / nrow(bd), lineend = "round") +
    scale_y_continuous(
      labels = scales::comma_format(),
      name = "Frecuencia"
    ) +
    coord_flip() +
    theme(
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major.x = element_line(color = "grey50", size = .1)
    )
  return(g)
}

frecuencia_paleta <- function(bd, x, y, color_base = "#912F40") {
  g <- bd %>%
    ggplot() +
    geom_segment(aes(xend = {{ x }}, x = {{ x }}, y = 0, yend = {{ y }}), size = 10 / nrow(bd), lineend = "round") +
    geom_point(aes(x = {{ x }}, y = {{ y }}),
      color = color_base, size = 3.1 * 11*.75 / .pt
    ) +
    geom_text(aes(x = {{ x }}, y = {{ y }}, label = formatear_num({{ y }})),
      color = "white", size = 11*.75/.pt
    ) +
    scale_size_area() +
    scale_y_continuous(
      labels = scales::comma_format(),
      name = "Frecuencia"
    ) +
    coord_flip() +
    theme(
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major.x = element_line(color = "grey50", size = .1)
    )
  return(g)
}
