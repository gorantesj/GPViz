# Frecuencias
#' Title
#'
#' @param g
#'
#' @return
#' @export
#'
#' @examples
barras_GP <- function(g){
  g +
    theme(
      # Textos
      text = element_text(family = "Poppins",colour = "#333333"),
      # Titulos
      plot.title = element_text(size = 16*.75),
      plot.subtitle = element_text(colour="#4D4D4D",size = 13*.75),
      axis.title = element_text(size = 12*.75),
      # Panel
      panel.background = element_blank(),
      panel.grid.major.x = element_line(color = "grey50", size = .1),
      # Ticks
      axis.ticks = element_blank(),
      )

}

#' Title
#'
#' @param g
#'
#' @return
#' @export
#'
#' @examples
gota_GP <- function(g){
  g +
    theme(
      # Textos
      text = element_text(family = "Poppins",colour = "#333333"),
      # Titulos
      plot.title = element_text(size = 16*.75),
      plot.subtitle = element_text(colour="#4D4D4D",size = 13*.75),
      axis.title = element_text(size = 12*.75),
      # Ejes
      axis.text.y=element_blank(),
      # Panel
      panel.background = element_blank(),
      panel.grid.major.x = element_line(color = "grey50", size = .1),
      # Ticks
      axis.ticks = element_blank(),
    )
}

#' Title
#'
#' @param g
#'
#' @return
#' @export
#'
#' @examples
paleta_GP <- function(g){
  g +
    theme(
      # Textos
      text = element_text(family = "Poppins",colour = "#333333"),
      # Titulos
      plot.title = element_text(size = 16*.75),
      plot.subtitle = element_text(colour="#4D4D4D",size = 13*.75),
      axis.title = element_text(size = 12*.75),
      # Panel
      panel.background = element_blank(),
      panel.grid.major.x = element_line(color = "grey50", size = .1),
      # Ticks
      axis.ticks = element_blank(),
    )
}

caja_violin_GP <- function(g){
  g +
    theme(
      # Textos
      text = element_text(family = "Poppins",colour = "#333333"),
      # Titulos
      plot.title = element_text(size = 16*.75),
      plot.subtitle = element_text(colour="#4D4D4D",size = 13*.75),
      axis.title = element_text(size = 12*.75),
      # Panel
      panel.background = element_blank(),
      panel.grid.major.x = element_line(color = "grey50", size = .1),
      # Ticks
      axis.ticks = element_blank(),
    )
}

aplicar_tema <- function(g, grafico, tema){
  funcion <- glue::glue("{grafico}_{tema}(g)")
  eval(rlang::parse_expr(funcion))

}
