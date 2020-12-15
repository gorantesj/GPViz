# Frecuencias
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
