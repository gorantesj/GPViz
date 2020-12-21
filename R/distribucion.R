graficar_distribucion <- function(bd,
                                  grupo,
                                  variable,
                                  grafico="histograma",
                                  tema,
                                  color_base = "#912F40"){
  g <- switch(grafico,
              histograma = distribucion_histograma(bd, variable = {{variable}},
                                                   color_base = color_base, grupo={{grupo}}),
              caja_violin=distribucion_caja_violin(bd, variable = {{variable}},
                                                   color_base = color_base, grupo={{grupo}})
              )
  g <- g %>%
    aplicar_tema(grafico = grafico, tema = tema)
  return(g)
}

distribucion_histograma <- function(bd, variable, grupo, color_base){
  resumen <- bd %>% summarise(mediana=median({{variable}}),
                              maximo=median({{variable}}),
                              minimo=median({{variable}}))
  letrero <- if_else(resumen$mediana<.5*(resumen$maximo-resumen$minimo),
                     "top-right",
                     "top-left")
  g <- bd %>%
    ggplot(aes(x={{variable}}))+
    geom_histogram(fill= color_base)+
    geom_hline(yintercept = 0,
               color = colortools::complementary(color = color_base, plot = F)[[2]])+
    scale_x_continuous(labels = scales::comma_format())
  if(!missing(grupo)) g <- g+facet_wrap(~{{grupo}})
}

#' Title
#'
#' @param bd
#' @param variable
#' @param grupo
#' @param color_base
#'
#' @return
#' @export
#'
#' @examples
distribucion_caja_violin <-function(bd, variable, grupo, color_base){
  color_texto <- if_else(plotwidgets::col2hsl(color_base)["L",1]>.7,
                                      "black",
                                      "white")
  g <- bd %>%
    group_by({{grupo}}) %>%
    mutate({{grupo}}:=paste({{grupo}},
                            scales::comma(sum({{variable}})),
                            sep = "\n")) %>%
    ggplot(aes(x={{grupo}},
                y={{variable}})) +
    geom_violin(width=1, alpha = .3, fill=color_base,
                color = "transparent") +
    geom_boxplot(width=0.1, fill=color_base, outlier.alpha = .2,
                 color=colortools::complementary(color = color_base, plot = F)[[2]],
                 alpha = .8) +
    geom_point(
               color = color_texto,
               shape = 18,
               stat="summary",fun=median)+
    coord_flip()
  return(g)
}
