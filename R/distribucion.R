graficar_distribucion <- function(bd,
                                  grupo,
                                  variable,
                                  grafico="histograma",
                                  tema,
                                  color_base = "#912F40"){
  g <- switch(grafico,
              histograma = distribucion_histograma(bd, variable = {{variable}},
                                                   color_base = color_base, grupo={{grupo}})
              )
  return(g)
}

distribucion_histograma <- function(bd, variable, color_base, grupo){
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
