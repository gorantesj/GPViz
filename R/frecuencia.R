#' Procesaimento de calcular frecuencia
#' @author Gerardo Orantes Jordan, \email{gorantes@@gerenciadelpoder.com.mx}
#' @section Quasiquotation:
#'
#' `calcular_frecuencia()` es una funci\'{o}n [quoting function][rlang::quotation]. Esto significa
#' que se utiliza en el contexto del preprocesamiento lo que facilita el trabajo con variables del
#' data frame.
#' @param bd  es el data frame de entrada
#' @param variable la nombre de variable a procesar
#' @param n_niveles este es un número entero positivo que nos da la cantidad de
#' variables a mostrar, por defecto son n_niveles = 9
#' @param otro este parámetro es para poner el nombre la la variable "Otro" "otros"
#' y que se muestre
#'
#' @return esta funcion regresa un data frame con los valores de las frecuencias, la frecuencua es n,
#' de cada variable seleccionada
#' @export
#' @import dplyr ggplot2
#' @examples
#' calcular_frecuencia(mtcars, hp)

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

#' Graficar frecuencias estilo GP
#' @author Gerardo Orantes Jordan, \email{gorantes@@gerenciadelpoder.com.mx}
#' @section Quasiquotation:
#'
#' `graficar_frecuencia()` es una funci\'{o}n [quoting function][rlang::quotation]. Esto significa
#' que se utiliza en el contexto del proceso de graficación de elementos ggplot2.
#' @param bd el data frame de entrada
#' @param variable la nombre de variable a procesar
#' @param frecuencia esta es la variable que nos indica la frecuencia de cierto elemento
#' en un data frame
#' @param n_niveles esta es un filtro de niveles, es decir filtra los n_niveles más altos,
#'  ademmás pertenece al par\'{a}metro de la funci\'{o}n calcular_frecuencia
#' @param otro este parámetro es para poner el nombre la la variable "Otro" "otros"
#' y que se muestre.
#' @param grafico este par\'{a}mtero nos indica cual es gr\'{a}fico a elegir: entre
#' barras, gota y paleta. Por defecto es barras.
#' @param color_base el color del gr\'{a}fico, por defecto es color Gerencia del poder.
#'
#' @return regresa un gráfico ggplot2
#' @export
#'
#' @examples
#'
graficar_frecuencia(mtcars, hp, frecuencia = n, n_niveles = 9)

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

#' Graficar frecuencia de barras estilo GP
#' @author Gerardo Orantes Jordan, \email{gorantes@@gerenciadelpoder.com.mx}
#' @section Quasiquotation:
#'
#' `frecuencia_barras()` es una funci\'{o}n [quoting function][rlang::quotation]. Esto significa
#' que se utiliza en el contexto del proceso de graficación de elementos ggplot2 y es una gráfica de barras.
#' @param bd el data frame de entrada
#' @param x es la variable dependiente, eje x
#' @param y es la variable independiente, eje x, a ser la frecuencia
#' @param color_base el color del gr\'{a}fico, por defecto es color Gerencia del poder.
#'
#' @return regresa un gráfico de barras ggplot2 a utilizar con frecuencia
#' @export
#'
#' @examples
#' frecuencia_barras(mtcars, hp, mpg)
frecuencia_barras <- function(bd, x, y, color_base = "#912F40") {
  g <- bd %>%
    ggplot(aes(x = {{ x }}, y = {{ y }})) +
    geom_bar(stat = "identity", fill = color_base) +
    scale_y_continuous(
      labels = scales::comma_format(),
      name = "Frecuencia"
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
#' Graficar frecuencia de barras con esquinas suaves estilo GP
#' @author Gerardo Orantes Jordan, \email{gorantes@@gerenciadelpoder.com.mx}
#' @section Quasiquotation:
#' `frecuencia_gota()` es una funci\'{o}n [quoting function][rlang::quotation]. Esto significa
#' que se utiliza en el contexto del proceso de graficación de elementos ggplot2 y es una gráfica de barras con
#' esquinas suaves.
#' @param bd el data frame de entrada
#' @param x es la variable dependiente, eje y
#' @param y es la variable independiente, eje x, a ser la frecuencia
#' @param color_base el color del gr\'{a}fico, por defecto es color Gerencia del poder.
#' @return regresa un gráfico de barras ggplot2 a utilizar con frecuencia
#' @export
#'
#' @examples
#' frecuencia_gota(mtcars, carb, gear)
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
#' Graficar frecuencia en paleta estilo GP
#' @author Gerardo Orantes Jordan, \email{gorantes@@gerenciadelpoder.com.mx}
#' @section Quasiquotation:
#' `frecuencia_gota()` es una funci\'{o}n [quoting function][rlang::quotation]. Esto significa
#' que se utiliza en el contexto del proceso de graficación de elementos ggplot2 y es una gráfica de barras con
#' esquinas suaves. Utiliza como auxiliar dentro de las labels la funci\'{o}n `formatear_num()`
#' @param bd el data frame de entrada
#' @param x es la variable dependiente, eje y
#' @param y es la variable independiente, eje x, a ser la frecuencia
#' @param color_base el color del gr\'{a}fico, por defecto es color Gerencia del poder.
#' @return regresa un gráfico de barras ggplot2 a utilizar con frecuencia
#' @export
#'
#' @examples
#' frecuencia_paleta(mtcars, mpg, gear)
frecuencia_paleta <- function(bd, x, y, color_base = "#912F40") {
  g <- bd %>%
    ggplot() +
    geom_segment(aes(xend = {{ x }}, x = {{ x }}, y = 0, yend = {{ y }}), size = 10 / nrow(bd), lineend = "round") +
    geom_point(aes(x = {{ x }}, y = {{ y }}),
      color = color_base, size = 3 * 12 / .pt
    ) +
    geom_text(aes(x = {{ x }}, y = {{ y }}, label = formatear_num({{ y }})),
      color = "white", size = 11 / .pt
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
