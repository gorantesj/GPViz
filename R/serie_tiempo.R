#' Procesaimento de calcular frecuencia por categorías en serie de tiempo
#' @author Gerardo Orantes Jordan, \email{gorantes@@gerenciadelpoder.com.mx}
#' @section Quasiquotation:
#'
#' `calcular_frecuencia_categorias()` es una funci\'{o}n [quoting function][rlang::quotation]. Esto significa
#' que se utiliza en el contexto del preprocesamiento lo que facilita el trabajo con variables del
#' data frame.
#' @param bd  es el data frame de entrada, es necesario que tenga una variable de tiempo.
#' @param fecha es la variable temporal referenciada a la categor\'{i}a.
#' @param categoria es la variable de clasificaci\'{o}n con la cual se va a comparar en el tiempo.
#' @param inf es el valor m\'{i}nimo de intervalo en el cual se va analizar la frecuencia de la categor\'{i}a.
#' @param sup es el valor m\'{a}ximo de intervalo en el cual se va analizar la frecuencia de la categor\'{i}a.
#' @param n_niveles este es un número entero positivo que nos da la cantidad de variables categ\'{o}ricas a
#' mostrar, por defecto son n_niveles = 5.
#'
#' @return esta funcion regresa un data frame con los valores de las frecuencias por categoria en el tiempo
#' de cada variable seleccionada.
#' @export
#' @import dplyr ggplot2
#' @examples
#' DB <- read_csv("https://storage.googleapis.com/kagglesdsdata/datasets/991280/1673543/dataset.csv?X-Goog-Algorithm=GOOG4-RSA-SHA256&X-Goog-Credential=gcp-kaggle-com%40kaggle-161607.iam.gserviceaccount.com%2F20201221%2Fauto%2Fstorage%2Fgoog4_request&X-Goog-Date=20201221T224035Z&X-Goog-Expires=259199&X-Goog-SignedHeaders=host&X-Goog-Signature=3ea9089ddca2d12e28ee92b43f55ed7afd290fe86c16c8b925731a5af53a6bf8f1ada3b3be105ef1f63b69b96fb8f1225a3197c17cd42a994ad184669249928ff5c35ddb12aabf9f4d0cf7c65c15acc8b58cf29460bb5da487fe9c17dfdccb57037198484da4813a655155f93725e8b8d413a4dd5c4aaf26572325bd49053d82602c8b9efc3fba06d8726e14570a9c39c298825d92f7e0b8788fda8c15b7fed13b1008eb4e3da81903f03a84896ddc5e87bbc916cfbd34a450c0676c2344109dc0e31abf930c97503c3b9b8779b96b9209e4bc612cd8bcca99d223d28ad121c2851f2f76d0c083c9eed5968dd6243ad5b6b13afe8284750002f6a0aa21bfb542")
#' calcular_frecuencia_categoria(DB, Birthday, Birthplace, 1, 20, 10)

calcular_frecuencia_categoria <- function(bd, fecha, categoria, inf = 1, sup = 20, n_niveles = 5) {

  bd <- bd %>%
        select({{ categoria }}, {{ fecha }}) %>%
        mutate(frecuencia = 1) %>%
        group_by({{ categoria }}, {{ fecha }}) %>%
        summarise(across(frecuencia, sum))

  variables <- bd %>%
               select({{ categoria }}, frecuencia) %>%
               filter(frecuencia > inf & frecuencia < sup) %>%
               unique() %>%
               head(n_niveles) %>%
               pull({{ categoria }})

  bd <- bd %>%
        filter({{ categoria }} %in% variables) %>%
        na.omit()

  return(bd)
}

#' Grafico de frecuencias de categor''{i}as en serie de tiempo
#' @author Gerardo Orantes Jordan, \email{gorantes@@gerenciadelpoder.com.mx}
#' @section Quasiquotation:
#'
#' `graficar_frecuencia_serie()` es una funci\'{o}n [quoting function][rlang::quotation]. Esto significa
#' que se utiliza en el contexto del preprocesamiento lo que facilita el trabajo con variables del
#' data frame graficando la frecuencia categórica en el tiempo.
#' @param bd data frame de entrada procesado  como en calcular_frecuencia_categorias().
#' @param fecha es la variable temporal referenciada a la categor\'{i}a.
#'
#' @param categoria es la variable de clasificaci\'{o}n con la cual se va a comparar en el tiempo.
#' @return el gr\'{a}fico de las frecuencias por categoria en el tiempo.
#' @export
#' @import dplyr ggplot2
#' @examples
#' bd <- calcular_frecuencia_categoria(DB, Birthday, Birthplace, 1, 20, 10)
#' graficar_frecuencia_series(bd, Birthday, Birthplace)
graficar_frecuencia_series <- function(bd, fecha, categoria) {

  g <-  ggplot(bd, aes({{ fecha }}, frecuencia)) +
        geom_line(aes(color = {{ categoria }}), size = 1) +
        theme_minimal()

   return(g)
}
