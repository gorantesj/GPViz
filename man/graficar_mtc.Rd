% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/centralidad.R
\name{graficar_mtc}
\alias{graficar_mtc}
\title{Grafico de medidas de tendencia central}
\usage{
graficar_mtc(
  bd,
  variable,
  grupo,
  medida = "media",
  grafico = "barras",
  color_base = "#912F40"
)
}
\arguments{
\item{bd}{data frame de entrada.}

\item{variable}{variable a analizar.}

\item{grupo}{dentro de que subconjutunto de ellos se analiza, filtro.}

\item{medida}{selector entre medidas de tendencia, dejando por defecto la media.}

\item{grafico}{tipo de grafico ggplot2 a mostrar, por defecto es "barras".}

\item{color_base}{el color del gr\'{a}fico, por defecto es color Gerencia del poder.}

\item{otro}{el numbre de la variable "Otro/Otros... etc".}
}
\value{
el gr\'{a}fico de medida central como un data frame con la variable dentro del grupo.
}
\description{
Grafico de medidas de tendencia central
}
\section{Quasiquotation}{


\code{graficar_mtc()} es una funci\'{o}n \link[rlang:nse-defuse]{quoting function}. Esto significa
que se utiliza en el contexto del preprocesamiento lo que facilita el trabajo con variables del
data frame graficando sus medidas de tendencia central.
}

\author{
Gerardo Orantes Jordan, \email{gorantes@gerenciadelpoder.com.mx}
}
