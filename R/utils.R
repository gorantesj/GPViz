formatear_num <- function(num) {
  num <- abs(num)
  num <- case_when(
    num < 1e3 ~ as.character(num),
    num < 1e6 ~ paste0(round(num / 1000, digits = 1), "k"),
    num > 1e6 ~ paste0(round(num / 1e6, digits = 1), "M")
  )
  return(num)
}

formatear_escala_entera <-function(x, n = 4) {
  l <- pretty(x, n)
  l[abs(l %% 1) < .Machine$double.eps ^ 0.5]
}
