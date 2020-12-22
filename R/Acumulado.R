#### Acumulado ####
# Cargar librerías
library(tidyverse)
library(magrittr)
library(lubridate)

#  Generar bd
bd <-  tibble(fecha = seq(today()-60, today(), "day"), n = rnorm(61, mean = 50, sd =25))


# Línea solita ------------------------------------------------------------
# Procesamiento
bd %>% arrange(fecha) %>%  mutate(n = cumsum(n)) %>%
  #Plot
  ggplot(aes(x=fecha, y= n)) +
  geom_line( color="#BFBD28", size=1.5, alpha=.8, linetype=2) +
  # Etiquetas
  labs(x = "Fecha", y = "Acumulado")+
  #  Tema
  theme_minimal() +
  theme(panel.grid.minor  = element_blank(),
        panel.grid.major.x = element_blank())


# Con área degradada ------------------------------------------------------

bd <- bd %>% arrange(fecha) %>%  mutate(n = cumsum(n))
bd %>% mutate(alfa = seq(0.9, 0, length.out = nrow(bd)),
              yinter = seq(0, max(bd$n), length.out = nrow(bd))) %>%
  #Plot
  ggplot(aes(x=fecha, y= n)) +
  geom_area( fill = "#BFBD28", alpha = 0.35)+
  # "Degradado"
  geom_hline( aes(yintercept = yinter, alpha = alfa),
             size  = 5, color = "white")+
  scale_alpha_identity()+
  # linea (está después porque si no se hace "degradada")
  geom_line( color="#BFBD28", size=1, linetype=2) +
    # Etiquetas
  labs(x = "Fecha", y = "Acumulado")+
  #  Tema
  theme_minimal() +
  theme(panel.grid.minor  = element_blank(),
        panel.grid.major.x = element_blank())

