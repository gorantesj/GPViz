#### Acumulado ####
# Cargar librerías
library(tidyverse)
library(magrittr)


#  Generar bd
bd <- tibble(x = sample(x = seq(from = 0,to = 100),size = 50, replace = T ),
       y = sample(x = seq(from = 0,to = 100),size = 50, replace = T ),
       grupo = sample(x = c("Primero", "Segundo", "Tercero"),size = 50, replace = T ))


#Scatterplot sin grupo

bd %>% ggplot(aes(x= x, y= y)) +
  geom_point(size = 2, color = "#1d3557") +
  theme_minimal()+
  theme(panel.grid.minor = element_blank())

# Scatterplot con grupo
bd %>% ggplot(aes(x= x, y= y, color = grupo)) +
geom_point(size = 2) +
  scale_color_manual(values = c("#1d3557", "#457b9d", "#a8dadc"))+
  theme_minimal()+
  labs(color = "Grupo")+
  theme(panel.grid.minor = element_blank())
