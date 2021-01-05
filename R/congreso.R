#### Acumulado ####
# Cargar librerías
library(tidyverse)
library(magrittr)
library(ggparliament)


#  Generar bd
bd <- tibble(partido =c("MORENA", "PAN", "PRI", "PT", "MC", "PES", "PRD", "PVEM", "SP"),
             color = c("#720026", "#114FBA", "#CF0A0A", "#D10A0A", "#FF5512", "#8622AB",
                       "#E8BA15", "#5BAD0E", "#AD0380"),
             n = c(252, 77, 48,46,27, 24,12,11,3),
             camara = c(rep("Diputados",9  )))


#Estructura correcta de la bd
dip <- parliament_data(election_data = bd,
                parl_rows = 10,
                type = 'semicircle',
                party_seats = bd$n)


# Chart
dip %>%  ggplot(aes(x, y, color = partido) )+
  geom_parliament_seats(size = 5) +
  labs(colour="Partido") +
  theme_ggparliament(legend = TRUE) +
  scale_colour_manual(values = dip$color,
                      limits = dip$partido)

