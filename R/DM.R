
# Librerías ---------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)
library(forcats)
library(scales)



# plot --------------------------------------------------------------------

nn <-  diamonds %>%  count(cut)

diamonds %>%
  left_join(nn) %>% group_by(cut) %>%  mutate(m = median(carat)) %>%
  mutate(tamano = paste0(cut, "\n", n %>%  comma)) %>%
  ggplot( aes(x=fct_reorder(tamano, -m), y=carat, fill=cut)) +
  geom_violin(width=1, alpha = .3, color = "#FFFFFF") +
  geom_boxplot(width=0.1, color="#92ADB0", alpha = .8) +
  geom_point(aes(y = m), color = "#FFFFFF", shape = 18)+
  scale_fill_manual(values = c("#0D5359", "#720026", "#CE4257", "#FF7F51", "#BFBD28"))+
  # scale_color_manual(values = c("#0D5359", "#720026", "#CE4257", "#FF7F51", "#BFBD28"))+
  theme_minimal () +
  theme(
    legend.position="none",
    plot.title = element_text(size=11),
    panel.grid.minor = element_blank()
  )+
  labs(x = "", y = "Quilates", title = "Boxplot <3 violin" )
