# Author: Alicia Franco
# Maintainer: MP, IS
# Copyright: 2023, Data Cívica
# blog-dificultad-diversidad/src/tema.R
# =========================================================================
if(!require(pacman)) install.packages("pacman")
p_load(ggnewscale)
#### TEMA ####
tema <-  theme_minimal() +
  theme(text = element_text(family = "Barlow Condensed", color = "grey35"),
        plot.title = element_text(size = 20, face = "bold", color = "black", hjust = 0.5),
        plot.subtitle = element_text(size = 16, face = "bold", color = "#666666", hjust = 0.5),
        plot.caption = element_text(hjust = 0, size = 10, face = "italic"),
        panel.grid = element_line(linetype = 2), 
        legend.position = "top",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 12, face = "bold"),
        strip.background = element_rect(fill="#525252"),
        strip.text = element_text(size=12, face = "bold", color = "#FAF9F6")
  )

pal_base <-  c("#9e0142", "#d53e4f", "#f46d43", "#fdae61", "#fee08b", "#ffffbf", 
               "#e6f598", "#abdda4", "#66c2a5", "#3288bd", "#5e4fa2", "#16F4D0")

pal_orient_sex <-  c("#f49595","#f9eb97", "#c6f9ac","#a8d9f6", "#e2bbfd", "gray")
pal_id_gen <- c( "#f46d43", "#fdae61","#abdda4", "#66c2a5", "#5e4fa2" )

caption <- "Fuente: Elaboración propia a partir de la Encuesta Nacional sobre Diversidad Sexual y de Género (ENDISEG) 2021."