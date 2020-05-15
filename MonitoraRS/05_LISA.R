library(ggplot2)
library(tidyverse)
library(sf)
library(ggthemes)
library(stringr)
library(readxl)
library(leaflet)
library(RColorBrewer)
library(spdep)

# Mapas LISA, considerando significância de 5\%

locm <- localmoran(df$casos, lstw)
df$Sgini <- scale(df$casos)
df$lag <- lag.listw(lstw, df$Sgini)
df$pval <- locm[,5]


df$quad_sig <- ifelse(df$Sgini >= 0 & df$lag >= 0 & df$pval <= 0.05, 1, 
                      ifelse(df$Sgini <= 0 & df$lag <= 0 & df$pval <= 0.05, 2, 
                             ifelse(df$Sgini >= 0 & df$lag <= 0 & df$pval <= 0.05, 3, 
                                    ifelse(df$Sgini >= 0 & df$lag <= 0 & df$pval <= 0.05, 4, 5))))


breaks <- seq(1, 5, 1)
labels <- c("High-High", "Low-Low", "High-Low", "Low-High", "Not Signif.")
np <- findInterval(df$quad_sig, breaks)

colors <- c(viridis::viridis(n = 5)[5],
            viridis::viridis(n = 5)[4], 
            viridis::viridis(n = 5)[3], 
            viridis::viridis(n = 5)[2], 
            viridis::viridis(n = 5)[1])

df$quad_sig <- ifelse(df$Sgini >= 0 & df$lag >= 0 & df$pval <= 0.05, 1, 
                      ifelse(df$Sgini <= 0 & df$lag <= 0 & df$pval <= 0.05, 2, 
                             ifelse(df$Sgini >= 0 & df$lag <= 0 & df$pval <= 0.05, 3, 
                                    ifelse(df$Sgini >= 0 & df$lag <= 0 & df$pval <= 0.05, 4, 5))))


df$quad_sig <- factor(df$quad_sig, levels = c(1,2,3, 4, 5), 
                      labels = c("Alto-Alto", "Baixo-Baixo", "Alto-Baixo", 
                                 "Baixo-Alto", "Não Significativo"))    

pal <- colorFactor(palette = c(viridis::viridis(n = 5)[1],
                               viridis::viridis(n = 5)[2], 
                               viridis::viridis(n = 5)[3], 
                               viridis::viridis(n = 5)[4], 
                               viridis::viridis(n = 5)[5]), domain = df$quad_sig)


glisa <- ggplot() +
  geom_sf(data = df, aes(fill = quad_sig), size = .05) +
  theme_map() +
  scale_fill_manual(values = colors, drop = FALSE) + 
  labs(title = paste0("COVID-19 - Municípios RS - \nSemana Epidemiológica ", 
                      unique(df$time), ' - LISA'), fill = NULL) +
  theme(legend.position = "right") + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1))

#ggsave(filename = 'LISA.png', plot = glisa, 
#       path = 'C:/Users/Juliana/Downloads/series', width = 8, height = 8)
