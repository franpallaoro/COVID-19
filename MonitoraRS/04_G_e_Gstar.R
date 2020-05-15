library(spdep)

source('01_org_data.R')

df <- dep_week %>% 
  filter(dep_week$time == max(dep_week$time, na.rm = TRUE)) #ultima semana epidemiológica
rownames(df) <- df$municipios

wr <- poly2nb(df, row.names = df$municipios) # vizinhos:
lstw <- nb2listw(wr, style = 'B') # matriz de pesos

# G:
Gi <- localG(df$casos, lstw) # G

# G star:
ws <- include.self(wr) #cada cidade como sua própria vizinha
lstws <- nb2listw(ws, style = 'B') # matriz de pesos
Gis <- localG(df$casos, lstws) # G*

Ii <- localmoran(df$casos, lstw) # local moran Ii

df <- df %>%
  mutate(G = as.numeric(Gi)) %>%
  mutate(Gstar = as.numeric(Gis)) %>%
  mutate(Ii = Ii[,1])

#------------------------- gráfico 


gg <- ggplot() +
  geom_sf(data = df, aes(fill = G), size = .05) +
  theme_map() +
  labs(fill = 'G', title = paste0("COVID-19 - Municípios RS - \nSemana Epidemiológica ", 
                                  unique(df$time), ' - Estatística Local G')) +
  theme(legend.position = "right") + 
  scale_fill_gradient(low = viridis::viridis(n = 2)[1], high = viridis::viridis(n = 2)[2], 
                      breaks = round(seq(-1, ceiling(max(df$G)), 3),2), 
                      limits = c(-1, ceiling(max(df$G)))) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1))


ggstar <- ggplot() +
  geom_sf(data = df, aes(fill = Gstar), size = .05) +
  theme_map() +
  labs(fill = 'G*', title = paste0("COVID-19 - Municípios RS - \nSemana Epidemiológica ", 
                                  unique(df$time), ' - Estatística Local G*')) +
  theme(legend.position = "right") + 
  scale_fill_gradient(low = viridis::viridis(n = 2)[1], high = viridis::viridis(n = 2)[2], 
                      breaks = round(seq(-1, ceiling(max(df$Gstar)), 1.5),2), 
                      limits = c(-1, ceiling(max(df$Gstar)))) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1))

gIi <- ggplot() +
  geom_sf(data = df, aes(fill = Ii), size = .05) +
  theme_map() +
  labs(fill = 'I de Moran', title = paste0("COVID-19 - Municípios RS - \nSemana Epidemiológica ", 
                                   unique(df$time), ' - Estatística Local I de Moran')) +
  theme(legend.position = "right") + 
  scale_fill_gradient(low = viridis::viridis(n = 2)[1], high = viridis::viridis(n = 2)[2], 
                      breaks = round(seq(-4, 56, 6),2), 
                      limits = c(-4, 56)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1))


#ggsave(filename = 'G.png', plot = gg, 
#       path = 'C:/Users/Juliana/Downloads/series', width = 8, height = 8)

#ggsave(filename = 'Gstar.png', plot = ggstar, 
#       path = 'C:/Users/Juliana/Downloads/series', width = 8, height = 8)

#ggsave(filename = 'Moran.png', plot = gIi, 
#       path = 'C:/Users/Juliana/Downloads/series', width = 8, height = 8)