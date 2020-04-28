library(spdep)

df <- dep_week %>% 
  filter(dep_week$time == 17) #ultima semana epidemiológica
rownames(df) <- df$municipios

wr <- poly2nb(df, row.names = df$municipios) # vizinhos:
lstw <- nb2listw(wr, style = 'B') # matriz de pesos

# G:
Gi <- localG(df$casos, lstw) # G
head(Gi)

# G star:
ws <- include.self(wr) #cada cidade como sua própria vizinha
lstws <- nb2listw(ws, style = 'B') # matriz de pesos
Gis <- localG(df$casos, lstws) # G*

Ii <- localmoran(df$casos, lstw) # local moran Ii
