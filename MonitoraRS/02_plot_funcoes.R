
#                             Semana Epidemiológica:
#---------------------------------------------------
#                       Estado do Rio Grande do Sul:
#---------------------------------------------------

virPallete <- viridis::viridis(n = 8)
options <- c('Casos Confirmados', 'Óbitos', 
             'Casos por\n 100.000 hab.', 'Letalidade')

input = options[3]

rs_week <- function(input){
  
  temp <- dep_week %>%
    select(casos_cat, mortes_cat, casos_p100_cat, 
           letalidade_cat, time, municipios, geometry)
  
  temp <- temp[,c(which(options == input), 5:7)]
  names(temp)[1] <- 'variavel'
  
  
  ggplot() +
    geom_sf(data = subset(temp, subset = !is.na(variavel)),
            aes(fill = variavel), size = .05) +
    scale_fill_manual(values = virPallete) +
    theme_map() +
    labs(fill = paste0(options[which(options == input)]),
         title = "COVID-19 - Municípios RS - 
         Acumulado por semana epidemiológica") +
    theme(legend.position = "right") +
    facet_wrap( ~ time)
}


