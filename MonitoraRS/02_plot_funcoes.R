
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


#                                            Diário:
#---------------------------------------------------
#                       Estado do Rio Grande do Sul:
#---------------------------------------------------

ggplot() +
  geom_sf(data = dep_date, aes(fill = casos)) +
  scale_fill_manual(values = virPallete) +
  theme_map() +
  transition_time(time) +
  labs(fill = "Casos por\n 100.000 hab.",
       title = "Semana epidemiológica: {round(frame_time, 0)}",
       caption = "Número  acumulado de casos confirmados COVID-19 
       - Municípios RS") +
  theme(legend.position = "right")

animate(covid_rs_vector)
anim_save(filename = "covid_rs_anima.gif",
          path = here::here("outputs"))