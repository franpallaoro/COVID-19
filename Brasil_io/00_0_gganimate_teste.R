library(gganimate)
library(ggplot2)

theme_set(theme_bw())


animate_plot_por_estado <- function(dataset, var_x){
  
  varx <- dataset[paste0(var_x)]
  varx <- unlist(varx)
  
  ggplot(dataset, aes(varx, state)) +
    geom_col(alpha = 0.7, show.legend = FALSE) +
    geom_text(aes(label = varx), size = 3, position = position_stack(vjust = 1)) + 
    labs(title = 'Dia: {frame_time}', y = NULL) +
    transition_time(date) +
    ease_aes('quartic-in')
}

animate_plot_por_estado(df_obitos, 'deaths_pneumonia_2019')
