#------------------------------------------------------------------------------#
#--------------------- Número básico de Reprodução (R_0) ----------------------#
#------------------------------------------------------------------------------#

# Estimating the serial interval of the novel coronavirus disease (COVID-19): 
# A statistical analysis using the public data in Hong Kong from January 16 to 
# February 15, 2020. DOI:10.21203/rs.3.rs-18805/v1. 

# As of February 15, there were 56 COVID-19 cases confirmed in Hong Kong since 
# the first case with symptom onset on January 23, 2020.
# 21 transmission events
# Gamma distribution performed lightly better

# mu = 4.4 (95%CI: 2.9 − 6.7) days and sigma = 3.0 (95%CI: 1.8 − 5.8) days

#-------------------------------------------------------------------------------

# Serial interval of novel coronavirus (COVID-19) infections - H.Nishiura et al.
# International Journal of Infectious Diseases 93 (2020) 284–286

# Estimates were obtainedfor certain and probable pairs combined (n = 28) 
# as well as for thecertain pairs alone (n = 18).

# mu = 4.7 (95% CrI: 3.7 - 6.0) days and sigma = 2.9 (95% CrI: 1.9 - 4.9) days

#-------------------------------------------------------------------------------

#  The serial interval of COVID-19 from publicly reported confirmed cases.

# [...] between the onset of symptoms in an index (infector) case and the onset of 
# symptoms in a secondary (infectee) case–of 468 infector-infectee pairs with 
# confirmed COVID-19 cases reported by health departments in 18 Chinese provinces 
# between January 21, 2020, and February 8, 2020. 

#  mu = 3.96 (95% CI: 3.53 - 4.39) days and sigma = 4.75 (95% CI: 4.46 - 5.07) days

#-------------------------------------------------------------------------------

# o último estudo me parece melhor, então vou usar os números desse estudo

library(incidence)
library(earlyR)
library(projections)
library(dplyr)
library(ggplot2)
library(scales)


covid <- readRDS(
  here::here("data",
             "casos_covid19_rs_tot.rds"))  


covid <- covid %>% 
  select(date, confirmed) %>%
  mutate(daily = c(covid$confirmed[1], diff(covid$confirmed)))

# o número de casos pelas datas:
onset <- rep(covid$date, times = covid$daily)

# incidencia dos casos:
i <- incidence(onset)

ggplot(mapping = aes(y = i$counts, x = i$dates)) + 
  geom_col() + 
  labs(x = NULL, y = 'Incidência Diária', 
       title = 'Incidência Diária - RS') + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
  scale_x_date(labels = date_format("%d-%b"), breaks = date_breaks("week"))

mu <- 3.96 # mean in days days
sigma <- 4.75 # standard deviation in days

res <- get_R(i, si_mean = mu, si_sd = sigma)
res

# Número básico de reprodução:
ggplot(mapping = aes(x = res$R_grid, y = res$R_like)) + 
  geom_line() + 
  labs(x = 'Número Básico de Reprodução (R)', y = NULL, 
       title = 'Número Básico de Reprodução (R) - RS') + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) + 
  geom_segment(aes(x = res$R_ml, y = 0, xend = res$R_ml, yend = max(res$R_like)), 
               linetype = 'dashed', color = 'red') + 
  geom_point(aes(x = res$R_ml, y = max(res$R_like)), color = 'red') + 
  annotate(geom = 'label', x = (res$R_ml + 0.5), y = max(res$R_like), 
            label = paste0('R = ', round(res$R_ml, 3)), size = 5, color = 'red')

# Número básico de reprodução 
# zoom - eixo x de 1 até 1.5:
ggplot(mapping = aes(x = res$R_grid, y = res$R_like)) + 
  geom_line() + 
  labs(x = 'Número Básico de Reprodução (R)', y = NULL, 
       title = 'Número Básico de Reprodução (R) - RS') + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) + 
  geom_segment(aes(x = res$R_ml, y = 0, xend = res$R_ml, yend = max(res$R_like)), 
               linetype = 'dashed', color = 'red') + 
  geom_point(aes(x = res$R_ml, y = max(res$R_like)), color = 'red') + 
  annotate(geom = 'label', x = (res$R_ml + 0.05), y = max(res$R_like), 
           label = paste0('R = ', round(res$R_ml, 3)), size = 5, color = 'red') + 
  xlim(1,1.5)


# a força global da infecção ao longo do tempo:
ggplot(mapping = aes(x = res$dates, y = res$lambdas)) + 
  geom_col(aes(fill = res$lambdas)) + 
  geom_vline(xintercept = max(covid$date), linetype = 'dashed', color = 'blue', size = 1) +
  labs(x = NULL, y = NULL, title = 'Força Global de Infecção - RS', 
       fill = expression(paste('Infecciosidade (', lambda, ')'))) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1), 
        legend.position = 'bottom') +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5)) +
  scale_fill_gradient(low = 'yellow', high = 'red')


# valores prováveis de R
R_val <- sample_R(res, 1000)  

ggplot(mapping = aes(x = R_val)) + 
  geom_histogram(color = 'white', fill = 'navy', bins = 13) + 
  labs(x = 'Valores de R', y = 'Frequência', title = 'Amostra de valores prováveis de R - RS') +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1))

# simular epicurvas futuras: 
pred <- project(i, R = R_val, n_sim = 1000, si = res$si, n_days = 30)
plot(cumulate(pred))
plot(pred)

predicted_n <- colSums(pred)
ggplot(mapping = aes(x = predicted_n)) + 
  geom_histogram(color = 'white', fill = 'darkred', bins = 15) + 
  labs(x = 'Quantidade de novos casos', y = 'Frequência', 
       title = 'Predição: Novos casos em 30 dias - RS') +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1))

df <- as.data.frame(pred, long = TRUE)
ggplot(df, aes(x = date, y = incidence)) +
  geom_jitter(alpha = .3) + geom_smooth() + 
  labs(x = NULL, y = 'Incidência') + 
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1))

pred_plot <- function(prediction_object, cumulate = FALSE){
  
  # transformando o objeto de prediction em data.frame:
  df.pred <- as.data.frame(prediction_object)
  
  # calculando os quantiles:
  quantiles <- t(apply(df.pred[,-1], 1, quantile, probs = c(.01, .05, .1, .5, .9, .95, .99)))
  q <- as.data.frame(quantiles)
  quantiles_names <- names(q)
  
  colnames(q) <- c('q1', 'q2', 'q3', 'q4', 'q5', 'q6', 'q7')
  q$date <- df.pred$dates
  
  cols <- c("q1" = "gray68", "q2" = "gray52", "q3" = "gray31", "q4" = "black", 
            "q5" = "gray31", "q6" = "gray52", "q7" = "gray68")
  
  
  if(cumulate == FALSE){
    
    ggplot(data = q, mapping = aes(x = date)) + 
      geom_ribbon(aes(ymin = q7, ymax = q1), alpha = .4, fill = 'gray68') + 
      geom_line(aes(y = q1, color = 'q1')) + 
      geom_line(aes(y = q2, color = 'q2')) + 
      geom_line(aes(y = q3, color = 'q3')) + 
      geom_line(aes(y = q4, color = 'q4')) + 
      geom_line(aes(y = q5, color = 'q5')) + 
      geom_line(aes(y = q6, color = 'q6')) + 
      geom_line(aes(y = q7, color = 'q7')) + 
      scale_colour_manual(values = cols, labels = quantiles_names) +
      labs(x = NULL, y = 'Incidência Predita', color = 'Quantil') +
      theme(panel.border = element_rect(colour = "black", fill = NA, size = 1))
    
  } else {
    
    ggplot(data = q, mapping = aes(x = date)) + 
      geom_ribbon(aes(ymin = cumsum(q7), ymax = cumsum(q1)), alpha = .4, fill = 'gray68') + 
      geom_line(aes(y = cumsum(q1), color = 'q1')) + 
      geom_line(aes(y = cumsum(q2), color = 'q2')) + 
      geom_line(aes(y = cumsum(q3), color = 'q3')) + 
      geom_line(aes(y = cumsum(q4), color = 'q4')) + 
      geom_line(aes(y = cumsum(q5), color = 'q5')) + 
      geom_line(aes(y = cumsum(q6), color = 'q6')) + 
      geom_line(aes(y = cumsum(q7), color = 'q7')) + 
      scale_colour_manual(values = cols, labels = quantiles_names) +
      labs(x = NULL, y = 'Incidência Acumulada Predita', color = 'Quantil') +
      theme(panel.border = element_rect(colour = "black", fill = NA, size = 1))
    
  }
}

pred_plot(pred, cumulate = FALSE)
pred_plot(pred, cumulate = TRUE)
