library(incidence)
library(earlyR)
library(dplyr)

R0 <- function(UF){
  
  covid <- readRDS(
    here::here("data",
               "casos_covid19.rds")) %>% 
    filter(place_type == 'state')
  
  covid <- covid %>% 
    filter(state == UF)
  
  covid <- covid %>%
    select(state, date, confirmed) %>%
    mutate(daily = c(covid$confirmed[1], diff(covid$confirmed)))
  
  onset <- rep(covid$date, times = covid$daily)
  i <- incidence(onset)

  mu <- 3.96 # mean in days days
  sigma <- 4.75 # standard deviation in days
  
  # Numero básico de Reprodução
  res <- get_R(i, si_mean = mu, si_sd = sigma)
  
  res$R_ml
}


estados <- c('AC', 'AL', 'AM', 'AP', 'BA', 'CE', 'DF', 'ES', 'GO', 'MA', 'MG', 'MS', 'MT', 'PA', 
             'PB', 'PE', 'PI', 'PR', 'RJ', 'RN', 'RO', 'RR', 'RS', 'SC', 'SE', 'SP', 'TO')

R0s <- vector()

for(i in 1:27){
  if(length(R0(UF = estados[i])) == 0){
    R0s[i] <- 0
  } else {
    R0s[i] <- R0(UF = estados[i])
  }
}

resultados <- data.frame(UF = estados, R0 = round(R0s, 3))

write.table(x = resultados, file = 'R0s.txt')
