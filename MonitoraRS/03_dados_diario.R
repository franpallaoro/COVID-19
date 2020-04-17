covid <- covid %>%
  left_join(pop, by = c("cd_municipio" = "codigo")) 

covid$rmpa      <- ifelse(tolower(iconv(x = covid$municipios, 
                                      from = "latin1", 
                                      to = "UTF-8")) %in% rmpa, 1, 0)
covid$serra     <- ifelse(tolower(iconv(x = covid$municipios, 
                                      from = "latin1", 
                                      to = "UTF-8")) %in% serra, 1, 0)
covid$litoral_n <- ifelse(tolower(iconv(x = covid$municipios, 
                                      from = "latin1", 
                                      to = "UTF-8")) %in% litoral_n, 1, 0)
covid$urba_sul  <- ifelse(tolower(iconv(x = covid$municipios, 
                                      from = "latin1", 
                                      to = "UTF-8")) %in% urba_sul, 1, 0)

names(covid)[6] <- 'pop'

covid <- covid %>% 
  mutate(casos100 = casos/pop*100000) %>%
  mutate(letal = mortes/casos) 

covid$letal <- ifelse(is.na(covid$letal) == 'TRUE', 0, covid$letal)
covid$casos100 <- round(covid$casos100, 3)

#                                            Diário:
#---------------------------------------------------
#                       Estado do Rio Grande do Sul:
#---------------------------------------------------

