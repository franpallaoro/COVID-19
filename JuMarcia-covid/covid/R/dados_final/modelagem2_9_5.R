# df.aux <- banco[,var.50]

# so para lembrar que a análise atual é considerando que as variáveis com mais de 
# 50% de missings foram desconsideradas. 

banco_imputado2=banco_imputado
banco_imputado=banco_imputado2
banco_imputado = df.knn
colnames(banco_imputado)[6] <- 'y'


abreviatura = names(banco_imputado)
names(banco_imputado) = seq(1:38)
codigo=seq(1:38)
nomes_banco_codigo = cbind(abreviatura, codigo)


import_variavel <- read_excel("import_variavel.xlsx")
nomes_completo_variaveis = import_variavel[, c(1, 4)]
names(nomes_completo_variaveis) = c("variavel", "abreviatura")

teste = merge(nomes_banco_codigo, nomes_completo_variaveis, by = 'abreviatura')

# R A N D O M - F O R E S T 
#----------------------------------------- BUSCA ALEATÓRIA DE PARÂMETROS

fit_control <- trainControl(method = "repeatedcv",
                             number = 10,
                             repeats = 1,
                             #sampling = "down",
                             search = "random")

rf_model <- train(y ~ ., 
                  data = banco_imputado[,-c(1:3)], 
                  method = "rf", 
                  preProcess = c("scale", "center"),
                  trControl = fit_control,
                  #tuneGrid = hiperparametros3,
                  verbose = FALSE,
                  tuneLength = 10)

rf_model

plot(rf_model)

importance_rf_model <- varImp(rf_model, scale = TRUE)
importance_rf_model
plot(importance_rf_model)

# B O O S T I N G
library(xgboost)

model_xgb <- caret::train(y ~ .,
                          data = banco_imputado[,-c(1:3)],
                          method = "xgbTree",
                          preProcess = c("scale", "center"),
                          trControl = trainControl(method = "repeatedcv", 
                                                   number = 10, 
                                                   repeats = 1, 
                                                   savePredictions = TRUE, 
                                                   verboseIter = FALSE)) 


importance <- varImp(model_xgb, scale = TRUE)
plot(importance)



#------------------------------ grafo

library(igraph)
library(dplyr)

# vou criar um for pra ficar melhor: 

grafo.plot <- function(mins, maxs){
  
  covid <- filter(banco_imputado, y >= mins & y < maxs) %>%
    select(4:36) %>%
    cor()
  
  c100 <- graph.adjacency(covid,
                          weighted = TRUE,
                          diag = FALSE,
                          mode = "upper")
  
  cut.off1 <- mean(E(c100)$weight)
  g100 <- delete_edges(c100, E(c100)[weight < cut.off1])
  c_g100 <- cluster_fast_greedy(g100) 
  
  plot(c_g100, g100,
       #vertex.size = colSums(co_covid_positivo) * 10,
       #vertex.frame.color = NA, 
       #vertex.label.color = "black", 
       #vertex.label.cex = 1,
       edge.width = E(g100)$weight * 15,
       layout = layout_with_fr(g100),
       main = paste0('Between ', mins,' and ' , maxs, ' confirmed cases'))
}

# eu fiz essa função que ele plota o grafo para um grupo específico 
# no mins e max eu escolho o numero min e max de casos confirmados de corona 

# no exemplo abaixo por exemplo peguei só os países que tinham 
# entre 1.000 e 10.000 casos

grafo.plot(mins = 5000, maxs = 1000000)
  
# --------------------- para o geral, considerando tudo de uma vez: 

covid <- select(banco_imputado, 4:36) %>%
  cor()

c100 <- graph.adjacency(covid,
                        weighted = TRUE,
                        diag = FALSE,
                        mode = "upper")

cut.off1 <- mean(E(c100)$weight)
g100 <- delete_edges(c100, E(c100)[weight < cut.off1])
c_g100 <- cluster_fast_greedy(g100) 

plot(c_g100, g100,
     #vertex.size = colSums(co_covid_positivo) * 10,
     #vertex.frame.color = NA, 
     #vertex.label.color = "black", 
     #vertex.label.cex = 1,
     edge.width = E(g100)$weight * 15,
     layout = layout_with_fr(g100),
     main = 'COVID-19 worldwide')



