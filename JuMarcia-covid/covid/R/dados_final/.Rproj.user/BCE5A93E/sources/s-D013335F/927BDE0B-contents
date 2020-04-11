# df.aux <- banco[,var.50]

# so para lembrar que a análise atual é considerando que as variáveis com mais de 
# 50% de missings foram desconsideradas. 

banco_imputado = df.knn
colnames(banco_imputado)[6] <- 'y'

require(caret)
set.seed(19)
index <- createDataPartition(banco_imputado$y, p = 0.85, list = FALSE)
train <- banco_imputado[index, ]
test  <- banco_imputado[-index, ]


# R A N D O M - F O R E S T 
#----------------------------------------- BUSCA ALEATÓRIA DE PARÂMETROS

fit_control <- trainControl(method = "repeatedcv",
                             number = 10,
                             repeats = 1,
                             #sampling = "down",
                             search = "random")

rf_model <- train(y ~ ., 
                  data = train[,-c(1:3)], 
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


rf.pred <- ceiling(predict(rf_model, test[,-c(1:3, 6)]))

require(ggrepel)
ggplot2::ggplot() + 
  geom_text_repel(aes(y = test$y, x = rf.pred, label = test$Country)) +
  geom_point(aes(y = test$y, x = rf.pred)) + 
  geom_line(aes(c(1,82000), c(1,82000)), linetype = 'dashed') + 
  labs(x = 'Predict', y = 'Confirmed Cases of COVID-19', 
       title = 'Random Forest')

# endo os primeiros mais de perto

ggplot2::ggplot() + 
  geom_text_repel(aes(y = test$y, 
                      x = rf.pred, 
                      label = test$Country)) +
  geom_point(aes(y = test$y, 
                 x = rf.pred)) + 
  geom_line(aes(c(1,300), c(1,300)), linetype = 'dashed') + 
  labs(x = 'Predict', y = 'Confirmed Cases of COVID-19', 
       title = 'Random Forest') + 
  xlim(0, 300) + ylim(0, 300)


# B O O S T I N G
library(xgboost)

model_xgb <- caret::train(y ~ .,
                          data = train[,-c(1:3)],
                          method = "xgbTree",
                          preProcess = c("scale", "center"),
                          trControl = trainControl(method = "repeatedcv", 
                                                   number = 10, 
                                                   repeats = 1, 
                                                   savePredictions = TRUE, 
                                                   verboseIter = FALSE)) 

importance <- varImp(model_xgb, scale = TRUE)
plot(importance)

xgb.pred <- ceiling(predict(model_xgb, test[,-c(1:3, 6)]))

require(ggrepel)

ggplot2::ggplot() + 
  geom_text_repel(aes(y = test$y, x = xgb.pred, label = test$Country)) +
  geom_point(aes(y = test$y, x = xgb.pred)) + 
  geom_line(aes(c(1,62000), c(1,62000)), linetype = 'dashed') + 
  labs(x = 'Predict', y = 'Confirmed Cases of COVID-19', title = 'GBM')

# endo os primeiros mais de perto

ggplot2::ggplot() + 
  geom_text_repel(aes(y = test$y, 
                      x = xgb.pred, 
                      label = test$Country)) +
  geom_point(aes(y = test$y, 
                 x = xgb.pred)) + 
  geom_line(aes(c(1,300), c(1,300)), linetype = 'dashed') + 
  labs(x = 'Predict', y = 'Confirmed Cases of COVID-19') + 
  xlim(0, 300) + ylim(0, 300)

#------------------------------ grafo


library(igraph)
library(dplyr)

# vou criar um for pra ficar melhor: 

grafo.plot <- function(mins, maxs){
  
  names(banco_imputado)[-c(1:3)] <- c(33:34, 'y', 1:32)
  
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

grafo.plot(mins = 1000, maxs = 5000)
  
# --------------------- para o geral, considerando tudo de uma vez: 

names(banco_imputado)[-c(1:3)] <- c(33:34, 'y', 1:32)

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