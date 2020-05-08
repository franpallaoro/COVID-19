#---------------------------
# pacotes utilizados:

library(ggplot2)
library(ggthemes)
library(SpatialEpi)
library(spdep)
library(dplyr)
library(DMwR)
library(naniar)
library(glmnet)
library(INLA)

# chamando o banco de dados organizado:
source('01_org_data.R')

rm(covid)
rm(covid_cumsum)
rm(covid_week)

# banco de dados:
temp <- dep_week %>% 
  filter(dep_week$time == max(dep_week$time, na.rm = TRUE)) 

# semana epid. mais atual:
temp <- temp %>%
  cbind(idarea = 1:nrow(temp))

rm(dep_week)

# banco com as covariáveis:
covars <- read_excel(here::here('data', 'AtlasBrasil_RS.xlsx'))
names(covars)[1] <- 'cd_geocodm'
covars$cd_geocodm <- as.character(covars$cd_geocodm)

# merge com a variável resposta: casos
dados <- covars %>%
  left_join(data.frame(casos = temp$casos, cd_geocodm = temp$cd_geocodm), 
            by = 'cd_geocodm')
rm(covars)

# Analisando se o banco de dados possui NAs: 
dados %>% 
  setNames(1:ncol(dados)) %>%
  vis_miss()

# quantidade de NA em cada variável 
# ou seja, algumas variáveis possuem NAs

round((dados %>% 
  apply(2, is.na) %>%
  apply(2, sum) %>%
  as.numeric)/nrow(dados)*100, 2)

# os NAs das variáveis são, em sua maioria, 
# poucos. O maior tem 50%, o resto tem 
# menos de 30%. 

dados[,1:77] %>% 
  gg_miss_var(show_pct = TRUE)

dados[,78:155] %>% 
  gg_miss_var(show_pct = TRUE)

dados[,156:231] %>% 
  gg_miss_var(show_pct = TRUE)

# fazer imputação por KNN:
dados <- as.data.frame(dados)

# Function that fills in all NA values using the k Nearest Neighbours of each case with NA 
# values. By default it uses the values of the neighbours and obtains an weighted 
# (by the distance to the case) average of their values to fill in the unknows

df = knnImputation(dados[,-(1:2)], k = 10, scale = T, meth = "weighAvg", distData = NULL)
anyNA(df)

# LASSO
set.seed(123)
# Train the model
glmmod = glmnet(x = as.matrix(df[,-229]), 
                y = df$casos, 
                alpha = 1, family = "poisson")

# Try cross validation lasso
cv.glmmod = cv.glmnet(x = as.matrix(df[,-229]), 
                      y = df$casos, 
                      alpha = 1, 
                      family = "poisson")

lambda = cv.glmmod$lambda.1se # the value of lambda used by default
coefs = as.matrix(coef(cv.glmmod)) # convert to a matrix 
cov = df[,which(coefs != 0)]

cov <- cov %>% 
  select(-casos)


#-- lista de vizinhos de cada município
nb <- poly2nb(temp, row.names = temp$municipios)
head(nb)

# PC priori:
prior <- list(
  prec = list(
    prior = "pc.prec",
    param = c(0.5 / 0.31, 0.01)),
  phi = list(
    prior = "pc",
    param = c(0.5, 2 / 3)))

nb2INLA("map2.adj", nb)
g <- inla.read.graph(filename = "map2.adj")


formula <- casos ~ f(idarea, model = "bym2", graph = g, hyper = prior) + cov
res <- inla(formula,
            family = "poisson", data = temp,
            E = E, control.predictor = list(compute = TRUE))
