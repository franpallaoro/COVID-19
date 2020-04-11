#------- imputação: 

# quantidade de NA em cada indicador econômico 
NA.col <- round(apply(apply(auxiliar, 2, is.na), 2, sum)/nrow(auxiliar), 2)

var.70 <- c(colnames(banco)[1:6], names(NA.col)[which(as.numeric(NA.col) < 0.7)])
var.60 <- c(colnames(banco)[1:6], names(NA.col)[which(as.numeric(NA.col) < 0.6)])
var.50 <- c(colnames(banco)[1:6], names(NA.col)[which(as.numeric(NA.col) < 0.5)])

df.aux <- banco[,var.50]

#-------------------------- IMPUTAÇÃO 
require(caret)
library(mice)

preProc <- preProcess(method = "bagImpute", df.aux)
df.bag <- predict(preProc, df.aux)

preProc2 <- preProcess(method = "knnImpute", k = 10, df.aux)
df.knn <- predict(preProc, df.aux)

miceMod <- mice(df.aux, method = "rf")  
miceOutput <- complete(miceMod)  # generate the completed data.
anyNA(miceOutput) # por algum motivo o banco ainda tem 7 NAs
