#packages---------------------- 
require(caret)
require(caretEnsemble)

#carregando os dados-----------------------
data <- read.table("df_knn.txt")
colnames(data)[6] <- 'y'

#particionando o banco -------------------------
set.seed(19)
index <- createDataPartition(data$y, p = 0.85, list = FALSE)
train <- data[index, ]
test  <- data[-index, ]

#modelo-----------------
stackControl <- trainControl(method = "repeatedcv",
                               number = 10,
                               repeats = 1,
                               #sampling = "down",
                               search = "random")

algorithmList <- c("rf", "xgbTree")

models <- caretList(y~., 
                    data = train[,-c(1:3)], 
                    preProcess = c("scale", "center"), 
                    trControl=stackedControl, 
                    methodList=algorithmList)

stackModel <- caretStack(models, 
                         method="gbm", #metalearner
                         metric="RMSE", 
                         trControl=stackControl)

