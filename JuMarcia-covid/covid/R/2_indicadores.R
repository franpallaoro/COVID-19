#-------------------------- criando o banco de dados final:

# para o primeiro:
aux.df <- read.table(file.name[1])
dataset <- data.frame(Country = aux.df$Country, CCode = aux.df$CCode, aux.df$Indicador)

# colocar uma condição no start 
aux = substr(x = file.name[1], start = 3, stop = nchar(file.name[1])-4)
colnames(dataset) <- c(colnames(dataset)[1:2], aux)

country.equal <- vector()
country.equal[1] <- 264

for(k in 2:length(file.name)){
  aux.df <- read.table(file.name[k])
  
  aux = substr(x = file.name[k], start = ifelse(k < 10, 3, 4), stop = nchar(file.name[k])-4)
  dataset <- cbind(dataset, aux.df$Indicador)
  colnames(dataset) <- c(colnames(dataset)[1:(k+1)], aux)
  
  # confirmando que os nomes dos países estão na mesma ordem 
  country.equal[k] = sum(dataset$Country == aux.df$Country)
}

write.table(dataset, file = 'indicadores.txt')
