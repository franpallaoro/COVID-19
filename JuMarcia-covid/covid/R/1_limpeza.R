library(readxl)
var.names <- read_excel('variaveis/variaveis.xlsx')

df <- list()

file.name <- vector()

for(i in 1:nrow(var.names)){
  
  all_content <- readLines(paste0('variaveis/', var.names$ARQUIVO[i]))
  skip_second <- all_content[-c(1:4)]
  temp <- read.csv2(textConnection(skip_second), header = TRUE, stringsAsFactors = FALSE, sep = ',')
  
  # as colunas que me importam: de 1960 até 2019:
  yrs <- colnames(temp)[-c(1:4, ncol(temp))]
  
  #---------------------------------------- pegar o valor do índice mais recente:
  recent.yrs <- indicator <- vector()
  
  for (j in 1:nrow(temp)){
    
    # é NA ou tem alguma informação? 
    info.na <- is.na(temp[j, yrs])
    
    if(sum(info.na == TRUE) == length(info.na)){
      
      # se só tiver NA no país
      recent.yrs[j] <- indicator[j] <- NA
      
    } else {
      
      # qual o ano mais recente com indicador? 
      recent.yrs[j] <- colnames(info.na)[max(which(info.na == 'FALSE'))]
      # pegando o valor do indicador: 
      indicator[j] <- as.numeric(temp[j,recent.yrs[j]])
      
    }
  }
  
  # agora salvando o indicador arrumado, com o ano em questão do indicador
  df[[i]] <- data.frame(Country = temp$Country.Name, 
                        CCode = temp$Country.Code, 
                        INome = temp$Indicator.Name, 
                        Indicador = indicator, 
                        Ano = recent.yrs)
  
  file.name[i] <- paste0(i, '_', temp$Indicator.Code[1], '.txt')
  write.table(x = df[[i]], file = file.name[i])
}
