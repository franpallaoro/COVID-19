banco <- read.table('confirmados.txt')

library(naniar)

auxiliar <- banco[,-c(1:6)]

vis_miss(auxiliar)
gg_miss_upset(auxiliar, nsets = 6)

gg_miss_var(auxiliar, show_pct = TRUE)

windows()



