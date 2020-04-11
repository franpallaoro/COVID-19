library(naniar)


vis_miss(dataset[-c(1:2)])
gg_miss_upset(dataset[-c(1:2)], nsets = 6)

gg_miss_var(dataset[-c(1:2)], show_pct = TRUE)

