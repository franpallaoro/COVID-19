library(tidyverse)

obitos_cartorio <- read_csv("Brasil_io/obitos_cartorio.csv")

# para o brasil inteiro



aux <- obitos_cartorio %>%
  group_by(date, epidemiological_week_2019, epidemiological_week_2020) %>%
  summarise_at(names(obitos_cartorio)[c(4:8,11:15)],sum) %>%
  pivot_longer(
    cols = -c(date,epidemiological_week_2019,epidemiological_week_2020),
    names_to = "disease_type",
    values_to = "deaths"
  ) %>%
  filter(!str_detect(disease_type,"^deaths"))


ggplot(aux,aes(x=date,y=deaths, color = disease_type)) +
  geom_line() +
  geom_point()
