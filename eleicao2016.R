library(tidyverse)
df <- read.csv("votos_pref2016.csv")
df <- df %>%
  group_by(cod_mun) %>% 
  mutate(total_bens = sum(bens)) %>%
  mutate(total_gastos = sum(gastos)) %>%
  arrange(gastos,bens,votos)


df <- df %>%
  mutate(porc_votos = votos/total_votos) %>%
  mutate(porc_gastos = gastos/total_gastos) %>%
  mutate(porc_bens = bens/total_bens) 

df <- df %>%
  ungroup() %>%
  filter(porc_gastos <= 1) %>%
  select(sexo,raca,porc_votos,porc_gastos,porc_bens)

df <- df %>%
  mutate(raca2 = ifelse(raca == "BRANCA","BRANCA","N_BRANCA")) %>%
  select(sexo,raca2,porc_votos,porc_gastos,porc_bens)


ggplot(df,aes(porc_gastos,porc_votos)) + geom_point()
ggplot(df,aes(porc_bens,porc_votos)) + geom_point() + stat_smooth(method="lm",col="red")

fit1 <- lm(porc_votos~.,df)
