options(scipen = 999)

install.packages("tidyverse")
library(tidyverse)

install.packages("devtools")
library(devtools)

#pacote de dados eleitorais
install_github("silvadenisson/electionsBR")
library(electionsBR)

#esse df é a arrecadação dos candidatos q eu juntei de outra fonte
df <- read.csv("guilherme.txt",sep=";",stringsAsFactors = F)

#carrega do pacote de dados eleitorais
cands <- candidate_fed(2014)

#checar uniques de duas variáveis
cands %>% 
  distinct(COD_SIT_TOT_TURNO,DESC_SIT_TOT_TURNO)

#criar uma nova coluna com base em um if-else (e trocar o tipo da outra)
df <- df %>%
  mutate(setorzao = ifelse(setor_doador_orig=="#NULO", setor, setor_doador_orig),
         cpf = ifelse(setor_doador_orig=="#NULO", cpf_doador, cpf_doador_orig),
         doador = ifelse(setor_doador_orig=="#NULO", nome_doador, nome_doador_orig),
         valor = as.numeric(gsub(",",".",valor))) 

#tabela dinâmica
porcargo <- df %>%
  filter(COD_SIT_TOT_TURNO %in% c(1,2,3)) %>%
  group_by(cpf, doador, setorzao, cargo) %>%
  summarise(valor=sum(as.numeric(valor),na.rm=T)) %>%
  na.omit() %>%
  spread(cargo, valor, fill=0)

#left join com base no sequencial
df <- df %>%
  mutate(sequencial = as.numeric(sequencial)) %>%
  left_join(select(cands,SEQUENCIAL_CANDIDATO,COD_SIT_TOT_TURNO), by = c("sequencial"="SEQUENCIAL_CANDIDATO"))

#tabela 'melted' com cada valor repetindo por linha
poreleito <- df %>%
  filter(COD_SIT_TOT_TURNO %in% c(1,2,3)) %>%
  group_by(nome,sigla,uf,cargo,cpf, doador, setorzao) %>%
  summarise(valor=sum(as.numeric(valor),na.rm=T)) %>%
  mutate(setorzao = gsub('#NULO',"PESSOA FISICA",setorzao))


write.csv(poreleito,"por_eleito.csv",row.names = F)
write.csv(porcargo,"por_cargo.csv",row.names = F)

df <- read.csv('bolsa_fpm.csv')
df2 <- read.csv('bolsa_fpm2.csv')
df3 <- read.csv('bolsa_fpm3.csv')

df <- df %>%
  left_join(df2,by=c("mun","uf")) %>%
  left_join(df3,by=c("mun","uf")) 

df %>%
  group_by(maior_2010) %>%
  summarise(Unique_Elements = n())