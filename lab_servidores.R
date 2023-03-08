library(tidyverse)

###### Laboratório para análises de dados de servidores públicos (tabela de cadastro)

ano<- 2016
mes<- 12

url_base<- sprintf("https://portaldatransparencia.gov.br/download-de-dados/servidores/%i%i_Servidores_SIAPE",ano,mes)


download.file(url = url_base, destfile = "data/dado_servidor.zip", mode = "wb")

diretorio_trabalho<-  paste0("data/sevidor_siape",ano,mes)

unzip(zipfile = "data/dado_servidor.zip",exdir = diretorio_trabalho)

library(readr)
siape_201612_Cadastro <- read_delim("data/sevidor_siape201612/201612_Cadastro.csv",
                               delim = ";", escape_double = FALSE, locale = locale(encoding = "LATIN1"),
                               trim_ws = TRUE)

glimpse(siape_201612_Cadastro)


resumo_orgao_lotacao<-
siape_201612_Cadastro %>%
  group_by(ORGSUP_LOTACAO) %>%
  summarise(
    quantidade = n()
  ) %>%
  arrange(desc(quantidade))


resumo_orgao_exercicio<-
  siape_201612_Cadastro %>%
  group_by(ORGSUP_EXERCICIO) %>%
  summarise(
    quantidade = n()
  ) %>%
  arrange(desc(quantidade))


####Os dados são incompatíveis para comparação já que não retratam a estrutura de anos anteriores
