library(tidyverse)

###### Laboratório para análises de dados de servidores públicos (tabela de cadastro)

busca_dados_cadastro <- function(ano, mes, faz_download= TRUE){


  if (faz_download){
    url_base<- sprintf("https://portaldatransparencia.gov.br/download-de-dados/servidores/%s%s_Servidores_SIAPE",ano,mes)

    print(url_base)


    download.file(url = url_base, destfile = "data/dado_servidor.zip", mode = "wb")



  }

  diretorio_trabalho<-  paste0("data/sevidor_siape",ano,mes)

  unzip(zipfile = "data/dado_servidor.zip",exdir = diretorio_trabalho)

  arquivo_trabalho<- paste0(diretorio_trabalho,"/",ano,mes,"_Cadastro.csv")

  library(readr)
  read_delim(arquivo_trabalho,
                                      delim = ";", escape_double = FALSE, locale = locale(encoding = "LATIN1"),
                                      trim_ws = TRUE)

}


siape_201612_Cadastro<- busca_dados_cadastro("2016","12")

resumo_orgao_lotacao_2016<-
siape_201612_Cadastro %>%
  group_by(ORGSUP_LOTACAO) %>%
  summarise(
    quantidade = n()
  ) %>%
  arrange(desc(quantidade))


resumo_orgao_exercicio_2016<-
  siape_201612_Cadastro %>%
  group_by(ORGSUP_EXERCICIO) %>%
  summarise(
    quantidade = n()
  ) %>%
  arrange(desc(quantidade))


####Em 2016 Os dados são incompatíveis para comparação já que não retratam a estrutura de anos anteriores


####Em janeiro de 2023 Os dados não estão batendo com os dados do BEP, nem mesmo o consolidado pelo próprio portal da transparência
##Tambpem não está batendo o total do portal da transparência com a agregação de quantidade de servidores em órgãos superiores por órgão de lotação

siape_202301_Cadastro<- busca_dados_cadastro("2023","01")

resumo_orgao_lotacao_2023_01<-
  siape_202301_Cadastro %>%
  group_by(ORGSUP_LOTACAO) %>%
  summarise(
    quantidade = n()
  ) %>%
  arrange(desc(quantidade))


resumo_orgao_exercicio_2023_01<-
  siape_202301_Cadastro %>%
  group_by(ORGSUP_EXERCICIO) %>%
  summarise(
    quantidade = n()
  ) %>%
  arrange(desc(quantidade))


unique(siape_202301_Cadastro$SITUACAO_VINCULO)
