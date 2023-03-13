library(tidyverse)

###### Laboratório para análises de dados de servidores públicos (tabela de cadastro)

busca_dados_cadastro <- function(ano, mes, faz_download= TRUE){

  diretorio_trabalho<-  paste0("data/sevidor_siape",ano,mes)

  if (faz_download){
    url_base<- sprintf("https://portaldatransparencia.gov.br/download-de-dados/servidores/%s%s_Servidores_SIAPE",ano,mes)

    print(url_base)


    download.file(url = url_base, destfile = "data/dado_servidor.zip", mode = "wb")

    unzip(zipfile = "data/dado_servidor.zip",exdir = diretorio_trabalho)


  }



  arquivo_trabalho<- paste0(diretorio_trabalho,"/",ano,mes,"_Cadastro.csv")

  library(readr)
  janitor::clean_names(read_delim(arquivo_trabalho,
                                  delim = ";", escape_double = FALSE, locale = locale(encoding = "LATIN1"),
                                  trim_ws = TRUE))

}


siape_201612_Cadastro<- busca_dados_cadastro("2016","12",FALSE)

resumo_orgao_lotacao_2016<-
siape_201612_Cadastro %>%
  distinct(orgsup_lotacao,
           id_servidor_portal) %>%
  group_by(orgsup_lotacao) %>%
  summarise(
    quantidade = n()
  ) %>%
  arrange(desc(quantidade))


resumo_orgao_exercicio_2016<-
  siape_201612_Cadastro %>%
  distinct(ORGSUP_EXERCICIO,
           Id_SERVIDOR_PORTAL) %>%
  group_by(ORGSUP_EXERCICIO) %>%
  summarise(
    quantidade = n()
  ) %>%
  arrange(desc(quantidade))


####Em 2016 Os dados são incompatíveis para comparação já que não retratam a estrutura de anos anteriores


####Em janeiro de 2023 Os dados não estão batendo com os dados do PEP no agrupamento por órgão superior, nem mesmo o consolidado pelo próprio portal da transparência

siape_202301_Cadastro<- busca_dados_cadastro("2023","01")

resumo_sup_orgao_lotacao_2023_01<-
  siape_202301_Cadastro %>%
  distinct(ORGSUP_LOTACAO,
           Id_SERVIDOR_PORTAL) %>%
  group_by(ORGSUP_LOTACAO) %>%
  summarise(
    quantidade = n()
  ) %>%
  arrange(desc(quantidade))


resumo_orgao_lotacao_2023_01<-
  siape_202301_Cadastro %>%
  distinct(ORG_LOTACAO,
           Id_SERVIDOR_PORTAL) %>%
  group_by(ORG_LOTACAO) %>%
  summarise(
    quantidade = n()
  ) %>%
  arrange(desc(quantidade))


#### Identificação das situações que estão associados a registros de órgãos superiores de lotação sem informação

lotacao_sup_n_informada<-
  siape_202301_Cadastro %>%
  filter(ORGSUP_LOTACAO == "Sem informação")


## Após análise dos registros acima, percebi que os dados de órgão de lotação estão corretamente associados, havendo portanto problema apenas no órgão superior

##Contagem de servidores ativos civis por órgão de lotação
##Para essa contagem os valores já estão mais próximo com os dados do PEP
resumo_sup_orgao_exercicio_2023_01<-
  siape_202301_Cadastro %>%
  distinct(ORGSUP_EXERCICIO,
           Id_SERVIDOR_PORTAL) %>%
  group_by(ORGSUP_EXERCICIO) %>%
  summarise(
    quantidade = n()
  ) %>%
  arrange(desc(quantidade))





