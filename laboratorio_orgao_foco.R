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


#busca dados de Janeiro de 2023

library(stringr)
library(tidyverse)

dados_jan_2023<-
busca_dados_cadastro("2023","01",FALSE)

dados_jan_2023<-
dados_jan_2023 %>%
  mutate(cod_uorg_lotacao = as.character(cod_uorg_lotacao),
         cod_uorg_exercicio = as.character(cod_uorg_exercicio))

secretaria<-
  dados_jan_2023 %>%
  filter(str_detect(uorg_lotacao,"SECRETARIA") ) %>%
  distinct(uorg_lotacao)



patrimonio<-
dados_jan_2023 %>%
  filter(str_detect(uorg_lotacao,"PATRIMONIO") ) %>%
  distinct(uorg_lotacao)

uniao<-
  dados_jan_2023 %>%
  filter(str_detect(uorg_lotacao,"UNIAO") ) %>%
  distinct(uorg_lotacao)



stn<-
  dados_jan_2023 %>%
  filter(str_detect(cod_uorg_exercicio,"17000000066887") ) %>%
  distinct(uorg_exercicio, cod_uorg_exercicio, cod_org_exercicio, org_exercicio, orgsup_exercicio)




dados_fev_2023<-
  busca_dados_cadastro("2023","02")
