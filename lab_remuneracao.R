library(tidyverse)

###### Laboratório para análises de dados de servidores públicos (tabela de remuneração)

busca_dados_remuneracao <- function(ano, mes, faz_download= TRUE){

  diretorio_trabalho<-  paste0("data/sevidor_siape",ano,mes)

  if (faz_download){
    url_base<- sprintf("https://portaldatransparencia.gov.br/download-de-dados/servidores/%s%s_Servidores_SIAPE",ano,mes)

    print(url_base)


    download.file(url = url_base, destfile = "data/dado_servidor.zip", mode = "wb")

    unzip(zipfile = "data/dado_servidor.zip",exdir = diretorio_trabalho)


  }



  arquivo_trabalho<- paste0(diretorio_trabalho,"/",ano,mes,"_Remuneracao.csv")

  library(readr)
  janitor::clean_names(read_delim("data/sevidor_siape202301/202301_Remuneracao.csv",
                                  delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",
                                                                                      grouping_mark = ".", encoding = "LATIN1"),
                                  trim_ws = TRUE))
}



### Para remuneração o PEP não consegue responder ao que é demandado na tabela 3.1 do BEP de Jan_2017

## Também não é possível fazer comparações dos dados do BEP com PEP já que o primeiro mês disponível no PEP é janeiro de 2017 e o último mês para o BEP ´dezembro de 2016

remuneracao_jan_2023 <- busca_dados_remuneracao("2023","01",FALSE)

cadastro_jan_2023<- busca_dados_cadastro("2023","01",FALSE)

glimpse(remuneracao_jan_2023)

glimpse(cadastro_jan_2023)

join_tabelas<-
remuneracao_jan_2023 %>%
  inner_join(cadastro_jan_2023, by= c("id_servidor_portal"))

glimpse(join_tabelas)


extremos_remuneracao_cargos<-
join_tabelas %>%
  group_by(descricao_cargo)%>%
  summarise(
    min(remuneracao_basica_bruta_r),
    max(remuneracao_basica_bruta_r)
  )
