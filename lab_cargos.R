library(tidyverse)

###### Laboratório para análises de dados de servidores públicos (dados de concurso vindos da tabela cadastro)

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


Cadastro_201612<- busca_dados_cadastro("2016","12",FALSE)


funcao_2016<-
Cadastro_201612 %>%
  filter(sigla_funcao != -1)%>%
  group_by(sigla_funcao,
           funcao) %>%
  summarise(
    quantidade = n()
  )


####Os dados de função da tabela de cadastro aproximam-se dos dados do BEP
####Os dados do PEP batem com os dados do BEP

