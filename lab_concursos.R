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



concursados_gestao<-
Cadastro_201612 %>%
  mutate(ano_ingresso = str_sub(data_ingresso_cargofuncao ,7,10)) %>%
  filter(regime_juridico == "REGIME JURIDICO UNICO",
         descricao_cargo == "ESP POL PUBL GESTAO GOVERNAMENTAL",
         )%>%
  group_by(ano_ingresso) %>%
  summarise(
    quantidade = n()
  )

###A partir dos dados de cadastro de servidores não é possível chegar às estatísticas do BEP referente a concursos
###Numa primeira observação dos dados do PEP, há compatibilidade entre os dados. Explorar os dados para saber o tamanho da diferença
