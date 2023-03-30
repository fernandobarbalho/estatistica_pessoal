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



max_fornecedor<-
dados_jan_2023 %>%
  filter(org_lotacao != org_exercicio) %>%
  group_by(org_lotacao, org_exercicio) %>%
  summarise(
    quantidade = n()
  ) %>%
  ungroup() %>%
  slice_max(order_by = quantidade, n=12) %>%
  distinct(org_lotacao, org_exercicio, quantidade)


aluvial<-
dados_jan_2023 %>%
  filter(org_lotacao %in% max_fornecedor$org_lotacao,
         org_exercicio %in% max_fornecedor$org_exercicio,
         org_lotacao != org_exercicio) %>%
  mutate(lotacao =  str_wrap(org_lotacao,10),
         exercicio = str_wrap(org_exercicio,10)) %>%
  select(lotacao, exercicio)


ordem_y<-
  dataset_analise %>%
  filter(deslocamento==1,
         nome_nivel_hierarquia.x %in% c("Capital Regional B","Capital Regional C", "Centro Sub-Regional B")) %>%
  group_by(nome_nivel_hierarquia.y) %>%
  summarise(
    quantidade = n()
  ) %>%
  ungroup() %>%
  inner_join(
    de_para_hierarquia %>%
      rename(nome_nivel_hierarquia.y=nome_nivel_hierarquia,
             entrada_abreviado = nome_abreviado)) %>%
  arrange(quantidade) %>%
  mutate(entrada = entrada_abreviado)

aluvial$entrada <- factor(aluvial$lotacao, levels = unique( max_fornecedor$org_lotacao [order(max_fornecedor$quantidade)]))

library(easyalluvial)

p<-
  alluvial_wide( data = aluvial,
                 max_variables = 2,
                 fill_by = 'first_variable')


parcats::parcats(p, data_input = aluvial,marginal_histograms = FALSE,labelfont = list(size = 15, color = "black"), sortpaths= "backwards")


library(networkD3)

# Load energy projection data
# Load energy projection data
URL <- paste0(
  "https://cdn.rawgit.com/christophergandrud/networkD3/",
  "master/JSONdata/energy.json")
Energy <- jsonlite::fromJSON(URL)


orgaos_nodes<- unique(c(unique(max_fornecedor$org_lotacao), unique(max_fornecedor$org_exercicio)))

df_orgaos_nodes<- tibble(name=orgaos_nodes, index=0:(length(orgaos_nodes)-1))



orgaos_links<-
  max_fornecedor %>%
  select(org_lotacao) %>%
  inner_join(
    df_orgaos_nodes,
    by= join_by(org_lotacao == name)
  ) %>%
  rename(source = index) %>%
  select(source) %>%
  bind_cols(
    max_fornecedor %>%
      select(org_exercicio, quantidade) %>%
      inner_join(
        df_orgaos_nodes,
        by= join_by(org_exercicio == name)
      ) %>%
      rename(target = index) %>%
      select(target, quantidade)
  )


# Plot
sankeyNetwork(Links = Energy$links, Nodes = Energy$nodes, Source = "source",
              Target = "target", Value = "value", NodeID = "name",
              units = "TWh", fontSize = 12, nodeWidth = 30)


# Plot
sankeyNetwork(Links = orgaos_links, Nodes = df_orgaos_nodes, Source = "source",
              Target = "target", Value = "quantidade", NodeID = "name",
              units = "", fontSize = 12, nodeWidth = 30)


