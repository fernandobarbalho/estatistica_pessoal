library(tidyverse)
library(colorspace)

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
  janitor::clean_names(read_delim(arquivo_trabalho,
                                  delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",
                                                                                      grouping_mark = ".", encoding = "LATIN1"),
                                  trim_ws = TRUE))
}



### Para remuneração as tabelas do PEP não conseguem responder ao que é demandado na tabela 3.1 do BEP de Jan_2017

## Também não é possível fazer comparações dos dados do BEP com PEP já que o primeiro mês disponível no PEP é janeiro de 2017 e o último mês para o BEP ´dezembro de 2016

## Testando a comparação de Janeiro de 2023 entre Portal da Transparência e PEP
remuneracao_jan_2023 <- busca_dados_remuneracao("2023","01",FALSE)

cadastro_jan_2023<- busca_dados_cadastro("2023","01",FALSE)

glimpse(remuneracao_jan_2023)

glimpse(cadastro_jan_2023)

join_tabelas<-
remuneracao_jan_2023 %>%
  inner_join(
    cadastro_jan_2023 %>%
      group_by(id_servidor_portal) %>%
      summarise(
        quantidade = n()
      ) %>%
      filter(quantidade == 1)
  ) %>%
  inner_join(cadastro_jan_2023, by= c("id_servidor_portal"))


##Testando a maior remuneração de carreira para AFFC
teste_affc<-
  join_tabelas %>%
  filter(classe_cargo == "S",
         padrao_cargo == "IV",
         descricao_cargo == "AUDITOR FEDERAL DE FINANCAS E CONTROLE")

##A mediana é um bom proxy para a remuneração básica bruta do último nível da carreira. Esse valor bate com o PEP
median(teste_affc$remuneracao_basica_bruta_r)


glimpse(join_tabelas)

join_tabelas %>%
  filter(descricao_cargo == "AUDITOR FEDERAL DE FINANCAS E CONTROLE") %>%
  mutate(posicao_carreira = str_c(classe_cargo,"-",padrao_cargo)) %>%
  group_by(posicao_carreira) %>%
  summarise(
    quantidade = n()
  ) %>%
  ungroup() %>%
  mutate(posicao_carreira = reorder(posicao_carreira, quantidade)) %>%
  ggplot(aes(x=quantidade, y=posicao_carreira, fill= quantidade )) +
  geom_col() +
  scale_fill_continuous_sequential(palette = "OrYel") +
  theme_light() +
  theme(
    panel.background = element_rect(fill = "#575756"),
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  labs(
    title= "AFFC - Ranking das classes (quantidade)",
    fill = "Quantidade",
    x="",
    y=""
  )

join_tabelas %>%
  filter(descricao_cargo == "AUDITOR FEDERAL DE FINANCAS E CONTROLE") %>%
  mutate(posicao_carreira = str_c(classe_cargo,"-",padrao_cargo)) %>%
  group_by(posicao_carreira) %>%
  summarise(
    quantidade = n()
  ) %>%
  ungroup() %>%
  ggplot(aes(x=posicao_carreira,y=quantidade,  fill= quantidade )) +
  geom_col() +
  scale_fill_continuous_sequential(palette = "OrYel") +
  theme_light() +
  theme(
    panel.background = element_rect(fill = "#575756"),
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  labs(
    title= "AFFC - Evolução das classes (quantidade)",
    fill = "Quantidade",
    x="",
    y=""
  )


join_tabelas %>%
  filter(descricao_cargo == "AUDITOR FEDERAL DE FINANCAS E CONTROLE") %>%
  mutate(posicao_carreira = str_c(classe_cargo,"-",padrao_cargo)) %>%
  ungroup() %>%
  ggplot(aes(x=posicao_carreira,y=remuneracao_basica_bruta_r, )) +
  geom_boxplot(color= "white") +

  theme_light() +
  theme(
    panel.background = element_rect(fill = "#575756"),
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  labs(
    title= "AFFC - Distribuição de remunerações por classes",
    x="",
    y=""
  )






#Desenvovler a query de extremos para pegar os top 40 maiores e menores e calcular a mediana
extremos_remuneracao_cargos<-
join_tabelas %>%
  filter(remuneracao_basica_bruta_r>0) %>%
  group_by(descricao_cargo)%>%
  summarise(
    min(remuneracao_basica_bruta_r),
    max(remuneracao_basica_bruta_r)
  )


## Testatndo a comparação de Dezembro de 2016 entre Portal da Transparência e PEP


remuneracao_dez_2016 <- busca_dados_remuneracao("2016","12",FALSE)

cadastro_dez_2016<- busca_dados_cadastro("2016","12",FALSE)


join_tabelas_2016<-
  remuneracao_dez_2016 %>%
  inner_join(
    cadastro_dez_2016 %>%
      group_by(id_servidor_portal) %>%
      summarise(
        quantidade = n()
      ) %>%
      filter(quantidade == 1)
  ) %>%
  inner_join(cadastro_dez_2016, by= c("id_servidor_portal"))


##Testando a maior remuneração de carreira para AFFC
teste_affc_2016<-
  join_tabelas_2016 %>%
  filter(classe_cargo == "S",
         padrao_cargo == "IV",
         descricao_cargo == "AUDITOR FEDERAL DE FINANCAS E CONTROLE")

##A mediana é um bom proxy para a remuneração básica bruta do último nível da carreira. Para Dezembro de 2016 este valor bate no centavo
median(teste_affc_2016$remuneracao_basica_bruta_r)


join_tabelas_2016 %>%
  filter(descricao_cargo == "AUDITOR FEDERAL DE FINANCAS E CONTROLE") %>%
  mutate(posicao_carreira = str_c(classe_cargo,"-",padrao_cargo)) %>%
  group_by(posicao_carreira) %>%
  summarise(
    quantidade = n()
  ) %>%
  ungroup() %>%
  mutate(posicao_carreira = reorder(posicao_carreira, quantidade)) %>%
  ggplot(aes(x=quantidade, y=posicao_carreira, fill= quantidade )) +
  geom_col() +
  scale_fill_continuous_sequential(palette = "OrYel") +
  theme_light() +
  theme(
    panel.background = element_rect(fill = "#575756"),
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  labs(
    title= "AFFC - Ranking das classes (quantidade)",
    fill = "Quantidade",
    x="",
    y=""
  )

join_tabelas_2016 %>%
  filter(descricao_cargo == "AUDITOR FEDERAL DE FINANCAS E CONTROLE") %>%
  mutate(posicao_carreira = str_c(classe_cargo,"-",padrao_cargo)) %>%
  group_by(posicao_carreira) %>%
  summarise(
    quantidade = n()
  ) %>%
  ungroup() %>%
  ggplot(aes(x=posicao_carreira,y=quantidade,  fill= quantidade )) +
  geom_col() +
  scale_fill_continuous_sequential(palette = "OrYel") +
  theme_light() +
  theme(
    panel.background = element_rect(fill = "#575756"),
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  labs(
    title= "AFFC - Evolução das classes (quantidade)",
    fill = "Quantidade",
    x="",
    y=""
  )


join_tabelas_2016 %>%
  filter(descricao_cargo == "AUDITOR FEDERAL DE FINANCAS E CONTROLE") %>%
  mutate(posicao_carreira = str_c(classe_cargo,"-",padrao_cargo)) %>%
  ungroup() %>%
  ggplot(aes(x=posicao_carreira,y=remuneracao_basica_bruta_r, )) +
  geom_boxplot(color= "white") +

  theme_light() +
  theme(
    panel.background = element_rect(fill = "#575756"),
    panel.grid = element_blank(),
    legend.position = "right"
  ) +
  labs(
    title= "AFFC - Distribuição de remunerações por classes",
    x="",
    y=""
  )
