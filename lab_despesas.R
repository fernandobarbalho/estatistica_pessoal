#################Laboratório para análises de despesas de pessoal


#Traz todos os datasets que tratam de despesas primárias
datasets<- ckanr::resource_search(url = "https://www.tesourotransparente.gov.br/ckan",
                                  id="8675a0a4-31c5-4593-a24d-fb8e17376eca",
                                  q= "name:Despesas e Transferências Totais da União")

#O primeiro dataset é sempre o do último ano


#Tratando de dados de 2016

url_despesa_ano_anterior<-datasets[["results"]][[8]][["url"]]

download.file(url = url_despesa_ano_anterior, destfile = "arquivo_despesa.xlsx", mode = "wb")

sheets<- readxl::excel_sheets("arquivo_despesa_2016.xlsx")

sheet_num <- 2

rtn_despesas_primarias_ano_anterior<-
  readxl::read_xlsx("arquivo_despesa.xlsx", sheet = sheet_num)



#Gera rtn_dimensional para comparar as medidas com o boletim estatístico e com o PEP
rtn_dimensional_ano_anterior<-
  rtn_despesas_primarias_ano_anterior %>%
  filter(CATEGORIA_RTN == "II.2.1 - Pessoal e Encargos Sociais - Ativo civil") %>%
  #mutate(Data = as.Date(paste(ID_ANO, ID_MES,"01",sep="-")) ) %>%
  group_by(PODER ) %>% # CATEGORIA_RTN,NO_FUNCAO_PT, ORGAO_DESCRICAO, NO_PROGRAMA_PT,NO_ACAO
  summarise(
    total_paga_ano = sum(DESPESAS_PAGAS/10^6),
    total_liquidado_ano = sum(DESPESAS_LIQUIDADAS/10^6)
  ) %>%
  ungroup()

