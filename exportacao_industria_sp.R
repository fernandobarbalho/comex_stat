# export_import_2024_sp <- readRDS("~/github/comex_stat/export_import_2024_sp.rds")
# 
# export_import_2024_brasil <- readRDS("~/github/comex_stat/export_import_2024_brasil.rds")
# 
# produtos_industrializados <- readRDS("~/github/comex_stat/produtos_industrializados.rds")
# 
# 
# exportacao_industria_sp<-
# export_import_2024_sp %>%
#   inner_join(
#     produtos_industrializados %>%
#       filter(is_manufacturing == "Yes") %>%
#       rename(codigo_sh2 = co_sh2)
#   ) 
# 
# 
# exportacao_industria_br<-
#   export_import_2024_brasil %>%
#   inner_join(
#     produtos_industrializados %>%
#       filter(is_manufacturing == "Yes") %>%
#       rename(codigo_sh2 = co_sh2)
#   ) 
# 
# 
# sum(exportacao_industria_sp$exportacao_2024_valor_us_fob)/sum(exportacao_industria_br$exportacao_2024_valor_us_fob)


####Fonte dos dados a seguir
#https://comexstat.mdic.gov.br/pt/geral

###Indústria de Transformação BrasiL 2024 de acordo com seção do ISIC: 
181.767.878.561

###Indústria de Transformação BrasiL 2024 de acordo com seção do ISIC: 
63.459.717.909

#Proporção 
63459717909/181767878561

###Dados por classe da indústria de transformação
export_import_sp_transformacao_2024 <- 
  read_excel("export_import_sp_transformacao_2024.xlsx") %>%
  janitor::clean_names()

export_import_br_transformacao_2024 <- 
  read_excel("export_import_br_transformacao_2024.xlsx")%>%
  janitor::clean_names()


proporcao_sp_transformacao<-
export_import_sp_transformacao_2024 %>%
  select(codigo_isic_classe, descricao_isic_classe, exportacao_2024_valor_us_fob) %>%
  rename(exportacao_2024_sp = exportacao_2024_valor_us_fob) %>%
  inner_join(
    export_import_br_transformacao_2024 %>%
    select(codigo_isic_classe,  exportacao_2024_valor_us_fob) %>%
      rename(exportacao_2024_br = exportacao_2024_valor_us_fob)
  ) %>%
  mutate(
    proporcao_sp = exportacao_2024_sp/exportacao_2024_br
  ) %>%
  arrange(desc(proporcao_sp))

proporcao_sp_transformacao %>%
  writexl::write_xlsx("proporcao_sp_transformacao.xlsx")


