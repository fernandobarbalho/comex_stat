library(tidyverse)
library(data.table)


ncm_sh <- read_delim("NCM_SH.csv", delim = ";", 
                     escape_double = FALSE, locale = locale(encoding = "latin1"), 
                     trim_ws = TRUE)

ncm <- read_delim("NCM.csv", delim = ";", 
                  escape_double = FALSE, locale = locale(encoding = "LATIN1"), 
                  trim_ws = TRUE)

exp_2025 <- read_delim("EXP_2025.csv", delim = ";", 
                       escape_double = FALSE, trim_ws = TRUE)


ncm_sh <- janitor::clean_names(ncm_sh)

ncm<- janitor::clean_names(ncm)

exp_2025 <- janitor::clean_names(exp_2025)


ncm_sh$co_sh6 <-  as.factor(ncm_sh$co_sh6)

teste<- "aqui tem pedaço"
str_detect(teste, "\\baço\\b")


pais <- read_delim("pais.csv", delim = ";", 
                   escape_double = FALSE, locale = locale(encoding = "LATIN1"), 
                   trim_ws = TRUE)

exportacao_municipios <- 
  read_delim("EXP_2024_MUN.csv", 
             delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
  janitor::clean_names() %>%
  mutate(co_mun = as.character(co_mun)) %>%
  mutate(co_mun = case_when(
    str_sub(co_mun,1,2) == "34" ~ paste0("35",str_sub(co_mun,3,7)),
    str_sub(co_mun,1,2) == "52" ~ paste0("50",str_sub(co_mun,3,7)),
    str_sub(co_mun,1,2) == "53" ~ paste0("52",str_sub(co_mun,3,7)),
    str_sub(co_mun,1,2) == "54" ~ paste0("53",str_sub(co_mun,3,7)),
    .default = co_mun
  )) %>%
  mutate(co_mun = as.numeric(co_mun))

saveRDS(exportacao_municipios, "exportacao_municipios.RDS" )

uf_mun <- 
  read_delim("uf_mun.csv", delim = ";", 
                     escape_double = FALSE, locale = locale(encoding = "latin1"), 
                     trim_ws = TRUE)%>%
  janitor::clean_names()%>%
  mutate(co_mun_geo = as.character(co_mun_geo)) %>%
  mutate(co_mun_geo = case_when(
    str_sub(co_mun_geo,1,2) == "34" ~ paste0("35",str_sub(co_mun_geo,3,7)),
    str_sub(co_mun_geo,1,2) == "52" ~ paste0("50",str_sub(co_mun_geo,3,7)),
    str_sub(co_mun_geo,1,2) == "53" ~ paste0("52",str_sub(co_mun_geo,3,7)),
    str_sub(co_mun_geo,1,2) == "54" ~ paste0("53",str_sub(co_mun_geo,3,7)),
    .default = co_mun_geo
  )) %>%
  mutate(co_mun_geo = as.numeric(co_mun_geo))

saveRDS(uf_mun, "uf_mun.rds")


# codigos_aco<- 
#   (ncm_sh %>%
#   filter(co_sh4 %in% c("7207")) %>%
#   select(co_sh6))$co_sh6


codigos_aco<- 
  (ncm_sh %>%
     filter(str_detect( no_sh4_por,"\\baço\\b"),
            co_sh2 %in% c("72","73"))  %>%
     select(co_sh6))$co_sh6


codigos_aco_ncm<-
  (ncm %>%
  filter(co_sh6 %in% codigos_aco))$co_ncm


exportacao_aco_eua_2025<-
  exp_2025 %>%
  filter(co_pais %in% c(249,396,873),
         co_ncm %in% codigos_aco_ncm ) %>%
  inner_join(
    ncm %>%
      select(co_ncm, co_sh6)
  ) %>%
  inner_join(
    ncm_sh %>%
      select(co_sh6, no_sh6_por, no_sh4_por, no_sh2_por )
  )



##Dados entre 1997 e 2025
data <- fread("exp_completa.csv")
data<- janitor::clean_names(data)

dados_trabalho_exportacao<-
  data %>%
  filter(co_pais %in% c(249,396,873),
         co_ncm %in% codigos_aco_ncm ) %>%
  inner_join(
    ncm %>%
      mutate(co_ncm = as.numeric(co_ncm)) %>%
      select(co_ncm, co_sh6)
  ) %>%
  inner_join(
    ncm_sh %>%
      select(co_sh6, no_sh6_por, no_sh4_por, no_sh2_por )
  )


saveRDS(dados_trabalho_exportacao, "dados_trabalho_exportacao.rds")
glimpse(dados_trabalho_exportacao)


names(dados_trabalho_exportacao)

tibble(codigos_aco = codigos_aco_ncm)  %>%
  writexl::write_xlsx("codigos_aco_ncm.xlsx")

gera_hora_reuniao<- function(){paste( str_pad(sample(8:18,1),2,side = "left", pad = "0"),str_pad(sample(1:59,1),2,side = "left", pad = "0"), sep=":")}

gera_hora_reuniao()

str_pad("9",2,side = "left", pad = "0")

str_pad()


ncm_sh %>%
  distinct(CO_SH2,
           NO_SH2_ING) %>%
  readr::write_csv("sh2.csv")



##O arquivo csv abaixo foi gerado a parti de consulta no deepseek
#https://chat.deepseek.com/a/chat/s/84c2e18b-d7ce-49d8-9b3c-1ccb0d8efa83

produtos_industrializados <- 
  read_csv("produtos_industrializados.csv") %>%
  janitor::clean_names()

saveRDS(produtos_industrializados,"produtos_industrializados.rds")

#Dados de exporação e importação por estado
#https://comexstat.mdic.gov.br/pt/geral

#Dados com filtro para São Paulo (estado) em 2024
library(readxl)
export_import_2024_sp <- 
  read_excel("export_import_2024_sp.xlsx") %>%
  janitor::clean_names()

saveRDS(export_import_2024_sp, "export_import_2024_sp.rds")

#Dados para todo o Brasil em 2024
export_import_2024_brasil <- 
  read_excel("export_import_2024_brasil.xlsx") %>%
  janitor::clean_names()

saveRDS(export_import_2024_brasil,"export_import_2024_brasil.rds")  
