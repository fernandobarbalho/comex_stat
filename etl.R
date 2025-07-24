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

saveRDS(ncm_sh, "ncm_sh.rds")

ncm<- janitor::clean_names(ncm)

exp_2025 <- janitor::clean_names(exp_2025)


ncm_sh$co_sh6 <-  as.factor(ncm_sh$co_sh6)

teste<- "aqui tem pedaço"
str_detect(teste, "\\baço\\b")

exportacao_municipios <- 
  read_delim("EXP_2024_MUN.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
  janitor::clean_names()

saveRDS(exportacao_municipios, "exportacao_municipios.RDS" )

uf_mun <- read_delim("uf_mun.csv", delim = ";", 
                     escape_double = FALSE, locale = locale(encoding = "latin1"), 
                     trim_ws = TRUE)%>%
  janitor::clean_names()

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
